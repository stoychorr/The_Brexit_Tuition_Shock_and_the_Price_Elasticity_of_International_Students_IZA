control_input <- file.path(path_raw, "ucas_control_group_2006_2024.xlsx")

control_raw <- readxl::read_excel(control_input, sheet = "full_data")

required_control_columns <- c(
  "Domicile_named_country", "Year", "Applicants"
)
stopifnot(all(required_control_columns %in% names(control_raw)))

control_filtered <- control_raw %>%
  filter(!Domicile_named_country %in% non_countries)

stopifnot(
  !anyDuplicated(control_filtered[c("Domicile_named_country", "Year")])
)

control_years <- 2014:2020

control_matching_data <- control_filtered %>%
  filter(Year %in% control_years) %>%
  group_by(Domicile_named_country) %>%
  filter(
    n_distinct(Year) == length(control_years),
    all(control_years %in% Year),
    all(is.finite(Applicants)),
    all(Applicants >= 100)
  ) %>%
  ungroup()

trajectory <- control_matching_data %>%
  select(Domicile_named_country, Year, Applicants) %>%
  pivot_wider(names_from = Year, values_from = Applicants) %>%
  as.data.frame()

rownames(trajectory) <- trajectory$Domicile_named_country
trajectory <- trajectory[, as.character(control_years), drop = FALSE]

log_trajectory <- log(as.matrix(trajectory))
valid_rows <- apply(log_trajectory, 1, function(x) all(is.finite(x)))
log_trajectory <- log_trajectory[valid_rows, , drop = FALSE]

row_sd <- apply(log_trajectory, 1, stats::sd)
log_trajectory <- log_trajectory[
  is.finite(row_sd) & row_sd > 0,
  ,
  drop = FALSE
]

stopifnot(
  identical(colnames(log_trajectory), as.character(control_years)),
  all(eu_real %in% rownames(log_trajectory)),
  nrow(control_matching_data) == 546L
)

scaled_trajectory <- t(scale(t(log_trajectory)))
distance_matrix <- as.matrix(dist(scaled_trajectory, method = "euclidean"))

treated_rows <- rownames(scaled_trajectory) %in% eu_real
treated_donor_distance <- distance_matrix[
  treated_rows,
  !treated_rows,
  drop = FALSE
]

n_donors <- 3L

donor_selection <- bind_rows(lapply(
  rownames(treated_donor_distance),
  function(country) {
    nearest <- sort(treated_donor_distance[country, ])[seq_len(n_donors)]
    data.frame(
      EU_country = country,
      donor = names(nearest),
      donor_rank = seq_len(n_donors),
      distance = as.numeric(nearest)
    )
  }
))

solve_weights <- function(y, Z) {
  n <- ncol(Z)
  fit <- Rsolnp::solnp(
    pars = rep(1 / n, n),
    fun = function(w) sum((y - Z %*% w)^2),
    eqfun = function(w) sum(w),
    eqB = 1,
    LB = rep(0, n),
    UB = rep(1, n),
    control = list(trace = 0)
  )

  if (!isTRUE(fit$convergence == 0)) {
    stop("Rsolnp failed to converge for a treated country.")
  }

  w <- as.numeric(fit$pars)
  w[abs(w) < 1e-12] <- 0
  w <- w / sum(w)

  stopifnot(
    all(w >= -1e-8),
    all(w <= 1 + 1e-8),
    abs(sum(w) - 1) < 1e-7
  )

  w
}

country_fits <- lapply(eu_real, function(country) {
  donors <- donor_selection$donor[
    donor_selection$EU_country == country
  ]

  y <- as.numeric(log_trajectory[country, ])
  Z <- t(log_trajectory[donors, , drop = FALSE])
  w <- solve_weights(y, Z)
  fitted <- as.numeric(Z %*% w)

  list(
    weights = data.frame(
      EU_country = country,
      donor = donors,
      synth_weight = w
    ),
    rmspe = sqrt(mean((y - fitted)^2))
  )
})

weight_results <- bind_rows(lapply(
  country_fits,
  function(x) x$weights
))

rmspe_results <- data.frame(
  EU_country = eu_real,
  rmspe = vapply(country_fits, function(x) x$rmspe, numeric(1))
)

donor_pool <- donor_selection %>%
  left_join(weight_results, by = c("EU_country", "donor")) %>%
  left_join(rmspe_results, by = "EU_country") %>%
  arrange(EU_country, donor_rank)

J <- length(eu_real)

weights <- donor_pool %>%
  group_by(donor) %>%
  summarise(global_weight = sum(synth_weight) / J, .groups = "drop") %>%
  mutate(
    normalized_weight = global_weight / sum(global_weight),
    regression_weight = J * normalized_weight
  ) %>%
  rename(donor_country = donor) %>%
  arrange(desc(normalized_weight))

stopifnot(
  nrow(donor_pool) == 78L,
  n_distinct(donor_pool$EU_country) == 26L,
  abs(sum(weights$normalized_weight) - 1) < 1e-10,
  abs(sum(weights$regression_weight) - J) < 1e-8
)

expected_weights <- readxl::read_excel(
  file.path(path_reference, "control_group_output_expected.xlsx"),
  sheet = "weights"
)

weight_audit <- weights %>%
  select(donor_country, reproduced_weight = regression_weight) %>%
  full_join(
    expected_weights %>%
      select(donor_country, expected_weight = regression_weight),
    by = "donor_country"
  ) %>%
  mutate(difference = reproduced_weight - expected_weight)

stopifnot(
  !anyNA(weight_audit),
  max(abs(weight_audit$difference)) < 1e-4
)

writexl::write_xlsx(
  list(donor_pool = donor_pool, weights = weights),
  file.path(path_derived, "control_group_output.xlsx")
)

write_csv_plain(weight_audit, "audit_control_group_weights.csv")
