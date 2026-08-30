m_event <- fixest::feols(
  log_app ~ i(event_time, EU, ref = 0) |
    Domicile_named_country + Year,
  data = panel_main,
  panel.id = ~ Domicile_named_country + Year,
  vcov = ~ Domicile_named_country,
  weights = ~ regression_weight
)

m_sat_event <- fixest::feols(
  log_app ~ i(event_time, treated_factor, ref = 0, ref2 = "CONTROL") |
    Domicile_named_country + Year,
  data = panel_main,
  panel.id = ~ Domicile_named_country + Year,
  vcov = ~ Domicile_named_country,
  weights = ~ regression_weight
)

make_distance_weights <- function(data) {
  pre_years <- 2016:2020

  eu_pre <- data %>%
    filter(EU == 1L, Year %in% pre_years) %>%
    group_by(Year) %>%
    summarise(eu_mean_log_app = mean(log_app), .groups = "drop")

  stopifnot(identical(sort(eu_pre$Year), pre_years))

  control_distances <- data %>%
    filter(EU == 0L, Year %in% pre_years) %>%
    select(Domicile_named_country, Year, log_app) %>%
    left_join(eu_pre, by = "Year") %>%
    group_by(Domicile_named_country) %>%
    filter(
      n_distinct(Year) == length(pre_years),
      all(pre_years %in% Year)
    ) %>%
    summarise(
      distance = sqrt(mean((log_app - eu_mean_log_app)^2)),
      .groups = "drop"
    ) %>%
    mutate(
      donor_weight = 1 / (distance + 0.001),
      donor_weight = donor_weight / mean(donor_weight)
    )

  stopifnot(
    nrow(control_distances) ==
      n_distinct(data$Domicile_named_country[data$EU == 0L])
  )

  output <- data %>%
    left_join(control_distances, by = "Domicile_named_country") %>%
    mutate(
      robustness_weight = if_else(EU == 1L, 1, donor_weight)
    )

  stopifnot(
    !anyNA(output$robustness_weight),
    all(is.finite(output$robustness_weight))
  )

  output
}

data_baseline_w <- make_distance_weights(data_baseline)
data_poor_w <- make_distance_weights(data_poor)
data_rich_w <- make_distance_weights(data_rich)
data_oecd_w <- make_distance_weights(data_oecd)

run_weighted_unweighted <- function(data) {
  weighted <- fixest::feols(
    log_app ~ EU:Post | Domicile_named_country + Year,
    data = data,
    weights = ~ robustness_weight,
    panel.id = ~ Domicile_named_country + Year,
    vcov = fixest::DK(1)
  )

  unweighted <- fixest::feols(
    log_app ~ EU:Post | Domicile_named_country + Year,
    data = data,
    panel.id = ~ Domicile_named_country + Year,
    vcov = fixest::DK(1)
  )

  list(weighted = weighted, unweighted = unweighted)
}

models_baseline <- run_weighted_unweighted(data_baseline_w)
models_poor <- run_weighted_unweighted(data_poor_w)
models_rich <- run_weighted_unweighted(data_rich_w)
models_oecd <- run_weighted_unweighted(data_oecd_w)

panel_ireland_pooled <- apps_clean %>%
  filter(Domicile_named_country %in% c(eu_real, "Ireland")) %>%
  mutate(
    EU = as.integer(Domicile_named_country %in% eu_real),
    Post = as.integer(Year >= 2021)
  )

m_ireland_only <- fixest::feols(
  log_app ~ EU:Post | Domicile_named_country + Year,
  data = panel_ireland_pooled,
  panel.id = ~ Domicile_named_country + Year,
  vcov = fixest::DK(1)
)

prepare_saturated_data <- function(data) {
  data %>%
    mutate(
      treated_factor = factor(if_else(
        Domicile_named_country %in% eu_real,
        Domicile_named_country,
        "CONTROL"
      ))
    )
}

run_saturated_models <- function(data) {
  weighted <- fixest::feols(
    log_app ~ i(treated_factor, Post, ref = "CONTROL") |
      Domicile_named_country + Year,
    data = data,
    weights = ~ robustness_weight,
    panel.id = ~ Domicile_named_country + Year,
    vcov = fixest::DK(1)
  )

  unweighted <- fixest::feols(
    log_app ~ i(treated_factor, Post, ref = "CONTROL") |
      Domicile_named_country + Year,
    data = data,
    panel.id = ~ Domicile_named_country + Year,
    vcov = fixest::DK(1)
  )

  list(weighted = weighted, unweighted = unweighted)
}

sat_baseline <- run_saturated_models(
  prepare_saturated_data(data_baseline_w)
)
sat_poor <- run_saturated_models(prepare_saturated_data(data_poor_w))
sat_rich <- run_saturated_models(prepare_saturated_data(data_rich_w))
sat_oecd <- run_saturated_models(prepare_saturated_data(data_oecd_w))

data_ireland_sat <- panel_ireland_pooled %>%
  mutate(
    treated_factor = factor(if_else(
      Domicile_named_country %in% eu_real,
      Domicile_named_country,
      "IRELAND"
    ))
  )

sat_ireland_only <- fixest::feols(
  log_app ~ i(treated_factor, Post, ref = "IRELAND") |
    Domicile_named_country + Year,
  data = data_ireland_sat,
  panel.id = ~ Domicile_named_country + Year,
  vcov = fixest::DK(1)
)

saveRDS(
  list(
    m_event = m_event,
    m_sat_event = m_sat_event,
    models_baseline = models_baseline,
    models_poor = models_poor,
    models_rich = models_rich,
    models_oecd = models_oecd,
    m_ireland_only = m_ireland_only,
    sat_baseline = sat_baseline,
    sat_poor = sat_poor,
    sat_rich = sat_rich,
    sat_oecd = sat_oecd,
    sat_ireland_only = sat_ireland_only
  ),
  file.path(path_derived, "figure_models.rds")
)
