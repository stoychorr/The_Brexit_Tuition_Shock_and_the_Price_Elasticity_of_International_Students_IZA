significance_code <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.001) return("***")
  if (p_value < 0.01) return("**")
  if (p_value < 0.05) return("*")
  if (p_value < 0.10) return("+")
  ""
}

coefficient_cell <- function(estimate, std_error, p_value, digits = 3L) {
  paste0(
    formatC(estimate, format = "f", digits = digits),
    significance_code(p_value),
    "<br>(",
    formatC(std_error, format = "f", digits = digits),
    ")"
  )
}

model_cell <- function(model, candidates, digits = 3L) {
  tidy_model <- broom::tidy(model)
  selected <- tidy_model[tidy_model$term %in% candidates, , drop = FALSE]

  if (nrow(selected) == 0L) return("")
  if (nrow(selected) != 1L) {
    stop("A table term matched more than one model coefficient.")
  }

  coefficient_cell(
    selected$estimate,
    selected$std.error,
    selected$p.value,
    digits
  )
}

country_cells <- function(model, suffix = "Post", digits = 3L) {
  tidy_model <- broom::tidy(model)
  pattern <- paste0("^treated_factor::.*:", suffix, "$")
  selected <- tidy_model[grepl(pattern, tidy_model$term), , drop = FALSE]

  countries <- sub(
    paste0(":", suffix, "$"),
    "",
    sub("^treated_factor::", "", selected$term)
  )
  countries <- clean_country_for_output(countries)

  cells <- vapply(
    seq_len(nrow(selected)),
    function(i) coefficient_cell(
      selected$estimate[i],
      selected$std.error[i],
      selected$p.value[i],
      digits
    ),
    character(1)
  )

  stats::setNames(cells, countries)
}

fit_value <- function(model, type) {
  as.numeric(fixest::r2(model, type = type))
}

format_stat <- function(x, digits = 3L) {
  formatC(x, format = "f", digits = digits)
}

descriptive_row <- function(data, variable, digits, label = variable) {
  x <- as.numeric(data[[variable]])
  x <- x[is.finite(x)]

  values <- c(
    length(x),
    mean(x),
    stats::sd(x),
    stats::median(x),
    min(x),
    max(x)
  )

  formatted <- mapply(
    function(value, places) {
      formatC(value, format = "f", digits = places, big.mark = "")
    },
    values,
    c(0L, digits)
  )

  data.frame(
    Variable = label,
    n = formatted[1],
    mean = formatted[2],
    sd = formatted[3],
    median = formatted[4],
    min = formatted[5],
    max = formatted[6],
    check.names = FALSE
  )
}

section_row <- function(label) {
  data.frame(
    Variable = paste0("<strong>", label, "</strong>"),
    n = "",
    mean = "",
    sd = "",
    median = "",
    min = "",
    max = "",
    check.names = FALSE
  )
}

table_1 <- bind_rows(
  section_row("Donor matching sample (applicants &ge; 100)*"),
  descriptive_row(
    control_matching_data,
    "Applicants",
    c(2L, 2L, 0L, 0L, 0L)
  ),
  section_row("Baseline DiD sample**"),
  descriptive_row(
    panel_main,
    "Applicants",
    c(2L, 2L, 1L, 1L, 1L)
  ),
  section_row("Synthetic DiD sample**"),
  descriptive_row(
    sdid_long,
    "Applicants",
    c(2L, 2L, 1L, 2L, 1L)
  ),
  section_row("World Bank regression sample**"),
  descriptive_row(panel_wb, "Applicants", c(2L, 2L, 2L, 2L, 2L)),
  descriptive_row(panel_wb, "log_app", rep(2L, 5L)),
  descriptive_row(panel_wb, "EU", rep(2L, 5L)),
  descriptive_row(panel_wb, "Post", rep(2L, 5L)),
  descriptive_row(panel_wb, "did", rep(2L, 5L)),
  descriptive_row(panel_wb, "event_time", rep(2L, 5L)),
  descriptive_row(panel_wb, "regression_weight", rep(2L, 5L)),
  descriptive_row(panel_wb, "GDPpc", rep(0L, 5L)),
  descriptive_row(panel_wb, "Pop15_64", rep(0L, 5L)),
  descriptive_row(panel_wb, "YouthUnempl", rep(2L, 5L)),
  descriptive_row(panel_wb, "EduSpendGDP", rep(2L, 5L)),
  descriptive_row(panel_wb, "c_log_GDPpc", rep(2L, 5L)),
  descriptive_row(panel_wb, "c_log_Pop15_64", rep(2L, 5L)),
  descriptive_row(panel_wb, "c_YouthUnempl", rep(2L, 5L))
)

format_weight <- function(x) {
  ifelse(x < 0.005, "<0.00", formatC(x, format = "f", digits = 2L))
}

table_2 <- weights %>%
  transmute(
    `Donor country` = donor_country,
    `Global weight` = format_weight(global_weight),
    `Global weight x 26` = format_weight(regression_weight)
  )

table_3_models <- list(m1, m2, m4, m5, m6)
table_3_names <- c("I", "II", "III", "IV", "V")

table_3 <- data.frame(
  Variable = c(
    "ATT",
    "c_log(GDPpc)",
    "c_log(Pop15_64)",
    "c_YU",
    "ATT x c_log(GDPpc)",
    "ATT x c_log(Pop15_64)",
    "ATT x c_YU",
    "Obs.",
    "R2 Adj",
    "R2 Within"
  ),
  I = rep("", 10L),
  II = rep("", 10L),
  III = rep("", 10L),
  IV = rep("", 10L),
  V = rep("", 10L),
  check.names = FALSE
)

table_3[1, table_3_names] <- c(
  model_cell(m1, c("Treated:Post", "Post:Treated")),
  model_cell(m2, "did"),
  model_cell(m4, "did"),
  model_cell(m5, "did"),
  model_cell(m6, "did")
)

table_3[2, table_3_names] <- c(
  "",
  model_cell(m2, "c_log_GDPpc"),
  model_cell(m4, "c_log_GDPpc"),
  model_cell(m5, "c_log_GDPpc"),
  model_cell(m6, "c_log_GDPpc")
)

table_3[3, table_3_names] <- c(
  "", "",
  model_cell(m4, "c_log_Pop15_64"),
  model_cell(m5, "c_log_Pop15_64"),
  model_cell(m6, "c_log_Pop15_64")
)

table_3[4, table_3_names] <- c(
  "", "", "",
  model_cell(m5, "c_YouthUnempl"),
  model_cell(m6, "c_YouthUnempl")
)

table_3[5, table_3_names] <- c(
  "", "", "", "",
  model_cell(m6, c("did:c_log_GDPpc", "c_log_GDPpc:did"))
)

table_3[6, table_3_names] <- c(
  "", "", "", "",
  model_cell(m6, c("did:c_log_Pop15_64", "c_log_Pop15_64:did"))
)

table_3[7, table_3_names] <- c(
  "", "", "", "",
  model_cell(m6, c("did:c_YouthUnempl", "c_YouthUnempl:did"))
)

table_3[8, table_3_names] <- vapply(
  table_3_models,
  function(x) as.character(stats::nobs(x)),
  character(1)
)
table_3[9, table_3_names] <- vapply(
  table_3_models,
  function(x) format_stat(fit_value(x, "ar2")),
  character(1)
)
table_3[10, table_3_names] <- vapply(
  table_3_models,
  function(x) format_stat(fit_value(x, "wr2")),
  character(1)
)

table_4 <- data.frame(
  Variable = c(
    "Control-group composition",
    "EU group",
    "Observations"
  ),
  Poor = c(
    paste(synth_poor, collapse = ", "),
    length(eu_real),
    nrow(data_poor)
  ),
  Rich = c(
    paste(synth_rich, collapse = ", "),
    length(eu_real),
    nrow(data_rich)
  ),
  OECD = c(
    paste(oecd, collapse = ", "),
    length(eu_real),
    nrow(data_oecd)
  ),
  check.names = FALSE
)

table_5 <- table_5_data %>%
  transmute(
    Country = Country,
    `Saturated TWFE` = formatC(Saturated_TWFE, format = "f", digits = 2L),
    `Synthetic DiD` = formatC(Synthetic_DiD, format = "f", digits = 2L),
    `Absolute difference` = formatC(
      Absolute_difference,
      format = "f",
      digits = 2L
    )
  ) %>%
  bind_rows(data.frame(
    Country = "N",
    `Saturated TWFE` = "520",
    `Synthetic DiD` = "950 per regression",
    `Absolute difference` = "",
    check.names = FALSE
  ))

country_order <- clean_country_for_output(eu_real)

a1_weighted_twfe <- country_cells(m_sat_DK)
a1_weighted_country <- country_cells(m_sat_DK1)
a1_weighted_pooled <- country_cells(m_sat_DK2)
a1_unweighted_twfe <- country_cells(m_sat_DK3)

table_a1_panel_a <- data.frame(
  Variable = c("Intercept", country_order),
  `Weighted TWFE` = c("", unname(a1_weighted_twfe[country_order])),
  `Weighted country FE` = c("", unname(a1_weighted_country[country_order])),
  `Weighted no FE` = c(
    model_cell(m_sat_DK2, "(Intercept)"),
    unname(a1_weighted_pooled[country_order])
  ),
  `Unweighted TWFE` = c("", unname(a1_unweighted_twfe[country_order])),
  check.names = FALSE
)

table_a1_panel_a <- bind_rows(
  table_a1_panel_a,
  data.frame(
    Variable = c(
      "Observations", "Weights", "Country fixed effects",
      "Year fixed effects", "Adjusted R2", "Within R2"
    ),
    `Weighted TWFE` = c(
      stats::nobs(m_sat_DK), "Yes", "Yes", "Yes",
      format_stat(fit_value(m_sat_DK, "ar2"), 2L),
      format_stat(fit_value(m_sat_DK, "wr2"), 2L)
    ),
    `Weighted country FE` = c(
      stats::nobs(m_sat_DK1), "Yes", "Yes", "No",
      format_stat(fit_value(m_sat_DK1, "ar2"), 2L),
      format_stat(fit_value(m_sat_DK1, "wr2"), 2L)
    ),
    `Weighted no FE` = c(
      stats::nobs(m_sat_DK2), "Yes", "No", "No",
      format_stat(fit_value(m_sat_DK2, "ar2"), 2L), ""
    ),
    `Unweighted TWFE` = c(
      stats::nobs(m_sat_DK3), "No", "Yes", "Yes",
      format_stat(fit_value(m_sat_DK3, "ar2"), 2L),
      format_stat(fit_value(m_sat_DK3, "wr2"), 2L)
    ),
    check.names = FALSE
  )
)

a1_s1 <- country_cells(s1)
a1_s2 <- country_cells(s2)
a1_s3 <- country_cells(s3)
a1_s4 <- country_cells(s4)

table_a1_panel_b <- data.frame(
  Variable = country_order,
  `S1 Saturated DiD` = unname(a1_s1[country_order]),
  `S2 + GDPpc` = unname(a1_s2[country_order]),
  `S3 + GDPpc + Pop` = unname(a1_s3[country_order]),
  `S4 + GDPpc + Pop + YouthUnempl` = unname(a1_s4[country_order]),
  check.names = FALSE
)

table_a1_panel_b <- bind_rows(
  table_a1_panel_b,
  data.frame(
    Variable = c(
      "c_log_GDPpc", "c_log_Pop15_64", "c_YouthUnempl",
      "Num.Obs.", "R2 Within", "R2 Within Adj.",
      "Country FE", "Year FE", "Weights"
    ),
    `S1 Saturated DiD` = c(
      "", "", "", stats::nobs(s1),
      format_stat(fit_value(s1, "wr2")),
      format_stat(fit_value(s1, "war2")),
      "YES", "YES", "YES"
    ),
    `S2 + GDPpc` = c(
      model_cell(s2, "c_log_GDPpc"), "", "", stats::nobs(s2),
      format_stat(fit_value(s2, "wr2")),
      format_stat(fit_value(s2, "war2")),
      "YES", "YES", "YES"
    ),
    `S3 + GDPpc + Pop` = c(
      model_cell(s3, "c_log_GDPpc"),
      model_cell(s3, "c_log_Pop15_64"),
      "", stats::nobs(s3),
      format_stat(fit_value(s3, "wr2")),
      format_stat(fit_value(s3, "war2")),
      "YES", "YES", "YES"
    ),
    `S4 + GDPpc + Pop + YouthUnempl` = c(
      model_cell(s4, "c_log_GDPpc"),
      model_cell(s4, "c_log_Pop15_64"),
      model_cell(s4, "c_YouthUnempl"),
      stats::nobs(s4),
      format_stat(fit_value(s4, "wr2")),
      format_stat(fit_value(s4, "war2")),
      "YES", "YES", "YES"
    ),
    check.names = FALSE
  )
)

split_initial <- country_cells(m_sat_DK_2, "Post")
split_later <- country_cells(m_sat_DK_2, "Post_COVID")

first_half <- country_order[1:13]
second_half <- country_order[14:26]

table_a2 <- data.frame(
  Country_1 = first_half,
  Post_1 = unname(split_initial[first_half]),
  Country_2 = second_half,
  Post_2 = unname(split_initial[second_half]),
  Country_3 = first_half,
  Post_3 = unname(split_later[first_half]),
  Country_4 = second_half,
  Post_4 = unname(split_later[second_half]),
  check.names = FALSE
)

table_a2_column_names <- c(
  "Country", "Post", "Country", "Post",
  "Country", "Post 2023", "Country", "Post 2023"
)

table_a2 <- bind_rows(
  table_a2,
  data.frame(
      Country_1 = c(
        "Observations", "Weights", "Country FE", "Time FE",
        "Adj. R2", "Within R2"
      ),
      Post_1 = c(
        stats::nobs(m_sat_DK_2), "YES", "YES", "YES",
        format_stat(fit_value(m_sat_DK_2, "ar2"), 2L),
        format_stat(fit_value(m_sat_DK_2, "wr2"), 2L)
      ),
      Country_2 = rep("", 6L),
      Post_2 = rep("", 6L),
      Country_3 = rep("", 6L),
      Post_3 = rep("", 6L),
      Country_4 = rep("", 6L),
      Post_4 = rep("", 6L),
      check.names = FALSE
  )
)

table_a3 <- threshold_table_2021 %>%
  transmute(
    Countries,
    `Lower threshold` = paste0("£", formatC(lower, format = "f", digits = 0L)),
    `Upper threshold` = paste0("£", formatC(upper, format = "f", digits = 0L))
  )

table_a4 <- threshold_table_2025 %>%
  transmute(
    Countries,
    `Lower threshold` = paste0("£", formatC(lower, format = "f", digits = 0L)),
    `Upper threshold` = paste0("£", formatC(upper, format = "f", digits = 0L))
  )

table_objects <- list(
  table_1 = table_1,
  table_2 = table_2,
  table_3 = table_3,
  table_4 = table_4,
  table_5 = table_5,
  table_a1_panel_a = table_a1_panel_a,
  table_a1_panel_b = table_a1_panel_b,
  table_a2 = table_a2,
  table_a3 = table_a3,
  table_a4 = table_a4
)

invisible(lapply(names(table_objects), function(name) {
  write_csv_plain(table_objects[[name]], paste0(name, ".csv"))
}))
