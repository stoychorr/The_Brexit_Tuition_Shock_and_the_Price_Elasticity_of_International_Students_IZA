analysis_years <- 2016:2025

sdid_long <- apps_clean %>%
  filter(
    Year %in% analysis_years,
    !is.na(Domicile_named_country),
    !is.na(EU),
    !is.na(Applicants),
    is.finite(log_app)
  ) %>%
  select(
    Domicile_named_country,
    Year,
    Applicants,
    log_app,
    EU
  ) %>%
  group_by(Domicile_named_country) %>%
  filter(
    n() == length(analysis_years),
    n_distinct(Year) == length(analysis_years),
    n_distinct(EU) == 1L,
    all(Applicants >= 25)
  ) %>%
  ungroup() %>%
  mutate(treated = as.integer(EU == 1L & Year >= 2021)) %>%
  arrange(Domicile_named_country, Year)

stopifnot(
  nrow(sdid_long) == 1200L,
  n_distinct(sdid_long$Domicile_named_country) == 120L,
  n_distinct(sdid_long$Domicile_named_country[sdid_long$EU == 1L]) == 26L,
  n_distinct(sdid_long$Domicile_named_country[sdid_long$EU == 0L]) == 94L,
  !anyDuplicated(sdid_long[c("Domicile_named_country", "Year")]),
  all(sdid_long$Applicants >= 25),
  all(is.finite(sdid_long$log_app))
)

sdid_setup <- synthdid::panel.matrices(
  as.data.frame(sdid_long),
  unit = "Domicile_named_country",
  time = "Year",
  outcome = "log_app",
  treatment = "treated"
)

sdid_pooled <- synthdid::synthdid_estimate(
  sdid_setup$Y,
  sdid_setup$N0,
  sdid_setup$T0
)

sdid_pooled_se <- as.numeric(sqrt(
  stats::vcov(sdid_pooled, method = "jackknife")
))

sdid_country_results <- bind_rows(lapply(sort(eu_real), function(country) {
  panel_country <- sdid_long %>%
    filter(Domicile_named_country == country | EU == 0L) %>%
    mutate(
      treated_country = as.integer(
        Domicile_named_country == country & Year >= 2021
      )
    )

  setup_country <- synthdid::panel.matrices(
    as.data.frame(panel_country),
    unit = "Domicile_named_country",
    time = "Year",
    outcome = "log_app",
    treatment = "treated_country"
  )

  estimate_country <- synthdid::synthdid_estimate(
    setup_country$Y,
    setup_country$N0,
    setup_country$T0
  )

  se_country <- as.numeric(sqrt(
    stats::vcov(estimate_country, method = "placebo")
  ))

  data.frame(
    Country = country,
    Synthetic_DiD = as.numeric(estimate_country),
    Placebo_SE = se_country,
    Control_units = setup_country$N0,
    Treated_units = nrow(setup_country$Y) - setup_country$N0,
    Pre_periods = setup_country$T0,
    Post_periods = ncol(setup_country$Y) - setup_country$T0
  )
})) %>%
  mutate(
    z_value = Synthetic_DiD / Placebo_SE,
    p_value = 2 * stats::pnorm(-abs(z_value)),
    conf_low = Synthetic_DiD - 1.96 * Placebo_SE,
    conf_high = Synthetic_DiD + 1.96 * Placebo_SE
  )

table_5_data <- betas_saturated_model %>%
  select(Country, Saturated_TWFE = Estimate_TWFE) %>%
  inner_join(sdid_country_results, by = "Country") %>%
  mutate(Absolute_difference = abs(Saturated_TWFE - Synthetic_DiD)) %>%
  arrange(Saturated_TWFE)

stopifnot(
  nrow(table_5_data) == 26L,
  all(table_5_data$Control_units == 94L),
  all(table_5_data$Treated_units == 1L)
)

writexl::write_xlsx(
  list(
    pooled = data.frame(
      estimate = as.numeric(sdid_pooled),
      standard_error = sdid_pooled_se,
      N0 = sdid_setup$N0,
      T0 = sdid_setup$T0
    ),
    country_results = sdid_country_results,
    table_5 = table_5_data
  ),
  file.path(path_derived, "synthetic_did_results.xlsx")
)

