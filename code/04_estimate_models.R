m_sat_DK <- fixest::feols(
  log_app ~ i(treated_factor, Post, ref = "CONTROL") |
    Domicile_named_country + Year,
  data = panel_main,
  panel.id = ~ Domicile_named_country + Year,
  vcov = fixest::DK(1),
  weights = ~ regression_weight
)

m_sat_DK1 <- fixest::feols(
  log_app ~ i(treated_factor, Post, ref = "CONTROL") |
    Domicile_named_country,
  data = panel_main,
  panel.id = ~ Domicile_named_country + Year,
  vcov = fixest::DK(1),
  weights = ~ regression_weight
)

m_sat_DK2 <- fixest::feols(
  log_app ~ i(treated_factor, Post, ref = "CONTROL"),
  data = panel_main,
  panel.id = ~ Domicile_named_country + Year,
  vcov = fixest::DK(1),
  weights = ~ regression_weight
)

m_sat_DK3 <- fixest::feols(
  log_app ~ i(treated_factor, Post, ref = "CONTROL") |
    Domicile_named_country + Year,
  data = panel_main,
  panel.id = ~ Domicile_named_country + Year,
  vcov = fixest::DK(1)
)

m1 <- fixest::feols(
  log_app ~ Treated:Post | Domicile_named_country + Year,
  data = panel_main,
  panel.id = ~ Domicile_named_country + Year,
  vcov = fixest::DK(1),
  weights = ~ regression_weight
)

m2 <- fixest::feols(
  log_app ~ did + c_log_GDPpc | Domicile_named_country + Year,
  data = panel_wb,
  panel.id = ~ Domicile_named_country + Year,
  vcov = fixest::DK(1),
  weights = ~ regression_weight
)

m4 <- fixest::feols(
  log_app ~ did + c_log_GDPpc + c_log_Pop15_64 |
    Domicile_named_country + Year,
  data = panel_wb,
  panel.id = ~ Domicile_named_country + Year,
  vcov = fixest::DK(1),
  weights = ~ regression_weight
)

m5 <- fixest::feols(
  log_app ~ did + c_log_GDPpc + c_log_Pop15_64 + c_YouthUnempl |
    Domicile_named_country + Year,
  data = panel_wb,
  panel.id = ~ Domicile_named_country + Year,
  vcov = fixest::DK(1),
  weights = ~ regression_weight
)

m6 <- fixest::feols(
  log_app ~ did * (c_log_GDPpc + c_log_Pop15_64 + c_YouthUnempl) |
    Domicile_named_country + Year,
  data = panel_wb,
  panel.id = ~ Domicile_named_country + Year,
  vcov = fixest::DK(1),
  weights = ~ regression_weight
)

s1 <- fixest::feols(
  log_app ~ i(treated_factor, Post, ref = "CONTROL") |
    Domicile_named_country + Year,
  data = panel_wb_sat,
  panel.id = ~ Domicile_named_country + Year,
  vcov = fixest::DK(1),
  weights = ~ regression_weight
)

s2 <- fixest::feols(
  log_app ~ i(treated_factor, Post, ref = "CONTROL") + c_log_GDPpc |
    Domicile_named_country + Year,
  data = panel_wb_sat,
  panel.id = ~ Domicile_named_country + Year,
  vcov = fixest::DK(1),
  weights = ~ regression_weight
)

s3 <- fixest::feols(
  log_app ~ i(treated_factor, Post, ref = "CONTROL") +
    c_log_GDPpc + c_log_Pop15_64 |
    Domicile_named_country + Year,
  data = panel_wb_sat,
  panel.id = ~ Domicile_named_country + Year,
  vcov = fixest::DK(1),
  weights = ~ regression_weight
)

s4 <- fixest::feols(
  log_app ~ i(treated_factor, Post, ref = "CONTROL") +
    c_log_GDPpc + c_log_Pop15_64 + c_YouthUnempl |
    Domicile_named_country + Year,
  data = panel_wb_sat,
  panel.id = ~ Domicile_named_country + Year,
  vcov = fixest::DK(1),
  weights = ~ regression_weight
)

m_sat_DK_2 <- fixest::feols(
  log_app ~
    i(treated_factor, Post, ref = "CONTROL") +
    i(treated_factor, Post_COVID, ref = "CONTROL") |
    Domicile_named_country + Year,
  data = panel_covid,
  panel.id = ~ Domicile_named_country + Year,
  vcov = fixest::DK(1),
  weights = ~ regression_weight
)

extract_country_estimates <- function(model, value_name) {
  out <- broom::tidy(model) %>%
    filter(grepl("^treated_factor::.*:Post$", term)) %>%
    transmute(
      Country = sub(
        ":Post$",
        "",
        sub("^treated_factor::", "", term)
      ),
      value = estimate
    )
  names(out)[names(out) == "value"] <- value_name
  out
}

betas_saturated_model <- extract_country_estimates(
  m_sat_DK,
  "Estimate_TWFE"
) %>%
  full_join(
    extract_country_estimates(s4, "Estimate_controls"),
    by = "Country"
  ) %>%
  full_join(
    extract_country_estimates(m_sat_DK2, "Estimate_Pooled"),
    by = "Country"
  ) %>%
  mutate(
    iso2c = countrycode::countrycode(
      Country,
      origin = "country.name",
      destination = "iso2c",
      custom_match = c("Cyprus (European Union)" = "CY"),
      warn = TRUE
    )
  ) %>%
  select(
    iso2c,
    Country,
    Estimate_TWFE,
    Estimate_controls,
    Estimate_Pooled
  ) %>%
  arrange(Country)

stopifnot(
  nrow(betas_saturated_model) == 26L,
  !anyNA(betas_saturated_model)
)

model_exports <- list(
  weighted_twfe = broom::tidy(m_sat_DK),
  weighted_country_fe = broom::tidy(m_sat_DK1),
  weighted_no_fe = broom::tidy(m_sat_DK2),
  weighted_controls = broom::tidy(s4),
  unweighted_twfe = broom::tidy(m_sat_DK3),
  split_periods = broom::tidy(m_sat_DK_2)
)

writexl::write_xlsx(
  list(main = betas_saturated_model),
  file.path(path_derived, "betas_saturated_model.xlsx")
)

writexl::write_xlsx(
  model_exports,
  file.path(path_derived, "model_coefficients.xlsx")
)

saveRDS(
  list(
    m_sat_DK = m_sat_DK,
    m_sat_DK1 = m_sat_DK1,
    m_sat_DK2 = m_sat_DK2,
    m_sat_DK3 = m_sat_DK3,
    m1 = m1,
    m2 = m2,
    m4 = m4,
    m5 = m5,
    m6 = m6,
    s1 = s1,
    s2 = s2,
    s3 = s3,
    s4 = s4,
    m_sat_DK_2 = m_sat_DK_2
  ),
  file.path(path_derived, "estimated_models.rds")
)

