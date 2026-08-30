tidy_with_ci <- function(model) {
  broom::tidy(model) %>%
    mutate(
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error
    )
}

figure_1_data <- tidy_with_ci(m_event) %>%
  filter(grepl("^event_time::", term)) %>%
  transmute(
    event_time = as.integer(sub(
      "^event_time::(-?[0-9]+):.*$",
      "\\1",
      term
    )),
    estimate,
    std.error,
    conf.low,
    conf.high
  ) %>%
  bind_rows(data.frame(
    event_time = 0L,
    estimate = 0,
    std.error = 0,
    conf.low = 0,
    conf.high = 0
  )) %>%
  arrange(event_time)

figure_2_data <- tidy_with_ci(m_sat_event) %>%
  filter(grepl("^event_time::", term)) %>%
  transmute(
    event_time = as.integer(sub(
      "^event_time::(-?[0-9]+):.*$",
      "\\1",
      term
    )),
    country = sub("^.*treated_factor::", "", term),
    estimate,
    std.error,
    conf.low,
    conf.high
  ) %>%
  filter(country != "CONTROL") %>%
  bind_rows(data.frame(
    event_time = 0L,
    country = eu_real,
    estimate = 0,
    std.error = 0,
    conf.low = 0,
    conf.high = 0
  )) %>%
  arrange(country, event_time)

fee_pre <- 9250
fee_post <- 22200
log_fee_change <- log(fee_post) - log(fee_pre)
arithmetic_fee_change <- (fee_post - fee_pre) / fee_pre

figure_3_data <- tidy_with_ci(m_sat_DK) %>%
  filter(grepl("^treated_factor::.*:Post$", term)) %>%
  transmute(
    country = sub(
      ":Post$",
      "",
      sub("^treated_factor::", "", term)
    ),
    beta = estimate,
    eta_log = estimate / log_fee_change,
    eta_log_low = conf.low / log_fee_change,
    eta_log_high = conf.high / log_fee_change,
    eta_arc = (exp(estimate) - 1) / arithmetic_fee_change
  ) %>%
  left_join(
    panel_main %>%
      filter(Domicile_named_country %in% eu_real) %>%
      group_by(Domicile_named_country) %>%
      summarise(
        app_pre = mean(Applicants[Year == 2020]),
        app_post = mean(Applicants[Year == 2021]),
        eta_mechanical =
          ((app_post - app_pre) / app_pre) / arithmetic_fee_change,
        .groups = "drop"
      ) %>%
      rename(country = Domicile_named_country),
    by = "country"
  ) %>%
  arrange(eta_log)

figure_4_models <- list(
  "S1: No controls" = s1,
  "S2: + GDPpc" = s2,
  "S3: + GDPpc + Pop" = s3,
  "S4: + GDPpc + Pop + YouthUnempl" = s4
)

figure_4_data <- bind_rows(lapply(names(figure_4_models), function(name) {
  tidy_with_ci(figure_4_models[[name]]) %>%
    filter(grepl("^treated_factor::.*:Post$", term)) %>%
    transmute(
      model = name,
      country = sub(
        ":Post$",
        "",
        sub("^treated_factor::", "", term)
      ),
      elasticity = estimate / log_fee_change,
      conf.low = conf.low / log_fee_change,
      conf.high = conf.high / log_fee_change
    )
}))

loan_2025_data <- loan_2025$data %>%
  arrange(iso2c)

figure_5_panel_a <- bind_rows(
  data.frame(
    iso2c = loan_2025_data$iso2c,
    log_gdp = loan_2025_data$log_gdp,
    model = "TWFE with controls",
    estimate = loan_2025_data$Estimate_controls
  ),
  data.frame(
    iso2c = loan_2025_data$iso2c,
    log_gdp = loan_2025_data$log_gdp,
    model = "TWFE without controls",
    estimate = loan_2025_data$Estimate_TWFE
  ),
  data.frame(
    iso2c = loan_2025_data$iso2c,
    log_gdp = loan_2025_data$log_gdp,
    model = "Pooled",
    estimate = loan_2025_data$Estimate_Pooled
  )
)

rho_2025_neutral <- loan_2025$rho_neutral
rho_2025_insignificant <- loan_2025$rho_insignificant

elasticity_at <- function(result, rho) {
  result$elasticity_grid[, which.min(abs(result$rho_grid - rho))]
}

figure_5_panel_b <- bind_rows(
  data.frame(
    iso2c = loan_2025_data$iso2c,
    log_gdp = loan_2025_data$log_gdp,
    rate = "rho = 3.0%",
    elasticity = elasticity_at(loan_2025, 0.03)
  ),
  data.frame(
    iso2c = loan_2025_data$iso2c,
    log_gdp = loan_2025_data$log_gdp,
    rate = paste0(
      "gamma = 0; rho = ",
      round(100 * rho_2025_neutral, 1),
      "%"
    ),
    elasticity = elasticity_at(loan_2025, rho_2025_neutral)
  ),
  data.frame(
    iso2c = loan_2025_data$iso2c,
    log_gdp = loan_2025_data$log_gdp,
    rate = "rho = 35.0%",
    elasticity = elasticity_at(loan_2025, 0.35)
  )
)

country_rate_data <- function(result, iso2c) {
  bind_rows(
    data.frame(
      iso2c = iso2c,
      rate = "Insignificance threshold",
      elasticity = elasticity_at(result, result$rho_insignificant)
    ),
    data.frame(
      iso2c = iso2c,
      rate = "Income neutrality",
      elasticity = elasticity_at(result, result$rho_neutral)
    ),
    data.frame(
      iso2c = iso2c,
      rate = "35% discount rate",
      elasticity = elasticity_at(result, 0.35)
    )
  )
}

figure_5_panel_c <- country_rate_data(
  loan_2025,
  loan_2025_data$iso2c
)

figure_6_data <- tidy_with_ci(m_sat_DK_2) %>%
  filter(grepl("^treated_factor::", term)) %>%
  transmute(
    country = sub(
      ":(Post|Post_COVID)$",
      "",
      sub("^treated_factor::", "", term)
    ),
    period = if_else(
      grepl(":Post_COVID$", term),
      "Later 2024-2025",
      "Initial 2021-2023"
    ),
    estimate,
    std.error,
    conf.low,
    conf.high
  )

pooled_robustness_models <- list(
  "Baseline weighted" = models_baseline$weighted,
  "Baseline unweighted" = models_baseline$unweighted,
  "Poor weighted" = models_poor$weighted,
  "Poor unweighted" = models_poor$unweighted,
  "Rich weighted" = models_rich$weighted,
  "Rich unweighted" = models_rich$unweighted,
  "OECD weighted" = models_oecd$weighted,
  "OECD unweighted" = models_oecd$unweighted,
  "Ireland only" = m_ireland_only
)

figure_7_data <- bind_rows(lapply(
  names(pooled_robustness_models),
  function(name) {
    tidy_with_ci(pooled_robustness_models[[name]]) %>%
      filter(term %in% c("EU:Post", "Post:EU")) %>%
      transmute(
        model = name,
        estimate,
        std.error,
        conf.low,
        conf.high,
        group = sub(" .*", "", name)
      )
  }
))

saturated_robustness_models <- list(
  "Baseline W" = sat_baseline$weighted,
  "Baseline U" = sat_baseline$unweighted,
  "Poor W" = sat_poor$weighted,
  "Poor U" = sat_poor$unweighted,
  "Rich W" = sat_rich$weighted,
  "Rich U" = sat_rich$unweighted,
  "OECD W" = sat_oecd$weighted,
  "OECD U" = sat_oecd$unweighted,
  "Ireland" = sat_ireland_only
)

figure_8_data <- bind_rows(lapply(
  names(saturated_robustness_models),
  function(name) {
    tidy_with_ci(saturated_robustness_models[[name]]) %>%
      filter(grepl("^treated_factor::.*:Post$", term)) %>%
      transmute(
        model = name,
        country = sub(
          ":Post$",
          "",
          sub("^treated_factor::", "", term)
        ),
        estimate,
        std.error,
        conf.low,
        conf.high
      )
  }
))

loan_2021_data <- loan_2021$data %>%
  arrange(iso2c)

figure_9_data <- country_rate_data(
  loan_2021,
  loan_2021_data$iso2c
)

profile_indicators <- c(
  Educ_spend_gdp = "SE.XPD.TOTL.GD.ZS",
  GDP_growth = "NY.GDP.MKTP.KD.ZG",
  Gini = "SI.POV.GINI",
  Inflation = "FP.CPI.TOTL.ZG",
  HH_cons_pc = "NE.CON.PRVT.PC.KD",
  GDP_PPP_pc = "NY.GDP.PCAP.PP.KD",
  GNI_PPP_pc = "NY.GNP.PCAP.PP.KD",
  Population = "SP.POP.TOTL",
  Tertiary_attain = "SE.TER.CUAT.BA.ZS",
  Tertiary_enrol = "SE.TER.ENRR",
  Unemp = "SL.UEM.TOTL.ZS",
  Youth_unemp = "SL.UEM.1524.ZS"
)

profile_wdi_file <- file.path(
  path_reference,
  "wdi_profile_2018_2022.csv"
)

if (file.exists(profile_wdi_file)) {
  profile_wdi <- utils::read.csv(profile_wdi_file)
} else {
  profile_wdi <- WDI::WDI(
    country = loan_2025_data$iso2c,
    indicator = profile_indicators,
    start = 2018,
    end = 2022,
    extra = FALSE
  )

  utils::write.csv(
    profile_wdi,
    file.path(path_derived, "wdi_profile_2018_2022.csv"),
    row.names = FALSE
  )
}

stopifnot(all(c(
  "iso2c",
  "year",
  names(profile_indicators)
) %in% names(profile_wdi)))

profile_wdi <- profile_wdi %>%
  mutate(
    log_cons = log(HH_cons_pc),
    log_gdp = log(GDP_PPP_pc),
    log_gni = log(GNI_PPP_pc),
    log_pop = log(Population)
  )

profile_variables <- c(
  "Educ_spend_gdp", "GDP_growth", "Gini", "Inflation",
  "log_cons", "log_gdp", "log_gni", "log_pop",
  "Tertiary_attain", "Tertiary_enrol", "Unemp", "Youth_unemp"
)

profile_labels <- c(
  "Education spending", "GDP growth", "Gini", "Inflation",
  "Consumption per capita", "GDP per capita", "GNI per capita",
  "Population", "Tertiary attainment", "Tertiary enrolment",
  "Unemployment", "Youth unemployment"
)

latest_profile <- profile_wdi %>%
  select(iso2c, year, all_of(profile_variables)) %>%
  pivot_longer(
    cols = all_of(profile_variables),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(is.finite(value)) %>%
  group_by(iso2c, variable) %>%
  slice_max(year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(iso2c, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value)

gdim_file <- file.path(path_reference, "gdim_income.dta")

if (!file.exists(gdim_file)) {
  gdim_file <- file.path(path_derived, "gdim_income.dta")
  utils::download.file(
    paste0(
      "https://datacatalogfiles.worldbank.org/ddh-published/",
      "0066878/DR0095414/IGE_Munoz_VanderWeide_June2025.dta"
    ),
    gdim_file,
    mode = "wb"
  )
}

mobility <- haven::read_dta(gdim_file) %>%
  transmute(
    iso2c = countrycode::countrycode(
      code,
      origin = "iso3c",
      destination = "iso2c",
      warn = FALSE
    ),
    IGE = as.numeric(IGE)
  )

profile_cross_section <- data.frame(
  iso2c = loan_2025_data$iso2c
) %>%
  left_join(latest_profile, by = "iso2c") %>%
  left_join(mobility, by = "iso2c")

stopifnot(
  nrow(profile_cross_section) == nrow(loan_2025_data),
  !anyNA(profile_cross_section$IGE)
)

profile_variables <- c(profile_variables, "IGE")
profile_labels <- c(
  profile_labels,
  "Intergenerational income persistence"
)

rho_profile <- seq(0.03, 0.40, by = 0.005)

profile_elasticities <- sapply(rho_profile, function(rho) {
  pv <- mapply(
    loan_pv,
    loan_2025_data$wage_gbp,
    loan_2025_data$lower,
    loan_2025_data$upper,
    MoreArgs = list(
      rho = rho,
      fixed_rpi = loan_2025$fixed_rpi
    )
  )

  loan_2025_data$Estimate_controls /
    (log(post_brexit_price) - log(pv))
})

figure_10_data <- bind_rows(lapply(
  seq_along(profile_variables),
  function(v) {
    bind_rows(lapply(seq_along(rho_profile), function(j) {
      x <- profile_cross_section[[profile_variables[v]]]
      y <- profile_elasticities[, j]
      ok <- is.finite(x) & is.finite(y)

      if (sum(ok) < 3L) return(NULL)

      model <- stats::lm(scale(y[ok]) ~ scale(x[ok]))
      robust <- lmtest::coeftest(
        model,
        vcov. = sandwich::vcovHC(model, type = "HC1")
      )

      data.frame(
        variable = profile_variables[v],
        label = profile_labels[v],
        rho = rho_profile[j],
        estimate = robust[2, 1],
        std.error = robust[2, 2],
        conf.low = robust[2, 1] - 1.96 * robust[2, 2],
        conf.high = robust[2, 1] + 1.96 * robust[2, 2],
        observations = sum(ok)
      )
    }))
  }
))

rpi_schedule <- data.frame(
  calibration = c("2021-2022", "2021-2022", "2025-2026", "2025-2026"),
  segment = c("Earlier rate", "Later rate", "Earlier rate", "Later rate"),
  rate = c(0.026, 0.015, 0.043, 0.032),
  months = c(5L, 7L, 5L, 7L)
) %>%
  group_by(calibration) %>%
  mutate(weighted_rpi = weighted.mean(rate, months)) %>%
  ungroup()

writexl::write_xlsx(
  list(
    event_study = figure_1_data,
    saturated_event = figure_2_data,
    decomposition = figure_3_data,
    controlled_elasticities = figure_4_data,
    raw_income = figure_5_panel_a,
    income_rates = figure_5_panel_b,
    country_rates_2025 = figure_5_panel_c,
    split_period = figure_6_data,
    pooled_robustness = figure_7_data,
    saturated_robustness = figure_8_data,
    country_rates_2021 = figure_9_data,
    discount_correlations = figure_10_data,
    rpi_schedule = rpi_schedule
  ),
  file.path(path_derived, "figure_data.xlsx")
)
