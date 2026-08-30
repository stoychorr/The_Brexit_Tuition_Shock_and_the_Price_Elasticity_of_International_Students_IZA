threshold_2021 <- data.frame(
  Country = c(
    "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia",
    "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
    "Hungary", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
    "Netherlands", "Poland", "Portugal", "Romania", "Slovakia",
    "Slovenia", "Spain", "Sweden"
  ),
  iso2c = c(
    "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR",
    "DE", "GR", "HU", "IT", "LV", "LT", "LU", "MT", "NL", "PL",
    "PT", "RO", "SK", "SI", "ES", "SE"
  ),
  lower = c(
    27295, 27295, 10920, 16380, 21840, 16380, 32755, 21840, 27295,
    27295, 27295, 21840, 16380, 21840, 16380, 16380, 27295, 21840,
    27295, 16380, 21840, 10920, 16380, 21840, 21840, 27295
  ),
  upper = c(
    49130, 49130, 19655, 29480, 39305, 29480, 58960, 39305, 49130,
    49130, 49130, 39305, 29480, 39305, 29480, 29480, 49130, 39305,
    49130, 29480, 39305, 19655, 29480, 39305, 39305, 49130
  )
)

threshold_2025 <- data.frame(
  Country = c(
    "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia",
    "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
    "Hungary", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
    "Netherlands", "Poland", "Portugal", "Romania", "Slovakia",
    "Slovenia", "Spain", "Sweden"
  ),
  iso2c = threshold_2021$iso2c,
  lower = c(
    28470, 28470, 11390, 17085, 22780, 17085, 28470, 22780, 28470,
    22780, 28470, 17085, 17085, 22780, 17085, 17085, 28470, 22780,
    28470, 17085, 17085, 11390, 17085, 22780, 22780, 28470
  ),
  upper = c(
    51245, 51245, 20500, 30750, 41000, 30750, 51245, 41000, 51245,
    41000, 51245, 30750, 30750, 41000, 30750, 30750, 51245, 41000,
    51245, 30750, 30750, 20500, 30750, 41000, 41000, 51245
  )
)

loan_amount <- 9250
post_brexit_price <- 22200
repayment_rate <- 0.09
wage_growth <- 0.02
interest_spread <- 0.03

loan_pv <- function(wage, lower, upper, rho, fixed_rpi) {
  balance <- loan_amount
  present_value <- 0

  for (year in 1:30) {
    earnings <- wage * (1 + wage_growth)^(year - 1)

    if (earnings <= lower) {
      loading <- 0
    } else if (earnings < upper) {
      loading <- (earnings - lower) / (upper - lower)
    } else {
      loading <- 1
    }

    interest_rate <- fixed_rpi + interest_spread * loading
    balance_after_interest <- balance * (1 + interest_rate)
    scheduled_repayment <- repayment_rate * max(0, earnings - lower)
    actual_repayment <- min(scheduled_repayment, balance_after_interest)

    present_value <- present_value + actual_repayment / (1 + rho)^year
    balance <- balance_after_interest - actual_repayment

    if (balance <= 0) break
  }

  present_value
}

gdp_snapshot <- utils::read.csv(
  file.path(path_reference, "wdi_gdp_2018_2022.csv")
) %>%
  filter(!is.na(GDP_PPP_pc)) %>%
  group_by(iso2c) %>%
  slice_max(year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(iso2c, GDP_PPP_pc)

betas_for_loan <- readxl::read_excel(
  file.path(path_derived, "betas_saturated_model.xlsx"),
  sheet = "main"
)

prepare_loan_data <- function(thresholds, wage_file, eur_gbp, grow_first) {
  wages <- readxl::read_excel(wage_file, sheet = "main_df")
  betas <- betas_for_loan

  wages$Country <- trimws(sub("\\s*\\([^()]*\\)\\s*$", "", wages$Country))
  betas$Country <- trimws(sub("\\s*\\([^()]*\\)\\s*$", "", betas$Country))
  betas$Country[betas$Country == "Czech Republic"] <- "Czechia"

  thresholds %>%
    inner_join(wages %>% select(Country, Wage_EURO), by = "Country") %>%
    inner_join(
      betas %>% select(
        Country,
        Estimate_TWFE,
        Estimate_controls,
        Estimate_Pooled
      ),
      by = "Country"
    ) %>%
    inner_join(gdp_snapshot, by = "iso2c") %>%
    mutate(
      wage_gbp_observed = Wage_EURO * eur_gbp,
      wage_gbp = wage_gbp_observed * (1 + wage_growth)^grow_first,
      log_gdp = log(GDP_PPP_pc)
    ) %>%
    arrange(iso2c)
}

run_loan_calibration <- function(
    thresholds,
    wage_file,
    eur_gbp,
    rpi_schedule,
    month_weights,
    grow_first,
    rho_grid) {

  fixed_rpi <- stats::weighted.mean(rpi_schedule, month_weights)
  data <- prepare_loan_data(thresholds, wage_file, eur_gbp, grow_first)
  stopifnot(nrow(data) == 25L)

  elasticity_grid <- matrix(
    NA_real_,
    nrow = nrow(data),
    ncol = length(rho_grid)
  )

  income_gradient <- numeric(length(rho_grid))
  income_p_value <- numeric(length(rho_grid))

  for (j in seq_along(rho_grid)) {
    pv <- mapply(
      loan_pv,
      data$wage_gbp,
      data$lower,
      data$upper,
      MoreArgs = list(rho = rho_grid[j], fixed_rpi = fixed_rpi)
    )

    price_change <- log(post_brexit_price) - log(pv)
    elasticity_grid[, j] <- data$Estimate_controls / price_change

    income_model <- stats::lm(elasticity_grid[, j] ~ data$log_gdp)
    robust <- lmtest::coeftest(
      income_model,
      vcov. = sandwich::vcovHC(income_model, type = "HC1")
    )

    income_gradient[j] <- robust[2, 1]
    income_p_value[j] <- robust[2, 4]
  }

  first_eligible <- rho_grid >= 0.03 & income_p_value >= 0.05

  list(
    data = data,
    rho_grid = rho_grid,
    elasticity_grid = elasticity_grid,
    income_gradient = income_gradient,
    income_p_value = income_p_value,
    fixed_rpi = fixed_rpi,
    rho_insignificant = rho_grid[which(first_eligible)[1]],
    rho_neutral = rho_grid[which.min(abs(income_gradient))]
  )
}

loan_2025 <- run_loan_calibration(
  threshold_2025,
  file.path(path_raw, "wages_2024.xlsx"),
  eur_gbp = 0.848248,
  rpi_schedule = c(0.043, 0.032),
  month_weights = c(5, 7),
  grow_first = 1,
  rho_grid = seq(0.03, 0.35, by = 0.001)
)

loan_2021 <- run_loan_calibration(
  threshold_2021,
  file.path(path_raw, "wages_2021.xlsx"),
  eur_gbp = 0.885798,
  rpi_schedule = c(0.026, 0.015),
  month_weights = c(5, 7),
  grow_first = 0,
  rho_grid = seq(0.01, 0.35, by = 0.001)
)

loan_export <- function(result, filename) {
  elasticity <- data.frame(
    iso2c = result$data$iso2c,
    Country = result$data$Country,
    result$elasticity_grid,
    check.names = FALSE
  )
  names(elasticity)[-(1:2)] <- sprintf("rho_%.3f", result$rho_grid)

  summary <- data.frame(
    fixed_rpi = result$fixed_rpi,
    rho_first_insignificant = result$rho_insignificant,
    rho_income_neutral = result$rho_neutral
  )

  writexl::write_xlsx(
    list(
      country_inputs = result$data,
      elasticity_grid = elasticity,
      income_gradient = data.frame(
        rho = result$rho_grid,
        coefficient = result$income_gradient,
        HC1_p_value = result$income_p_value
      ),
      summary = summary
    ),
    file.path(path_derived, filename)
  )
}

loan_export(loan_2025, "loan_calibration_2025.xlsx")
loan_export(loan_2021, "loan_calibration_2021.xlsx")

group_thresholds <- function(x) {
  x %>%
    mutate(
      display_country = if_else(Country == "Czechia", "Czech Republic", Country)
    ) %>%
    group_by(lower, upper) %>%
    summarise(
      Countries = paste(sort(display_country), collapse = ", "),
      .groups = "drop"
    ) %>%
    arrange(lower) %>%
    select(Countries, lower, upper)
}

threshold_table_2021 <- group_thresholds(threshold_2021)
threshold_table_2025 <- group_thresholds(threshold_2025)

