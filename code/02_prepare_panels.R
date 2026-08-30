main_input <- file.path(path_raw, "ucas_main_2016_2025.xlsx")
apps_raw <- readxl::read_excel(main_input, sheet = "full_data")

required_main_columns <- c(
  "Domicile_named_country", "Year", "Applicants"
)
stopifnot(all(required_main_columns %in% names(apps_raw)))

apps_clean <- apps_raw %>%
  filter(
    Year %in% 2016:2025,
    !Domicile_named_country %in% non_countries
  ) %>%
  mutate(
    Domicile_named_country = as.character(Domicile_named_country),
    Year = as.integer(Year),
    EU = as.integer(Domicile_named_country %in% eu_real),
    Post = as.integer(Year >= 2021),
    log_app = log(Applicants)
  )

stopifnot(
  all(apps_clean$Applicants > 0),
  all(is.finite(apps_clean$log_app)),
  !anyDuplicated(apps_clean[c("Domicile_named_country", "Year")])
)

donor_weights <- readxl::read_excel(
  file.path(path_derived, "control_group_output.xlsx"),
  sheet = "weights"
)

eu_synth <- donor_weights$donor_country

regression_weights <- bind_rows(
  donor_weights %>%
    transmute(
      Domicile_named_country = donor_country,
      regression_weight
    ),
  data.frame(
    Domicile_named_country = eu_real,
    regression_weight = 1
  )
)

panel_main <- apps_raw %>%
  filter(
    Domicile_named_country %in% c(eu_real, eu_synth),
    Year %in% 2016:2025
  ) %>%
  mutate(
    Domicile_named_country = as.character(Domicile_named_country),
    Year = as.integer(Year),
    log_app = log(Applicants),
    EU = as.integer(Domicile_named_country %in% eu_real),
    Treated = EU,
    Post = as.integer(Year >= 2021),
    event_time = Year - 2020L,
    EU_group = if_else(EU == 1L, "Real_EU", "Synthetic_EU"),
    treated_factor = factor(if_else(
      EU == 1L,
      Domicile_named_country,
      "CONTROL"
    ))
  ) %>%
  left_join(regression_weights, by = "Domicile_named_country") %>%
  arrange(Domicile_named_country, Year)

stopifnot(
  nrow(panel_main) == 520L,
  n_distinct(panel_main$Domicile_named_country) == 52L,
  n_distinct(panel_main$Year) == 10L,
  !anyDuplicated(panel_main[c("Domicile_named_country", "Year")]),
  !anyNA(panel_main$regression_weight),
  all(panel_main$Applicants > 0),
  all(is.finite(panel_main$log_app))
)

panel_covid <- panel_main %>%
  mutate(
    Post = as.integer(Year %in% 2021:2023),
    Post_COVID = as.integer(Year >= 2024)
  )

synth_poor <- c(
  "Pakistan", "Nepal", "Bangladesh", "Ethiopia", "Uganda",
  "Tanzania, United Republic of", "Rwanda", "Mozambique", "Malawi",
  "Sierra Leone", "Ghana", "Nigeria", "Kenya", "Zimbabwe"
)

synth_rich <- c(
  "United States of America", "Canada", "Australia", "Norway",
  "Switzerland", "Singapore", "Qatar", "United Arab Emirates",
  "Kuwait", "Saudi Arabia", "Israel", "Japan"
)

oecd <- c(
  "Australia", "Canada", "Chile", "Colombia", "Iceland", "Israel",
  "Japan", "Korea, Republic of", "Mexico", "New Zealand", "Norway",
  "Switzerland", "Turkey", "United States of America"
)

data_baseline <- apps_clean %>%
  filter(Domicile_named_country %in% c(eu_real, eu_synth))

data_poor <- apps_clean %>%
  filter(Domicile_named_country %in% c(eu_real, synth_poor))

data_rich <- apps_clean %>%
  filter(Domicile_named_country %in% c(eu_real, synth_rich))

data_oecd <- apps_clean %>%
  filter(Domicile_named_country %in% c(eu_real, oecd))

writexl::write_xlsx(
  list(panel_main = panel_main),
  file.path(path_derived, "panel_main_2016_2025.xlsx")
)

