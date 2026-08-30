panel_wb_base <- panel_main %>%
  filter(Year <= 2024) %>%
  mutate(did = EU * Post)

panel_countries <- unique(panel_wb_base$Domicile_named_country)

iso_map <- data.frame(Domicile_named_country = panel_countries) %>%
  mutate(
    iso2c = countrycode::countrycode(
      Domicile_named_country,
      origin = "country.name",
      destination = "iso2c",
      custom_match = c("Cyprus (European Union)" = "CY"),
      warn = TRUE
    ),
    iso3c = countrycode::countrycode(
      iso2c,
      origin = "iso2c",
      destination = "iso3c",
      warn = FALSE
    )
  )

stopifnot(!anyNA(iso_map$iso3c))

refresh_wdi <- identical(
  tolower(Sys.getenv("REFRESH_WDI", "false")),
  "true"
)

frozen_wdi_file <- file.path(
  path_reference,
  "panel_wb_2016_2024_audited_2026_07.xlsx"
)

use_frozen_wdi <- file.exists(frozen_wdi_file) && !refresh_wdi

wb_codes <- c(
  GDPpc = "NY.GDP.PCAP.PP.KD",
  Pop15_64 = "SP.POP.1564.TO",
  YouthUnempl = "SL.UEM.1524.ZS",
  EduSpendGDP = "SE.XPD.TOTL.GD.ZS"
)

if (!use_frozen_wdi) {
  if (!requireNamespace("WDI", quietly = TRUE)) {
    stop(
      "The frozen WDI file is unavailable or REFRESH_WDI=true. ",
      "Install the WDI package to download the indicators."
    )
  }

  wb_raw <- WDI::WDI(
    country = iso_map$iso2c,
    indicator = wb_codes,
    start = 2016,
    end = 2024,
    extra = FALSE
  )

  impute_lbn_2024 <- function(x, variable) {
    target <- x$iso3c == "LBN" & x$year == 2024
    source <- x$iso3c == "LBN" & x$year %in% c(2022, 2023)
    stopifnot(sum(target) == 1L, sum(source) == 2L)
    if (is.na(x[[variable]][target])) {
      x[[variable]][target] <- mean(x[[variable]][source])
    }
    x
  }

  wb_raw <- impute_lbn_2024(wb_raw, "GDPpc")
  wb_raw <- impute_lbn_2024(wb_raw, "YouthUnempl")

  wb_clean <- wb_raw %>%
    transmute(
      iso3c,
      Year = as.integer(year),
      GDPpc,
      Pop15_64,
      YouthUnempl,
      EduSpendGDP
    )
} else {
  wb_reference <- readxl::read_excel(
    frozen_wdi_file,
    sheet = 1
  )

  wb_clean <- wb_reference %>%
    transmute(
      iso3c,
      Year = as.integer(Year),
      GDPpc = as.numeric(GDPpc),
      Pop15_64 = as.numeric(Pop15_64),
      YouthUnempl = as.numeric(YouthUnempl),
      EduSpendGDP = as.numeric(EduSpendGDP)
    ) %>%
    distinct(iso3c, Year, .keep_all = TRUE)
}

panel_wb <- panel_wb_base %>%
  left_join(
    iso_map %>% select(Domicile_named_country, iso3c),
    by = "Domicile_named_country"
  ) %>%
  left_join(wb_clean, by = c("iso3c", "Year")) %>%
  filter(
    !is.na(GDPpc),
    !is.na(Pop15_64),
    !is.na(YouthUnempl)
  ) %>%
  mutate(
    c_log_GDPpc = log(GDPpc) - mean(log(GDPpc)),
    c_log_Pop15_64 = log(Pop15_64) - mean(log(Pop15_64)),
    c_YouthUnempl = YouthUnempl - mean(YouthUnempl)
  ) %>%
  arrange(Domicile_named_country, Year)

stopifnot(
  nrow(panel_wb) == 468L,
  n_distinct(panel_wb$Domicile_named_country) == 52L,
  n_distinct(panel_wb$Year) == 9L,
  all(panel_wb$GDPpc > 0),
  all(panel_wb$Pop15_64 > 0),
  !anyNA(panel_wb[c(
    "GDPpc", "Pop15_64", "YouthUnempl",
    "c_log_GDPpc", "c_log_Pop15_64", "c_YouthUnempl"
  )])
)

panel_wb_sat <- panel_wb %>%
  mutate(
    treated_factor = factor(if_else(
      Domicile_named_country %in% eu_real,
      Domicile_named_country,
      "CONTROL"
    ))
  )

writexl::write_xlsx(
  list(panel_wb = panel_wb_sat),
  file.path(path_derived, "panel_wb_2016_2024.xlsx")
)
