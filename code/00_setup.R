options(stringsAsFactors = FALSE, scipen = 999)

required_packages <- c(
  "readxl", "writexl", "dplyr", "tidyr", "Rsolnp", "fixest",
  "broom", "countrycode", "synthdid", "sandwich", "lmtest",
  "knitr", "rmarkdown", "ggplot2", "stringr", "purrr", "ggrepel",
  "WDI", "haven"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing R packages: ", paste(missing_packages, collapse = ", "),
    ". Install them before running the replication."
  )
}

suppressPackageStartupMessages({
  library(readxl)
  library(writexl)
  library(dplyr)
  library(tidyr)
  library(fixest)
  library(broom)
})

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

if (!file.exists(file.path(project_root, "replication.Rmd"))) {
  stop("Run replication.Rmd or run_replication.R from the package root.")
}

path_raw <- file.path(project_root, "data", "raw")
path_reference <- file.path(project_root, "data", "reference")
path_derived <- file.path(project_root, "data", "derived")
path_tables <- file.path(project_root, "output", "tables")
path_audit <- file.path(project_root, "output", "audit")
path_figures <- file.path(project_root, "output", "figures")

dir.create(path_derived, recursive = TRUE, showWarnings = FALSE)
dir.create(path_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(path_audit, recursive = TRUE, showWarnings = FALSE)
dir.create(path_figures, recursive = TRUE, showWarnings = FALSE)

eu_real <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia",
  "Cyprus (European Union)", "Czech Republic", "Denmark", "Estonia",
  "Finland", "France", "Germany", "Greece", "Hungary", "Italy",
  "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
  "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
  "Spain", "Sweden"
)

non_countries <- c(
  "England", "Scotland", "Wales", "Northern Ireland",
  "Alderney", "Guernsey", "Jersey", "Isle of Man",
  "Anguilla", "Bermuda", "British Antarctic Territory",
  "British Indian Ocean Territory", "British Virgin Islands",
  "Cayman Islands", "Falkland Islands (Malvinas)", "Gibraltar",
  "Montserrat", "St Helena, Ascension & Tristan da Cunha",
  "Turks and Caicos Islands", "Aland Islands", "Aruba",
  "Canary Islands", "Christmas Island", "Curacao", "Faroe Islands",
  "French Guiana", "French Polynesia", "Greenland", "Guadeloupe",
  "Guam", "Hong Kong", "Macao", "Martinique",
  "Netherlands Antilles", "New Caledonia", "Norfolk Island",
  "Northern Mariana Islands", "Puerto Rico", "Reunion",
  "Sint Maarten (Dutch Part)", "St Barthelemy",
  "St Martin (French part)", "Virgin Islands (US)",
  "Cyprus (Non-European Union)", "Cyprus (Not otherwise specified)",
  "All", "No information provided (Overseas)", "Not Known",
  "Stateless", "USSR (Not In Use)", "French West Indies (Not In Use)"
)

clean_country_for_output <- function(x) {
  x <- sub("^Cyprus \\(European Union\\)$", "Cyprus", x)
  x
}

write_csv_plain <- function(x, filename) {
  utils::write.csv(x, file.path(path_tables, filename), row.names = FALSE)
}
