figure_checks <- data.frame(
  check = c(
    "figure_1_event_study_rows",
    "figure_2_saturated_event_rows",
    "figure_3_decomposition_countries",
    "figure_4_controlled_country_models",
    "figure_5_panel_a_rows",
    "figure_5_panel_b_rows",
    "figure_5_panel_c_rows",
    "figure_6_split_period_rows",
    "figure_7_robustness_models",
    "figure_8_saturated_robustness_rows",
    "figure_9_rows",
    "figure_10_profile_rows",
    "figure_workbook_sheets",
    "figure_files"
  ),
  actual = c(
    nrow(figure_1_data),
    nrow(figure_2_data),
    nrow(figure_3_data),
    nrow(figure_4_data),
    nrow(figure_5_panel_a),
    nrow(figure_5_panel_b),
    nrow(figure_5_panel_c),
    nrow(figure_6_data),
    nrow(figure_7_data),
    nrow(figure_8_data),
    nrow(figure_9_data),
    nrow(figure_10_data),
    length(readxl::excel_sheets(
      file.path(path_derived, "figure_data.xlsx")
    )),
    length(figure_files)
  ),
  expected = c(
    10L,
    260L,
    26L,
    104L,
    75L,
    75L,
    75L,
    52L,
    9L,
    234L,
    75L,
    975L,
    13L,
    14L
  )
) %>%
  mutate(status = if_else(actual == expected, "passed", "failed"))

stopifnot(
  all(figure_checks$status == "passed"),
  all(is.finite(figure_1_data$estimate)),
  all(is.finite(figure_2_data$estimate)),
  all(is.finite(figure_3_data$eta_log)),
  all(is.finite(figure_4_data$elasticity)),
  all(is.finite(figure_5_panel_b$elasticity)),
  all(is.finite(figure_6_data$estimate)),
  all(is.finite(figure_7_data$estimate)),
  all(is.finite(figure_8_data$estimate)),
  all(is.finite(figure_9_data$elasticity)),
  all(is.finite(figure_10_data$estimate)),
  all(file.exists(figure_files)),
  all(file.info(figure_files)$size > 0)
)

utils::write.csv(
  figure_checks,
  file.path(path_audit, "figure_audit_summary.csv"),
  row.names = FALSE
)
