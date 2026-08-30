numeric_country_results <- function(model, specification, suffix = "Post") {
  tidy_model <- broom::tidy(model)
  selected <- tidy_model[
    grepl(paste0("^treated_factor::.*:", suffix, "$"), tidy_model$term),
    ,
    drop = FALSE
  ]

  data.frame(
    country = clean_country_for_output(sub(
      paste0(":", suffix, "$"),
      "",
      sub("^treated_factor::", "", selected$term)
    )),
    specification = specification,
    actual = selected$estimate,
    actual_se = selected$std.error,
    actual_p = selected$p.value,
    actual_stars = audit_star(selected$p.value)
  )
}

audit_star <- function(p_value) {
  ifelse(
    is.na(p_value), "",
    ifelse(
      p_value < 0.001, "***",
      ifelse(
        p_value < 0.01, "**",
        ifelse(p_value < 0.05, "*", ifelse(p_value < 0.10, "+", ""))
      )
    )
  )
}

baseline_actual <- bind_rows(
  numeric_country_results(m_sat_DK, "Weighted TWFE"),
  numeric_country_results(m_sat_DK1, "Weighted country FE"),
  numeric_country_results(m_sat_DK2, "Weighted no FE"),
  numeric_country_results(m_sat_DK3, "Unweighted TWFE")
)

baseline_expected <- utils::read.csv(
  file.path(path_reference, "expected_table_a1_panel_a.csv"),
  check.names = FALSE
) %>%
  select(
    country,
    specification,
    expected = reproduced,
    expected_se = reproduced_se,
    expected_stars = reproduced_stars
  )

audit_baseline <- baseline_actual %>%
  inner_join(baseline_expected, by = c("country", "specification")) %>%
  mutate(
    coefficient_difference = actual - expected,
    se_difference = actual_se - expected_se
  )

stopifnot(
  nrow(audit_baseline) == 104L,
  max(abs(audit_baseline$coefficient_difference)) < 1e-4,
  max(abs(audit_baseline$se_difference)) < 1e-4,
  identical(audit_baseline$actual_stars, audit_baseline$expected_stars)
)

controlled_actual_for_model <- function(model, specification) {
  tidy_model <- broom::tidy(model)

  terms <- ifelse(
    grepl("^treated_factor::.*:Post$", tidy_model$term),
    clean_country_for_output(sub(
      ":Post$",
      "",
      sub("^treated_factor::", "", tidy_model$term)
    )),
    tidy_model$term
  )

  data.frame(
    term = terms,
    specification = specification,
    actual = tidy_model$estimate,
    actual_se = tidy_model$std.error,
    actual_p = tidy_model$p.value,
    actual_stars = audit_star(tidy_model$p.value)
  )
}

controlled_actual <- bind_rows(
  controlled_actual_for_model(s1, "S1 Saturated DiD"),
  controlled_actual_for_model(s2, "S2 + GDPpc"),
  controlled_actual_for_model(s3, "S3 + GDPpc + Pop"),
  controlled_actual_for_model(s4, "S4 + GDPpc + Pop + YouthUnempl")
)

controlled_expected <- utils::read.csv(
  file.path(path_reference, "expected_table_a1_panel_b.csv"),
  check.names = FALSE
) %>%
  select(
    term,
    specification,
    expected = reproduced,
    expected_se = reproduced_se,
    expected_stars = reproduced_stars
  ) %>%
  mutate(
    term = clean_country_for_output(term)
  )

audit_controlled <- controlled_actual %>%
  inner_join(controlled_expected, by = c("term", "specification")) %>%
  mutate(
    coefficient_difference = actual - expected,
    se_difference = actual_se - expected_se
  )

stopifnot(
  nrow(audit_controlled) == nrow(controlled_expected),
  max(abs(audit_controlled$coefficient_difference)) < 1e-4,
  max(abs(audit_controlled$se_difference)) < 1e-4,
  identical(audit_controlled$actual_stars, audit_controlled$expected_stars)
)

split_actual <- bind_rows(
  numeric_country_results(m_sat_DK_2, "split", "Post") %>%
    mutate(period = "initial"),
  numeric_country_results(m_sat_DK_2, "split", "Post_COVID") %>%
    mutate(period = "later")
) %>%
  select(country, period, actual, actual_se, actual_p, actual_stars)

split_expected <- utils::read.csv(
  file.path(path_reference, "expected_table_a2.csv"),
  check.names = FALSE
) %>%
  select(
    country,
    period,
    expected = reproduced,
    expected_se = reproduced_se,
    expected_stars = reproduced_stars
  )

audit_split <- split_actual %>%
  inner_join(split_expected, by = c("country", "period")) %>%
  mutate(
    coefficient_difference = actual - expected,
    se_difference = actual_se - expected_se
  )

stopifnot(
  nrow(audit_split) == 52L,
  max(abs(audit_split$coefficient_difference)) < 1e-4,
  max(abs(audit_split$se_difference)) < 1e-4,
  identical(audit_split$actual_stars, audit_split$expected_stars)
)

sdid_expected <- utils::read.csv(
  file.path(path_reference, "expected_table_5.csv"),
  check.names = FALSE
) %>%
  transmute(
    Country = Country,
    expected = SDID_reproduced
  )

audit_sdid <- sdid_country_results %>%
  transmute(
    Country = clean_country_for_output(Country),
    actual = Synthetic_DiD
  ) %>%
  inner_join(sdid_expected, by = "Country") %>%
  mutate(coefficient_difference = actual - expected)

stopifnot(
  nrow(audit_sdid) == 26L,
  max(abs(audit_sdid$coefficient_difference)) < 1e-4
)

pooled_att <- broom::tidy(m1)
pooled_att <- pooled_att$estimate[
  pooled_att$term %in% c("Treated:Post", "Post:Treated")
]
stopifnot(length(pooled_att) == 1L, abs(pooled_att + 1.108843703) < 1e-4)

table_3_actual_for_model <- function(model, model_name) {
  tidy_model <- broom::tidy(model)
  term <- tidy_model$term
  term[term %in% c("Treated:Post", "Post:Treated")] <- "did"

  interaction_names <- c(
    c_log_GDPpc = "did_x_c_log_GDPpc",
    c_log_Pop15_64 = "did_x_c_log_Pop15_64",
    c_YouthUnempl = "did_x_c_YouthUnempl"
  )

  for (variable in names(interaction_names)) {
    is_interaction <- grepl("did", term, fixed = TRUE) &
      grepl(variable, term, fixed = TRUE) &
      grepl(":", term, fixed = TRUE)
    term[is_interaction] <- interaction_names[[variable]]
  }

  data.frame(
    model = model_name,
    term = term,
    actual = tidy_model$estimate,
    actual_se = tidy_model$std.error
  )
}

table_3_actual <- bind_rows(
  table_3_actual_for_model(m1, "M1"),
  table_3_actual_for_model(m2, "M2"),
  table_3_actual_for_model(m4, "M4"),
  table_3_actual_for_model(m5, "M5"),
  table_3_actual_for_model(m6, "M6")
)

table_3_expected <- utils::read.csv(
  file.path(path_reference, "expected_table_3_current_wdi.csv"),
  check.names = FALSE
) %>%
  select(
    model,
    term,
    published = reported,
    expected = reproduced,
    published_se = reported_se,
    expected_se = reproduced_se
  )

audit_table_3 <- table_3_expected %>%
  inner_join(table_3_actual, by = c("model", "term")) %>%
  mutate(
    coefficient_difference = actual - expected,
    se_difference = actual_se - expected_se,
    published_coefficient_difference = actual - published,
    published_se_difference = actual_se - published_se
  )

stopifnot(
  nrow(audit_table_3) == 17L,
  max(abs(audit_table_3$coefficient_difference)) < 1e-4,
  max(abs(audit_table_3$se_difference)) < 1e-4
)

write_csv_plain(audit_baseline, "audit_table_a1_panel_a.csv")
write_csv_plain(audit_controlled, "audit_table_a1_panel_b.csv")
write_csv_plain(audit_split, "audit_table_a2.csv")
write_csv_plain(audit_sdid, "audit_table_5.csv")

utils::write.csv(
  audit_table_3,
  file.path(path_audit, "table_3_published_vs_audited_wdi.csv"),
  row.names = FALSE
)

audit_summary <- data.frame(
  check = c(
    "control_group_weights",
    "baseline_country_models",
    "controlled_country_models",
    "split_period_model",
    "synthetic_did",
    "pooled_att",
    "table_3_current_wdi",
    "table_3_published_vs_audited_wdi"
  ),
  status = c(
    rep("passed", 7L),
    "documented_vintage_difference"
  )
)

utils::write.csv(
  audit_summary,
  file.path(path_audit, "audit_summary.csv"),
  row.names = FALSE
)
