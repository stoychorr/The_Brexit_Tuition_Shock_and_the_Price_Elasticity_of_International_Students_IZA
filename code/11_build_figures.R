suppressPackageStartupMessages({
  library(ggplot2)
  library(ggrepel)
})

figure_workbook <- file.path(path_derived, "figure_data.xlsx")

read_figure_sheet <- function(sheet) {
  readxl::read_excel(figure_workbook, sheet = sheet)
}

save_figure <- function(plot, filename, width, height) {
  ggplot2::ggsave(
    filename = file.path(path_figures, filename),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 180,
    bg = "white"
  )
}

event_data <- read_figure_sheet("event_study")

figure_1 <- ggplot(event_data, aes(event_time, estimate)) +
  geom_hline(yintercept = 0, colour = "grey65") +
  geom_vline(xintercept = 0, linetype = "dotted", colour = "grey35") +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.08,
    colour = "grey45"
  ) +
  geom_point(size = 1.7) +
  scale_x_continuous(breaks = sort(unique(event_data$event_time))) +
  labs(
    title = "Event-study: EU vs synthetic control",
    x = "Years relative to 2020",
    y = "Log difference in applications"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

save_figure(figure_1, "figure_01_event_study.png", 7.2, 4.6)

saturated_event <- read_figure_sheet("saturated_event")

figure_2 <- ggplot(saturated_event, aes(event_time, estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_vline(xintercept = 0, linetype = "dotted", colour = "grey45") +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.12,
    linewidth = 0.3
  ) +
  geom_line(linewidth = 0.35) +
  geom_point(size = 1.2) +
  facet_wrap(~ country, scales = "free_y", ncol = 6) +
  labs(
    title = "Saturated DID event study by country",
    x = "Years relative to 2020",
    y = "Estimated log effect"
  ) +
  theme_minimal(base_size = 8) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    strip.text = element_text(size = 7),
    panel.grid.minor = element_blank()
  )

save_figure(figure_2, "figure_02_saturated_event_study.png", 10, 7.2)

decomposition <- read_figure_sheet("decomposition") %>%
  arrange(eta_log) %>%
  mutate(
    country = factor(country, levels = country),
    eta_arc_shift = eta_arc + 0.04,
    eta_mechanical_shift = eta_mechanical + 0.08
  )

figure_3 <- ggplot(decomposition, aes(y = country)) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "grey70",
    linewidth = 0.35
  ) +
  geom_errorbarh(
    aes(xmin = eta_log_low, xmax = eta_log_high),
    height = 0.16,
    linewidth = 0.4,
    colour = "grey45"
  ) +
  geom_segment(
    aes(x = eta_log, xend = eta_arc_shift, yend = country),
    colour = "grey80",
    linewidth = 0.3
  ) +
  geom_segment(
    aes(x = eta_arc_shift, xend = eta_mechanical_shift, yend = country),
    colour = "grey85",
    linewidth = 0.3
  ) +
  geom_point(
    aes(x = eta_log),
    shape = 21,
    size = 2.5,
    fill = "#2C6DB2",
    stroke = 0.25
  ) +
  geom_point(
    aes(x = eta_arc_shift),
    shape = 21,
    size = 2.3,
    fill = "#8FBCE6",
    stroke = 0.25
  ) +
  geom_point(
    aes(x = eta_mechanical_shift),
    shape = 21,
    size = 2.3,
    fill = "white",
    stroke = 0.25
  ) +
  labs(
    title = "Decomposition of post-Brexit demand elasticities",
    subtitle = paste(
      "Dark = signed log elasticity   Light = arc elasticity",
      "Hollow = percentage-change measure"
    ),
    x = "Demand elasticity",
    y = "Country"
  ) +
  theme_classic(base_size = 11) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

save_figure(figure_3, "figure_03_elasticity_decomposition.png", 9.2, 7)

controlled_elasticities <- read_figure_sheet("controlled_elasticities")

controlled_order <- controlled_elasticities %>%
  filter(model == "S4: + GDPpc + Pop + YouthUnempl") %>%
  arrange(elasticity) %>%
  pull(country)

controlled_elasticities <- controlled_elasticities %>%
  mutate(
    country = factor(country, levels = controlled_order),
    model = factor(
      model,
      levels = c(
        "S1: No controls",
        "S2: + GDPpc",
        "S3: + GDPpc + Pop",
        "S4: + GDPpc + Pop + YouthUnempl"
      )
    )
  )

figure_4 <- ggplot(
  controlled_elasticities,
  aes(elasticity, country, colour = model, group = country)
) +
  geom_line(colour = "grey75", linewidth = 0.5) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.12,
    linewidth = 0.45,
    alpha = 0.85
  ) +
  geom_point(size = 2.2) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "grey70",
    linewidth = 0.35
  ) +
  scale_colour_manual(
    values = c("#4C78A8", "#72B7B2", "#F2CF5B", "#E45756"),
    name = NULL
  ) +
  labs(
    title = "Country-specific demand elasticities across saturated DiD specifications",
    subtitle = "Country-specific post-2021 effects rescaled by the tuition fee increase",
    x = "Implied demand elasticity",
    y = NULL
  ) +
  theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top"
  )

save_figure(figure_4, "figure_04_controlled_elasticities.png", 9.2, 7)

raw_income <- read_figure_sheet("raw_income") %>%
  mutate(
    model = factor(
      model,
      levels = c(
        "TWFE with controls",
        "TWFE without controls",
        "Pooled"
      )
    )
  )

figure_5a <- ggplot(raw_income, aes(log_gdp, estimate, colour = model)) +
  geom_point(size = 1.5) +
  geom_text(aes(label = iso2c), vjust = -0.7, size = 2.3) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    colour = "red",
    linewidth = 0.7
  ) +
  facet_wrap(~ model, nrow = 1, scales = "free_y") +
  scale_colour_manual(values = c("#1F4E79", "#D55E00", "#009E73")) +
  labs(
    title = "Raw treatment coefficients and log of GDP per capita",
    x = "log GDP per capita, PPP, constant",
    y = "Raw country coefficient"
  ) +
  theme_bw(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

save_figure(figure_5a, "figure_05_panel_a_raw_income_gradient.png", 9.5, 4.7)

income_rates <- read_figure_sheet("income_rates") %>%
  mutate(rate = factor(rate, levels = unique(rate)))

figure_5b <- ggplot(income_rates, aes(log_gdp, elasticity)) +
  geom_point(size = 1.5, colour = "#1F4E79") +
  geom_text(
    aes(label = iso2c),
    vjust = -0.7,
    size = 2.3,
    colour = "#1F4E79"
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    colour = "red",
    linewidth = 0.7
  ) +
  facet_wrap(~ rate, nrow = 1, scales = "free_y") +
  labs(
    title = "Country elasticities at 3%, income neutrality, and 35%",
    x = "log GDP per capita, PPP, constant",
    y = "Estimated price elasticity"
  ) +
  theme_bw(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

save_figure(figure_5b, "figure_05_panel_b_loan_adjusted_income.png", 9.5, 4.7)

plot_country_rates <- function(data, title, subtitle, filename) {
  wide <- data %>%
    pivot_wider(names_from = rate, values_from = elasticity) %>%
    mutate(
      average = rowMeans(across(where(is.numeric))),
      iso2c = factor(iso2c, levels = iso2c[order(average)])
    ) %>%
    select(-average)

  long <- wide %>%
    pivot_longer(
      cols = -iso2c,
      names_to = "rate",
      values_to = "elasticity"
    )

  endpoints <- long %>%
    group_by(iso2c) %>%
    summarise(
      minimum = min(elasticity),
      maximum = max(elasticity),
      .groups = "drop"
    )

  plot <- ggplot(long, aes(elasticity, iso2c)) +
    annotate(
      "rect",
      xmin = -0.80,
      xmax = -0.10,
      ymin = -Inf,
      ymax = Inf,
      fill = "grey75",
      alpha = 0.35
    ) +
    geom_segment(
      data = endpoints,
      aes(x = minimum, xend = maximum, yend = iso2c),
      colour = "#9FB4C7",
      linewidth = 1.2
    ) +
    geom_point(
      data = filter(long, rate == "Insignificance threshold"),
      colour = "#D55E00",
      size = 1.8
    ) +
    geom_point(
      data = filter(long, rate == "Income neutrality"),
      colour = "#1F4E79",
      size = 1.8
    ) +
    geom_point(
      data = filter(long, rate == "35% discount rate"),
      colour = "#009E73",
      size = 1.8
    ) +
    geom_vline(xintercept = 0, colour = "grey60") +
    labs(title = title, subtitle = subtitle, x = "Estimated price elasticity", y = NULL) +
    theme_classic(base_size = 10) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))

  save_figure(plot, filename, 8.2, 7)
}

plot_country_rates(
  read_figure_sheet("country_rates_2025"),
  "Country elasticities under alternative discount rates",
  paste0(
    "Insignificance at ",
    round(100 * loan_2025$rho_insignificant, 1),
    "%, income neutrality at ",
    round(100 * loan_2025$rho_neutral, 1),
    "%, and upper benchmark at 35%"
  ),
  "figure_05_panel_c_country_discount_rates.png"
)

split_period <- read_figure_sheet("split_period") %>%
  select(country, period, estimate) %>%
  pivot_wider(names_from = period, values_from = estimate) %>%
  arrange(desc(`Later 2024-2025`)) %>%
  mutate(country = factor(country, levels = country))

split_long <- split_period %>%
  pivot_longer(
    cols = c(`Initial 2021-2023`, `Later 2024-2025`),
    names_to = "period",
    values_to = "estimate"
  )

figure_6 <- ggplot(split_long, aes(estimate, country)) +
  geom_segment(
    data = split_period,
    aes(
      x = `Initial 2021-2023`,
      xend = `Later 2024-2025`,
      yend = country
    ),
    colour = "grey75",
    linewidth = 0.6
  ) +
  geom_point(aes(colour = period), size = 2.4) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "grey70",
    linewidth = 0.35
  ) +
  scale_colour_manual(values = c("#4C78A8", "#E45756"), name = NULL) +
  labs(
    title = "Country-specific post-reform log effects by period",
    subtitle = "Initial 2021-2023 estimates and later 2024-2025 estimates",
    x = "Log effect on applications",
    y = NULL
  ) +
  theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top"
  )

save_figure(figure_6, "figure_06_split_period_effects.png", 9, 7)

pooled_robustness <- read_figure_sheet("pooled_robustness") %>%
  mutate(
    model = factor(model, levels = rev(unique(model))),
    group = factor(group)
  )

figure_7 <- ggplot(
  pooled_robustness,
  aes(estimate, model, colour = group)
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.18
  ) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = c(
    Baseline = "#1F77B4",
    Poor = "#D62728",
    Rich = "#2CA02C",
    OECD = "#9467BD",
    Ireland = "#FF7F0E"
  )) +
  labs(
    title = "Robustness of the Brexit effect",
    subtitle = "Pooled DiD estimates",
    x = "Effect on log applicants",
    y = NULL,
    colour = NULL
  ) +
  theme_classic(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

save_figure(figure_7, "figure_07_pooled_control_robustness.png", 8.8, 6)

saturated_robustness <- read_figure_sheet("saturated_robustness") %>%
  mutate(model = factor(model, levels = unique(model)))

ranked_countries <- saturated_robustness %>%
  group_by(country) %>%
  summarise(average = mean(estimate), .groups = "drop") %>%
  arrange(average)

lowest_country <- ranked_countries$country[1]
highest_country <- ranked_countries$country[nrow(ranked_countries)]

saturated_robustness <- saturated_robustness %>%
  mutate(group = case_when(
    country == lowest_country ~ "Lowest",
    country == highest_country ~ "Highest",
    TRUE ~ "Other"
  ))

average_path <- saturated_robustness %>%
  group_by(model) %>%
  summarise(estimate = mean(estimate), .groups = "drop") %>%
  mutate(country = "AVERAGE", group = "Average")

figure_8 <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_line(
    data = filter(saturated_robustness, group == "Other"),
    aes(model, estimate, group = country),
    colour = "grey75",
    alpha = 0.5
  ) +
  geom_line(
    data = filter(saturated_robustness, group != "Other"),
    aes(model, estimate, group = country, colour = group),
    linewidth = 1.2
  ) +
  geom_line(
    data = average_path,
    aes(model, estimate, group = country, colour = group),
    linewidth = 1.4
  ) +
  geom_point(
    data = filter(saturated_robustness, group != "Other"),
    aes(model, estimate, colour = group),
    size = 2.1
  ) +
  geom_point(
    data = average_path,
    aes(model, estimate, colour = group),
    size = 2.4
  ) +
  ggrepel::geom_text_repel(
    data = bind_rows(
      filter(
        saturated_robustness,
        country %in% c(lowest_country, highest_country),
        model == "Ireland"
      ),
      filter(average_path, model == "Ireland")
    ),
    aes(model, estimate, label = country, colour = group),
    nudge_x = 0.35,
    hjust = 0,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = c(
    Lowest = "#8B0000",
    Highest = "#2166AC",
    Average = "black"
  )) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Saturated DiD robustness",
    subtitle = "Lowest, highest, and average trajectories",
    x = NULL,
    y = "Effect on log applicants",
    colour = NULL
  ) +
  theme_classic(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.margin = margin(10, 75, 10, 10)
  )

save_figure(figure_8, "figure_08_saturated_control_robustness.png", 9, 6.5)

plot_country_rates(
  read_figure_sheet("country_rates_2021"),
  "Country elasticities under alternative discount rates",
  paste0(
    "Insignificance at ",
    round(100 * loan_2021$rho_insignificant, 1),
    "%, income neutrality at ",
    round(100 * loan_2021$rho_neutral, 1),
    "%, and upper benchmark at 35%"
  ),
  "figure_09_2021_information_set.png"
)

discount_correlations <- read_figure_sheet("discount_correlations")

sign_changes <- discount_correlations %>%
  arrange(variable, rho) %>%
  group_by(variable) %>%
  summarise(
    sign_rho = {
      crossing <- which(
        estimate[-n()] * estimate[-1] < 0
      )[1]
      if (is.na(crossing)) NA_real_ else rho[crossing]
    },
    .groups = "drop"
  ) %>%
  filter(is.finite(sign_rho))

figure_10 <- ggplot(discount_correlations, aes(100 * rho, estimate)) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    fill = "#1F4E79",
    alpha = 0.15
  ) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_line(colour = "#1F4E79", linewidth = 0.7) +
  geom_vline(
    data = sign_changes,
    aes(xintercept = 100 * sign_rho),
    colour = "red",
    linewidth = 0.5
  ) +
  facet_wrap(~ label, scales = "free_y", ncol = 4) +
  scale_x_continuous(
    breaks = c(3, 10, 20, 30, 40),
    labels = paste0(c(3, 10, 20, 30, 40), "%")
  ) +
  labs(
    title = "Which correlations survive the discounting assumption",
    subtitle = paste(
      "Shaded bands show 95% HC1 confidence intervals",
      "and red lines mark sign changes"
    ),
    x = "Subjective discount rate",
    y = "Correlation with estimated elasticity"
  ) +
  theme_bw(base_size = 8) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(size = 7),
    panel.grid.minor = element_blank()
  )

save_figure(figure_10, "figure_10_discount_correlations.png", 11, 7)

price <- seq(0.65, 2.75, length.out = 300)
eta_curve <- 0.70
quantity <- price^(-eta_curve)
p0 <- 1
p1 <- 2.35
q0 <- p0^(-eta_curve)
q1 <- p1^(-eta_curve)
secant_slope <- (q1 - q0) / (p1 - p0)
tangent_slope <- -eta_curve * q0 / p0

figure_a1_data <- data.frame(price = price, quantity = quantity)

figure_a1 <- ggplot(figure_a1_data, aes(price, quantity)) +
  geom_line(linewidth = 0.8, colour = "grey25") +
  annotate(
    "segment",
    x = p0,
    y = q0,
    xend = p1,
    yend = q1,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  geom_abline(
    intercept = q0 - tangent_slope * p0,
    slope = tangent_slope,
    linetype = "dotted",
    colour = "grey45"
  ) +
  geom_point(
    data = data.frame(price = c(p0, p1), quantity = c(q0, q1)),
    size = 2
  ) +
  annotate("text", x = 0.92, y = q0 + 0.08, label = "(P0, Q0)", size = 3) +
  annotate("text", x = p1 + 0.05, y = q1 - 0.03, label = "(P1, Q1)", size = 3) +
  annotate(
    "text",
    x = 1.05,
    y = max(quantity) * 0.92,
    label = "Q(P) == A * P^{-eta}",
    parse = TRUE,
    size = 4
  ) +
  labs(x = "Price P", y = "Quantity Q") +
  coord_cartesian(clip = "off") +
  theme_classic(base_size = 11)

save_figure(figure_a1, "figure_a1_elasticity_geometry.png", 7.2, 4.5)

rpi_plot_data <- read_figure_sheet("rpi_schedule") %>%
  mutate(
    start = c(2020.33, 2021.67, 2024.33, 2025.67),
    end = c(2021.67, 2022.58, 2025.67, 2026.58),
    colour = c("#4169E1", "#D55E00", "#4169E1", "#D55E00")
  )

figure_a2 <- ggplot() +
  annotate(
    "segment",
    x = 2020,
    xend = 2027,
    y = 0,
    yend = 0,
    arrow = grid::arrow(length = grid::unit(0.12, "inches")),
    linewidth = 0.6
  ) +
  geom_segment(
    data = rpi_plot_data,
    aes(x = start, xend = end, y = 0.45, yend = 0.45, colour = colour),
    arrow = grid::arrow(length = grid::unit(0.10, "inches")),
    linewidth = 1
  ) +
  geom_text(
    data = rpi_plot_data,
    aes(
      x = (start + end) / 2,
      y = 0.62,
      label = paste0(round(100 * rate, 1), "% RPI"),
      colour = colour
    ),
    fontface = "bold",
    size = 3.4
  ) +
  annotate(
    "text",
    x = 2021.3,
    y = -0.35,
    label = paste0(
      "Apr 2021 to Mar 2022\n5 months + 7 months = ",
      round(100 * loan_2021$fixed_rpi, 4),
      "%"
    ),
    size = 3
  ) +
  annotate(
    "text",
    x = 2025.3,
    y = -0.35,
    label = paste0(
      "Apr 2025 to Mar 2026\n5 months + 7 months = ",
      round(100 * loan_2025$fixed_rpi, 4),
      "%"
    ),
    size = 3
  ) +
  scale_colour_identity() +
  scale_x_continuous(breaks = 2020:2027) +
  coord_cartesian(ylim = c(-0.65, 0.85), clip = "off") +
  labs(title = "Plan 2 RPI calibration across the two repayment years") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank()
  )

save_figure(figure_a2, "figure_a2_rpi_calibration.png", 11, 5)

figure_files <- c(
  figure_01 = file.path(path_figures, "figure_01_event_study.png"),
  figure_02 = file.path(path_figures, "figure_02_saturated_event_study.png"),
  figure_03 = file.path(path_figures, "figure_03_elasticity_decomposition.png"),
  figure_04 = file.path(path_figures, "figure_04_controlled_elasticities.png"),
  figure_05a = file.path(path_figures, "figure_05_panel_a_raw_income_gradient.png"),
  figure_05b = file.path(path_figures, "figure_05_panel_b_loan_adjusted_income.png"),
  figure_05c = file.path(path_figures, "figure_05_panel_c_country_discount_rates.png"),
  figure_06 = file.path(path_figures, "figure_06_split_period_effects.png"),
  figure_07 = file.path(path_figures, "figure_07_pooled_control_robustness.png"),
  figure_08 = file.path(path_figures, "figure_08_saturated_control_robustness.png"),
  figure_09 = file.path(path_figures, "figure_09_2021_information_set.png"),
  figure_10 = file.path(path_figures, "figure_10_discount_correlations.png"),
  figure_a1 = file.path(path_figures, "figure_a1_elasticity_geometry.png"),
  figure_a2 = file.path(path_figures, "figure_a2_rpi_calibration.png")
)

stopifnot(
  length(figure_files) == 14L,
  all(file.exists(figure_files)),
  all(file.info(figure_files)$size > 0)
)
