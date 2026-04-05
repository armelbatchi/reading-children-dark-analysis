source(file.path("R", "04_multilevel_models_table2.R"))

country_slopes_stage1 <- load_or_build("country_slopes_stage1", {
  country_meta <- pirls_analytic %>%
    group_by(iso3c, country_name, region, cycle) %>%
    summarise(
      electrification_cycle = clean_electrification(w_mean_safe(electrification, weight)),
      n_country_cycle = n(),
      .groups = "drop"
    ) %>%
    arrange(iso3c, desc(cycle), desc(electrification_cycle)) %>%
    group_by(iso3c) %>%
    summarise(
      country_name = first(country_name),
      region = first(region),
      electrification = clean_electrification(first(electrification_cycle)),
      n_country = sum(n_country_cycle),
      .groups = "drop"
    ) %>%
    mutate(
      elec_group = pirls_elec_group_fun(iso3c, electrification),
      elec_deficit = pmax(100 - electrification, 0),
      elec_deficit_log = log(elec_deficit + 0.1)
    )

  book_sd <- model_df %>%
    group_by(iso3c) %>%
    summarise(sd_books_raw = first(sd_books_country_raw), .groups = "drop")

  within_ctrls <- c("parent_ed_cwc")
  if (has_ses) within_ctrls <- c(within_ctrls, "ses_cwc")

  model_df %>%
    group_by(iso3c) %>%
    group_modify(function(.x, .y) {
      out <- fit_country_slope_within(.x, pv_names, within_ctrls)
      mutate(out, n_students = nrow(.x))
    }) %>%
    ungroup() %>%
    left_join(country_meta, by = "iso3c") %>%
    left_join(book_sd, by = "iso3c") %>%
    filter(
      is.finite(estimate),
      is.finite(std.error),
      n_students >= 500,
      std.error > 0
    )
}, force = rebuild_pirls)

stage2_data <- country_slopes_stage1 %>%
  mutate(inv_var_wt = 1 / std.error^2)

meta_reg_log <- lm(estimate ~ elec_deficit_log, data = stage2_data, weights = inv_var_wt)
meta_reg_log_summary <- summary(meta_reg_log)

meta_reg_raw <- lm(estimate ~ electrification, data = stage2_data, weights = inv_var_wt)
meta_reg_raw_summary <- summary(meta_reg_raw)

meta_reg_both <- lm(
  estimate ~ elec_deficit_log + sd_books_raw,
  data = stage2_data,
  weights = inv_var_wt
)
meta_reg_both_summary <- summary(meta_reg_both)

stage2_data$cooks_d <- cooks.distance(meta_reg_log)
stage2_data$influential <- stage2_data$cooks_d > 4 / nrow(stage2_data)

meta_reg_no_za <- lm(
  estimate ~ elec_deficit_log,
  data = stage2_data %>% filter(iso3c != "ZAF"),
  weights = inv_var_wt
)

tidy_meta <- function(m, label) {
  broom::tidy(m) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      cell = fmt_coef(estimate, std.error, p.value),
      model = label
    ) %>%
    select(term, model, cell)
}

meta_table_df <- bind_rows(
  tidy_meta(meta_reg_log, "WLS: elec. deficit (log)"),
  tidy_meta(meta_reg_raw, "WLS: electrification (raw)"),
  tidy_meta(meta_reg_both, "WLS: deficit + books SD"),
  tidy_meta(meta_reg_no_za, "WLS: deficit (excl. ZAF)")
) %>%
  mutate(term = dplyr::recode(
    term,
    "elec_deficit_log" = "Electrification deficit (log)",
    "electrification" = "Electrification (%)",
    "sd_books_raw" = "Within-country books SD"
  ))

r2_row <- tibble(
  term = "R²",
  `WLS: elec. deficit (log)` = sprintf("%.3f", meta_reg_log_summary$r.squared),
  `WLS: electrification (raw)` = sprintf("%.3f", meta_reg_raw_summary$r.squared),
  `WLS: deficit + books SD` = sprintf("%.3f", meta_reg_both_summary$r.squared),
  `WLS: deficit (excl. ZAF)` = sprintf("%.3f", summary(meta_reg_no_za)$r.squared)
)

n_row <- tibble(
  term = "Countries",
  `WLS: elec. deficit (log)` = as.character(nobs(meta_reg_log)),
  `WLS: electrification (raw)` = as.character(nobs(meta_reg_raw)),
  `WLS: deficit + books SD` = as.character(nobs(meta_reg_both)),
  `WLS: deficit (excl. ZAF)` = as.character(nobs(meta_reg_no_za))
)

meta_wide <- meta_table_df %>%
  pivot_wider(names_from = model, values_from = cell, values_fill = "—") %>%
  rename(Term = term) %>%
  bind_rows(rename(r2_row, Term = term)) %>%
  bind_rows(rename(n_row, Term = term))

table3 <- flextable(meta_wide) %>%
  style_ft(
    highlight_rows = which(meta_wide$Term %in% c("Electrification deficit (log)", "Electrification (%)")),
    italic_rows = which(meta_wide$Term %in% c("R²", "Countries")),
    font_size = 8.8
  ) %>%
  width(j = 1, width = 2.0) %>%
  width(j = 2:5, width = 1.16) %>%
  align(j = 1, align = "left", part = "body") %>%
  align(j = 2:5, align = "center", part = "body") %>%
  set_caption(
    "Table 3. Two-stage meta-regression: country-level books slopes on electrification\nStage 1 = within-country books slopes pooled across PVs; Stage 2 = inverse-variance WLS"
  ) %>%
  add_footer_lines(
    wrap_note(
      "Stage 1 slopes use within-country centered books_home with household controls and scaled weights, pooled across PV1–PV5 via Rubin's rules. Stage 2 uses inverse-variance weighting (1/SE²). Each country is one observation. The meta-regression R² indicates the proportion of between-country variance in the books slope explained by the moderator."
    )
  )

save_ft(table3, "p2_table3_metareg_jr_v20")

set.seed(42)

elec_levels <- c("Below 95%", "95 to <100%", "100%")
pal_elec_lines <- c(
  "Below 95%" = "#D55E00",
  "95 to <100%" = "#0072B2",
  "100%" = "#009E73"
)

plot_data <- stage2_data %>%
  mutate(
    electrification_plot = clean_electrification(electrification),
    elec_group = factor(
      pirls_elec_group_fun(iso3c, electrification_plot),
      levels = elec_levels
    ),
    elec_jittered = ifelse(
      electrification_plot == 100,
      100 - runif(n(), 0.0002, 0.003),
      electrification_plot
    ),
    label = case_when(
      iso3c %in% c("ZAF", "BRA", "TTO") ~ country_name,
      TRUE ~ ""
    )
  )

fitted_line <- tibble(
  electrification = seq(87.5, 100, by = 0.05),
  elec_deficit_log = log(100 - electrification + 0.1)
)

pred_ci <- predict(meta_reg_log, newdata = fitted_line, se.fit = TRUE)

fitted_line <- fitted_line %>%
  mutate(
    predicted = pred_ci$fit,
    lo = pred_ci$fit - 1.96 * pred_ci$se.fit,
    hi = pred_ci$fit + 1.96 * pred_ci$se.fit
  )

y_range <- quantile(stage2_data$estimate, c(0.02, 0.98), na.rm = TRUE)
y_pad <- diff(y_range) * 0.15
y_lims <- c(
  max(y_range[1] - y_pad, min(stage2_data$estimate) - 1),
  y_range[2] + y_pad + 2
)

fig1 <- ggplot() +
  geom_ribbon(
    data = fitted_line,
    aes(x = electrification, ymin = lo, ymax = hi),
    fill = "#DDDDDD",
    alpha = 0.7
  ) +
  geom_line(
    data = fitted_line,
    aes(x = electrification, y = predicted),
    colour = "#1F1F1F",
    linewidth = 1.1
  ) +
  geom_point(
    data = plot_data,
    aes(
      x = elec_jittered,
      y = estimate,
      size = 1 / std.error,
      fill = elec_group,
      colour = elec_group
    ),
    shape = 21,
    alpha = 0.82,
    stroke = 0.7
  ) +
  ggrepel::geom_text_repel(
    data = plot_data %>% filter(label != "", iso3c != "BRA"),
    aes(x = elec_jittered, y = estimate, label = label, colour = elec_group),
    size = 3.15,
    fontface = "italic",
    box.padding = 0.28,
    point.padding = 0.16,
    seed = 42,
    segment.colour = "#8C8C8C",
    segment.size = 0.3,
    min.segment.length = 0,
    show.legend = FALSE
  ) +
  ggrepel::geom_text_repel(
    data = plot_data %>% filter(iso3c == "BRA"),
    aes(x = elec_jittered, y = estimate, label = label, colour = elec_group),
    size = 3.15,
    fontface = "italic",
    nudge_x = -0.25,
    nudge_y = 0.65,
    direction = "both",
    hjust = 1,
    box.padding = 0.28,
    point.padding = 0.16,
    seed = 42,
    segment.colour = "#8C8C8C",
    segment.size = 0.3,
    min.segment.length = 0,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = pal_elec_lines,
    breaks = elec_levels,
    name = "Electrification group"
  ) +
  scale_fill_manual(
    values = pal_elec_lines,
    breaks = elec_levels,
    name = "Electrification group"
  ) +
  scale_size_continuous(range = c(1.4, 4.8), guide = "none") +
  scale_x_continuous(
    breaks = c(88, 90, 92, 94, 96, 98, 100),
    limits = c(87, 100.15),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20)) +
  coord_cartesian(ylim = c(y_lims[1], 20), clip = "off") +
  labs(
    title = "Countries with lower electrification show steeper books-at-home gradients",
    subtitle = wrap_note(
      paste0(
        "Each bubble = one country (N = ", nrow(stage2_data),
        "). Size proportional to precision (1/SE). ",
        "Line = inverse-variance WLS fit with 95% CI. ",
        "Countries at 100% are jittered slightly leftward for visibility."
      ),
      width = 100
    ),
    x = "National electrification rate (%)",
    y = "Within-country books slope\n(PIRLS points per books category)",
    caption = wrap_note(
      paste0(
        "Two-stage meta-regression. Stage 1: within-country Mundlak OLS slopes, PV-pooled via Rubin's rules. ",
        "Stage 2: inverse-variance WLS on log(100 - electrification + 0.1). ",
        "beta = ", sprintf("%.2f", coef(meta_reg_log)["elec_deficit_log"]),
        " (SE = ", sprintf("%.2f", meta_reg_log_summary$coefficients["elec_deficit_log", "Std. Error"]),
        "), p = ", format.pval(meta_reg_log_summary$coefficients["elec_deficit_log", "Pr(>|t|)"], digits = 3),
        ", R^2 = ", sprintf("%.3f", meta_reg_log_summary$r.squared), "."
      ),
      width = 118
    )
  ) +
  theme_journal(base_size = 11.5) +
  theme(
    plot.title = element_text(size = 15, face = "bold", lineheight = 1.03, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11.8, lineheight = 1.12, margin = margin(b = 32)),
    plot.caption = element_text(size = rel(0.74), lineheight = 1.16, margin = margin(t = 10)),
    plot.margin = margin(t = 18, r = 16, b = 14, l = 12),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.title.x = element_text(margin = margin(t = 7)),
    axis.line = element_line(colour = "#6F6F6F", linewidth = 0.45),
    axis.ticks = element_line(colour = "#6F6F6F", linewidth = 0.45),
    panel.border = element_rect(colour = "#7A7A7A", fill = NA, linewidth = 0.6),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  ) +
  guides(
    colour = guide_legend(
      override.aes = list(shape = 21, size = 3.2, alpha = 1, stroke = 0.8),
      order = 1
    ),
    fill = "none"
  )

save_plot(fig1, "p2_fig1_metareg_scatter", w = 8.5, h = 6.4)
