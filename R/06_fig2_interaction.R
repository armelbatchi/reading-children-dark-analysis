source(file.path("R", "05_meta_regression_table3_fig1.R"))

m3_pool <- load_or_build("model3_pool", {
  pool_vcov_models(m3_list)
}, force = rebuild_pirls)

elec_levels <- c("Below 95%", "95 to <100%", "100%")
pal_elec_lines <- c(
  "Below 95%" = "#D55E00",
  "95 to <100%" = "#0072B2",
  "100%" = "#009E73"
)
line_types_elec <- c(
  "Below 95%" = "solid",
  "95 to <100%" = "longdash",
  "100%" = "dotted"
)

ref_groups <- country_slopes_stage1 %>%
  distinct(iso3c, elec_group, electrification) %>%
  filter(!is.na(elec_group)) %>%
  mutate(
    elec_group = factor(as.character(elec_group), levels = elec_levels),
    electrification = clean_electrification(electrification)
  ) %>%
  group_by(elec_group) %>%
  summarise(
    electrification = clean_electrification(mean(electrification, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    elec_group = factor(as.character(elec_group), levels = elec_levels),
    elec_deficit_log = log(pmax(100 - electrification, 0) + 0.1),
    label = paste0(as.character(elec_group), " (", sprintf("%.1f", electrification), "%)")
  ) %>%
  arrange(elec_group)

label_map <- setNames(ref_groups$label, as.character(ref_groups$elec_group))

books_cwc_seq <- seq(-3, 4, by = 0.25)
needed <- names(m3_pool$coef)
beta <- m3_pool$coef[needed]
V <- m3_pool$vcov[needed, needed, drop = FALSE]

pred_grid <- bind_rows(lapply(seq_len(nrow(ref_groups)), function(i) {
  grp <- ref_groups[i, ]

  tmp <- tibble(
    elec_group = factor(as.character(grp$elec_group), levels = elec_levels),
    group_label = as.character(grp$label),
    books_cwc = books_cwc_seq,
    elec_deficit_log = grp$elec_deficit_log,
    books_cmean = 0,
    cycle_2021 = mean(model_df$cycle_2021, na.rm = TRUE),
    parent_ed_cwc = 0,
    parent_ed_cmean = 0,
    ses_cwc = 0,
    ses_cmean = 0
  )

  rhs_f <- as.formula(paste(
    "~ books_cwc * elec_deficit_log + books_cmean +",
    paste(
      c(
        "cycle_2021",
        "parent_ed_cwc",
        "parent_ed_cmean",
        if (has_ses) c("ses_cwc", "ses_cmean")
      ),
      collapse = " + "
    )
  ))

  X <- model.matrix(rhs_f, data = tmp)[, needed, drop = FALSE]
  ref_row <- tmp %>% filter(books_cwc == min(books_cwc))
  Xr <- model.matrix(rhs_f, data = ref_row)[, needed, drop = FALSE]
  D <- sweep(X, 2, Xr[1, ], "-")

  tmp$gain <- as.numeric(D %*% beta)
  tmp$se <- sqrt(pmax(diag(D %*% V %*% t(D)), 0))
  tmp$lo <- tmp$gain - 1.96 * tmp$se
  tmp$hi <- tmp$gain + 1.96 * tmp$se
  tmp
})) %>%
  mutate(elec_group = factor(as.character(elec_group), levels = elec_levels))

mean_books <- weighted.mean(pirls_analytic$books_home, pirls_analytic$weight, na.rm = TRUE)

pred_grid <- pred_grid %>%
  mutate(books_approx = books_cwc + mean_books) %>%
  filter(books_approx >= 1, books_approx <= 8) %>%
  mutate(elec_group = factor(as.character(elec_group), levels = elec_levels))

slope_check <- pred_grid %>%
  group_by(elec_group) %>%
  summarise(delta_gain = max(gain) - min(gain), .groups = "drop")

stopifnot(
  slope_check$delta_gain[match("Below 95%", slope_check$elec_group)] >
    slope_check$delta_gain[match("95 to <100%", slope_check$elec_group)],
  slope_check$delta_gain[match("95 to <100%", slope_check$elec_group)] >
    slope_check$delta_gain[match("100%", slope_check$elec_group)]
)

fig2 <- ggplot(
  pred_grid,
  aes(
    x = books_approx,
    y = gain,
    ymin = lo,
    ymax = hi,
    group = elec_group,
    colour = elec_group,
    linetype = elec_group,
    fill = elec_group
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.45, colour = "#6B6B6B") +
  geom_ribbon(alpha = 0.12, colour = NA, show.legend = FALSE) +
  geom_line(linewidth = 1.10) +
  scale_colour_manual(
    values = pal_elec_lines,
    breaks = elec_levels,
    labels = unname(label_map[elec_levels]),
    name = "Electrification level"
  ) +
  scale_fill_manual(
    values = pal_elec_lines,
    breaks = elec_levels,
    labels = unname(label_map[elec_levels]),
    guide = "none"
  ) +
  scale_linetype_manual(
    values = line_types_elec,
    breaks = elec_levels,
    labels = unname(label_map[elec_levels]),
    name = "Electrification level"
  ) +
  scale_x_continuous(breaks = 1:8, expand = expansion(mult = c(0.01, 0.02))) +
  labs(
    title = "The within-country books gradient is steeper in lower-electrification settings",
    subtitle = wrap_note(
      "Model-implied reading gains from multilevel Model 3 (Table 2). Shaded bands = 95% CI. Other covariates held at country means.",
      width = 100
    ),
    x = "Books at home (ordinal scale)",
    y = "Predicted gain in PIRLS reading score\n(relative to lowest books category)",
    caption = "Predictions from PV-pooled Model 3 with Mundlak within/between decomposition."
  ) +
  theme_journal(base_size = 11.5) +
  theme(
    legend.box = "vertical",
    legend.key.width = unit(2.4, "cm"),
    legend.text = element_text(size = 10),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.title.x = element_text(margin = margin(t = 7))
  ) +
  guides(
    colour = guide_legend(order = 1, override.aes = list(linewidth = 1.15)),
    linetype = guide_legend(order = 1)
  )

save_plot(fig2, "p2_fig2_interaction_journal_v22", w = 10, h = 5.8)
