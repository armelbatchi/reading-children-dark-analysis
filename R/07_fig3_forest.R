source(file.path("R", "05_meta_regression_table3_fig1.R"))

pal_elec <- c(
  "Below 95%" = "#D55E00",
  "95 to <100%" = "#0072B2",
  "100%" = "#009E73",
  "Pooled" = "#000000"
)

ranked_main <- stage2_data %>%
  mutate(
    electrification = clean_electrification(electrification),
    elec_group = factor(
      pirls_elec_group_fun(iso3c, electrification),
      levels = c("Below 95%", "95 to <100%", "100%")
    )
  ) %>%
  arrange(estimate) %>%
  mutate(
    display_name = country_name,
    display_name = ifelse(
      duplicated(display_name) | duplicated(display_name, fromLast = TRUE),
      paste0(display_name, " (", iso3c, ")"),
      display_name
    ),
    sig = (conf.low > 0 | conf.high < 0),
    pooled_flag = FALSE
  )

overall_books_df <- pool_fixed_models(m3_list, terms = "books_cwc") %>%
  slice(1)

pooled_row <- tibble(
  country_name = NA_character_,
  iso3c = "Pooled",
  estimate = overall_books_df$estimate,
  conf.low = overall_books_df$conf.low,
  conf.high = overall_books_df$conf.high,
  elec_group = factor("Pooled", levels = c("Below 95%", "95 to <100%", "100%", "Pooled")),
  sig = (overall_books_df$conf.low > 0 | overall_books_df$conf.high < 0),
  pooled_flag = TRUE,
  display_name = "Pooled"
)

ranked_plot <- bind_rows(
  pooled_row,
  ranked_main %>%
    mutate(
      elec_group = factor(
        as.character(elec_group),
        levels = c("Below 95%", "95 to <100%", "100%", "Pooled")
      )
    )
) %>%
  mutate(
    row_id = seq_len(n()),
    elec_group = factor(
      elec_group,
      levels = c("Below 95%", "95 to <100%", "100%", "Pooled")
    )
  )

guide_df <- tibble(row_id = ranked_plot$row_id)

x_min <- min(c(ranked_plot$conf.low, 0), na.rm = TRUE)
x_max <- max(c(ranked_plot$conf.high, 0), na.rm = TRUE)
x_pad <- 0.08 * (x_max - x_min)

fig_forest <- ggplot(ranked_plot, aes(x = estimate, y = row_id)) +
  geom_hline(
    data = guide_df,
    aes(yintercept = row_id),
    inherit.aes = FALSE,
    colour = "grey80",
    linewidth = 0.25,
    alpha = 0.35
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.5,
    colour = "grey60"
  ) +
  geom_segment(
    data = dplyr::filter(ranked_plot, !pooled_flag),
    aes(x = conf.low, xend = conf.high, yend = row_id, colour = elec_group),
    linewidth = 1.05,
    alpha = 1,
    lineend = "round"
  ) +
  geom_segment(
    data = dplyr::filter(ranked_plot, pooled_flag),
    aes(x = conf.low, xend = conf.high, yend = row_id, colour = elec_group),
    linewidth = 1.25,
    alpha = 1,
    lineend = "round"
  ) +
  geom_point(
    data = dplyr::filter(ranked_plot, !pooled_flag),
    aes(colour = elec_group),
    shape = 21,
    fill = "white",
    size = 3.1,
    stroke = 0.95
  ) +
  geom_point(
    data = dplyr::filter(ranked_plot, !pooled_flag, sig),
    aes(fill = elec_group, colour = elec_group),
    shape = 21,
    size = 3.1,
    stroke = 0.95
  ) +
  geom_point(
    data = dplyr::filter(ranked_plot, pooled_flag),
    aes(colour = elec_group, fill = elec_group),
    shape = 21,
    size = 3.5,
    stroke = 1.0
  ) +
  scale_colour_manual(
    values = pal_elec,
    breaks = c("Below 95%", "95 to <100%", "100%"),
    name = "Electrification group"
  ) +
  scale_fill_manual(values = pal_elec, guide = "none") +
  scale_y_continuous(
    breaks = ranked_plot$row_id,
    labels = ranked_plot$display_name,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  coord_cartesian(
    xlim = c(x_min - x_pad, x_max + x_pad),
    clip = "off"
  ) +
  labs(
    title = "Within-country books slopes are universally positive",
    subtitle = wrap_note(
      paste0(
        "PV-pooled within-country OLS slopes with household controls (N = ",
        nrow(ranked_main),
        " countries). Countries are sorted by slope magnitude. ",
        "Filled dots indicate 95% confidence intervals excluding zero; open dots indicate non-significant estimates. ",
        "The pooled row reports the multilevel Model 3 coefficient."
      ),
      width = 110
    ),
    x = "Within-country books slope (PIRLS score points per category)",
    y = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "grey40", fill = NA, linewidth = 0.7),
    axis.line = element_line(colour = "grey35", linewidth = 0.4),
    axis.ticks = element_line(colour = "grey35", linewidth = 0.4),
    axis.text.y = element_text(size = 8.8),
    axis.text.x = element_text(size = 10),
    legend.position = "top",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10.5),
    plot.margin = margin(10, 20, 10, 10)
  ) +
  guides(
    colour = guide_legend(
      override.aes = list(
        shape = 21,
        fill = "white",
        size = 3.2,
        linewidth = 1
      )
    )
  )

save_plot(fig_forest, "p2_fig3_country_forest_pooled_journal_v23", w = 9.0, h = 12.0)
