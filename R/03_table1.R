source(file.path("R", "02_data_prep.R"))

table1_df <- load_or_build("table1_df", {
  bind_rows(
    pirls_analytic %>%
      mutate(elec_group = factor(elec_group, levels = c("Below 95%", "95 to <100%", "100%"))) %>%
      group_by(elec_group) %>%
      summarise(
        `Country-cycles` = n_distinct(interaction(iso3c, cycle, drop = TRUE)),
        Countries = n_distinct(iso3c),
        Students = n(),
        `Mean electrification (%)` = w_mean_safe(electrification, weight),
        `Weighted mean reading` = w_mean_safe(mean_read, weight),
        `Weighted mean books` = w_mean_safe(books_home, weight),
        .groups = "drop"
      ) %>%
      rename(`Electrification group` = elec_group),
    pirls_analytic %>%
      summarise(
        `Electrification group` = "Overall",
        `Country-cycles` = n_distinct(interaction(iso3c, cycle, drop = TRUE)),
        Countries = n_distinct(iso3c),
        Students = n(),
        `Mean electrification (%)` = w_mean_safe(electrification, weight),
        `Weighted mean reading` = w_mean_safe(mean_read, weight),
        `Weighted mean books` = w_mean_safe(books_home, weight)
      )
  )
}, force = rebuild_pirls)

table1_disp <- table1_df %>%
  mutate(
    `Mean electrification (%)` = sprintf("%.1f", `Mean electrification (%)`),
    `Weighted mean reading` = sprintf("%.1f", `Weighted mean reading`),
    `Weighted mean books` = sprintf("%.1f", `Weighted mean books`),
    `Country-cycles` = formatC(`Country-cycles`, format = "d", big.mark = ","),
    Countries = formatC(Countries, format = "d", big.mark = ","),
    Students = formatC(Students, format = "d", big.mark = ",")
  )

table1 <- flextable(table1_disp) %>%
  style_ft(
    highlight_rows = which(table1_disp$`Electrification group` == "Overall"),
    font_size = 9.5
  ) %>%
  width(j = 1, width = 1.55) %>%
  width(j = 2:3, width = 0.75) %>%
  width(j = 4, width = 0.95) %>%
  width(j = 5:7, width = 1.05) %>%
  align(j = 1, align = "left", part = "body") %>%
  align(j = 2:7, align = "center", part = "body") %>%
  set_caption(
    "Table 1. Analytic sample by electrification group\nWeighted descriptive statistics, PIRLS 2016/2021"
  ) %>%
  add_footer_lines(
    "Weighted means use the student weight. Country-cycles counts distinct country × cycle cells."
  )

save_ft(table1, "p2_table1_summary_jr_v20")
table1
