source(file.path("R", "02_data_prep.R"))

rhs_m1 <- paste(c("books_cwc", "books_cmean", all_controls), collapse = " + ")
rhs_m2 <- paste(c("books_cwc", "books_cmean", "elec_deficit_log", all_controls), collapse = " + ")
rhs_m3 <- paste(c("books_cwc * elec_deficit_log", "books_cmean", all_controls), collapse = " + ")
rhs_m4 <- paste(c("books_cwc * elec_deficit_log", "books_cwc_sq", "books_cmean", all_controls), collapse = " + ")

fit_pv <- function(rhs, stem, data = model_df) {
  load_or_build(stem, {
    lapply(pv_names, function(pv) {
      dat <- data %>% filter(is.finite(.data[[pv]]))
      ff <- as.formula(paste(pv, "~", rhs, "+ (1 + books_cwc | iso3c)"))
      fb <- as.formula(paste(pv, "~", rhs, "+ (1 | iso3c)"))
      fit_lmer_stable(ff, fb, dat)
    })
  }, force = rebuild_pirls)
}

m1_list <- fit_pv(rhs_m1, "m1_list")
m2_list <- fit_pv(rhs_m2, "m2_list")
m3_list <- fit_pv(rhs_m3, "m3_list")
m4_list <- fit_pv(rhs_m4, "m4_list")

table2_df <- load_or_build("table2_df", {
  bind_rows(
    pool_fixed_models(m1_list, c("books_cwc", "books_cmean")) %>% mutate(model = "Model 1"),
    pool_fixed_models(m2_list, c("books_cwc", "books_cmean", "elec_deficit_log")) %>% mutate(model = "Model 2"),
    pool_fixed_models(m3_list, c("books_cwc", "books_cmean", "elec_deficit_log", "books_cwc:elec_deficit_log")) %>%
      mutate(model = "Model 3"),
    pool_fixed_models(
      m4_list,
      c("books_cwc", "books_cmean", "elec_deficit_log", "books_cwc:elec_deficit_log", "books_cwc_sq")
    ) %>% mutate(model = "Model 4")
  ) %>%
    mutate(
      term = clean_term_labels(term),
      cell = fmt_coef(estimate, std.error, p.value)
    ) %>%
    select(term, model, cell)
}, force = rebuild_pirls)

fits <- lapply(list(m1_list, m2_list, m3_list, m4_list), compute_model_fit)

fit_row <- tibble(
  term = c("ICC", "Marginal R²", "Conditional R²"),
  `Model 1` = c(fits[[1]]$ICC, fits[[1]]$`Marginal R²`, fits[[1]]$`Conditional R²`),
  `Model 2` = c(fits[[2]]$ICC, fits[[2]]$`Marginal R²`, fits[[2]]$`Conditional R²`),
  `Model 3` = c(fits[[3]]$ICC, fits[[3]]$`Marginal R²`, fits[[3]]$`Conditional R²`),
  `Model 4` = c(fits[[4]]$ICC, fits[[4]]$`Marginal R²`, fits[[4]]$`Conditional R²`)
)

table2_wide <- table2_df %>%
  pivot_wider(names_from = model, values_from = cell, values_fill = "—") %>%
  rename(Term = term) %>%
  bind_rows(rename(fit_row, Term = term))

table2 <- flextable(table2_wide) %>%
  style_ft(
    highlight_rows = which(table2_wide$Term == "Books (within) × elec. deficit"),
    italic_rows = which(table2_wide$Term %in% c("ICC", "Marginal R²", "Conditional R²")),
    font_size = 8.8
  ) %>%
  width(j = 1, width = 2.05) %>%
  width(j = 2:5, width = 1.16) %>%
  align(j = 1, align = "left", part = "body") %>%
  align(j = 2:5, align = "center", part = "body") %>%
  set_caption(
    "Table 2. Plausible-value pooled multilevel models with Mundlak decomposition\nPooled fixed-effect estimates; within/between controls and cycle FE included"
  ) %>%
  add_footer_lines(
    wrap_note(
      "Rubin's pooling across PV1–PV5. Method A weight scaling (Rabe-Hesketh & Skrondal, 2006). Mundlak within/between centering. Electrification deficit = log(100 − electrification + 0.1). Random intercepts + random slope for books when non-singular."
    )
  )

save_ft(table2, "p2_table2_models_jr_v20")
table2
