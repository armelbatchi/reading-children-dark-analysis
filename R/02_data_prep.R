source(file.path("R", "01_helpers.R"))

pirls_analytic <- load_or_build("analytic_data", {
  get_analytic_data() %>%
    mutate(
      cycle = as.integer(cycle),
      cycle_2021 = ifelse(cycle == 2021, 1, 0),
      books_home = suppressWarnings(as.numeric(books_home)),
      parent_ed = suppressWarnings(as.numeric(parent_ed)),
      ses_index = suppressWarnings(as.numeric(ses_index)),
      across(starts_with("read_pv"), ~ suppressWarnings(as.numeric(.x))),
      mean_read = rowMeans(as.data.frame(select(., starts_with("read_pv"))), na.rm = TRUE),
      electrification = clean_electrification(electrification),
      elec_group = pirls_elec_group_fun(iso3c, electrification)
    ) %>%
    filter(
      is.finite(weight),
      weight > 0,
      is.finite(electrification),
      !is.na(iso3c)
    )
}, force = FALSE)

stopifnot(nrow(pirls_analytic) > 0)

pv_names <- paste0("read_pv", 1:5)

model_df <- load_or_build("model_data", {
  has_ses_raw <- "ses_index" %in% names(pirls_analytic) &&
    any(is.finite(pirls_analytic$ses_index))

  df <- pirls_analytic %>%
    mutate(
      elec_deficit = pmax(100 - electrification, 0),
      elec_deficit_log = log(elec_deficit + 0.1)
    )

  df <- df %>%
    group_by(iso3c, cycle) %>%
    mutate(
      n_cluster = n(),
      sum_wt = sum(weight),
      wt_scaled = weight * n_cluster / sum_wt
    ) %>%
    ungroup()

  cmeans <- df %>%
    group_by(iso3c) %>%
    summarise(
      books_cmean = weighted.mean(books_home, weight, na.rm = TRUE),
      parent_ed_cmean = weighted.mean(parent_ed, weight, na.rm = TRUE),
      ses_cmean = if (has_ses_raw) weighted.mean(ses_index, weight, na.rm = TRUE) else NA_real_,
      sd_books_country_raw = sqrt(
        sum(
          weight * (books_home - weighted.mean(books_home, weight, na.rm = TRUE))^2,
          na.rm = TRUE
        ) / sum(weight, na.rm = TRUE)
      ),
      .groups = "drop"
    )

  df <- df %>%
    left_join(cmeans, by = "iso3c") %>%
    mutate(
      books_cwc = books_home - books_cmean,
      parent_ed_cwc = parent_ed - parent_ed_cmean,
      ses_cwc = if (has_ses_raw) ses_index - ses_cmean else NA_real_,
      books_cmean = books_cmean - weighted.mean(books_cmean, n_cluster, na.rm = TRUE),
      parent_ed_cmean = parent_ed_cmean - weighted.mean(parent_ed_cmean, n_cluster, na.rm = TRUE),
      books_cwc_sq = books_cwc^2,
      sd_books_cwc_country = sd_books_country_raw -
        weighted.mean(sd_books_country_raw, n_cluster, na.rm = TRUE)
    )

  if (has_ses_raw) {
    df <- df %>%
      mutate(ses_cmean = ses_cmean - weighted.mean(ses_cmean, n_cluster, na.rm = TRUE))
  }

  df %>%
    filter(
      is.finite(wt_scaled),
      wt_scaled > 0,
      is.finite(books_cwc),
      is.finite(books_cmean),
      is.finite(elec_deficit_log),
      is.finite(sd_books_cwc_country),
      is.finite(parent_ed_cwc),
      is.finite(parent_ed_cmean),
      !is.na(iso3c)
    )
}, force = rebuild_pirls)

has_ses <- "ses_cwc" %in% names(model_df) && any(is.finite(model_df$ses_cwc))

if (has_ses) {
  model_df <- model_df %>%
    filter(is.finite(ses_cwc), is.finite(ses_cmean))
}

stopifnot(nrow(model_df) > 0)

within_controls <- c("cycle_2021", "parent_ed_cwc")
between_controls <- c("parent_ed_cmean")

if (has_ses) {
  within_controls <- c(within_controls, "ses_cwc")
  between_controls <- c(between_controls, "ses_cmean")
}

all_controls <- c(within_controls, between_controls)
