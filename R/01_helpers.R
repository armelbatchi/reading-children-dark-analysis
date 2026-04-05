source(file.path("R", "00_setup.R"))

w_mean_safe <- function(x, w) {
  if (all(is.na(x))) return(NA_real_)
  weighted.mean(x, w, na.rm = TRUE)
}

pool_scalar <- function(q, u) {
  m <- length(q)
  qbar <- mean(q, na.rm = TRUE)
  ubar <- mean(u, na.rm = TRUE)
  b <- if (m > 1) stats::var(q, na.rm = TRUE) else 0
  tvar <- ubar + (1 + 1 / m) * b

  tibble(
    estimate = qbar,
    std.error = sqrt(tvar),
    conf.low = qbar - 1.96 * sqrt(tvar),
    conf.high = qbar + 1.96 * sqrt(tvar),
    p.value = 2 * pnorm(abs(qbar / sqrt(tvar)), lower.tail = FALSE)
  )
}

pool_fixed_models <- function(ml, terms = NULL) {
  tidies <- lapply(ml, function(m) broom.mixed::tidy(m, effects = "fixed"))
  if (!is.null(terms)) {
    tidies <- lapply(tidies, function(d) d %>% filter(term %in% terms))
  }

  tn <- unique(unlist(lapply(tidies, function(d) d$term)))

  bind_rows(lapply(tn, function(ti) {
    q <- sapply(tidies, function(d) d$estimate[d$term == ti][1])
    se <- sapply(tidies, function(d) d$std.error[d$term == ti][1])
    mutate(pool_scalar(q, se^2), term = ti, .before = 1)
  }))
}

pool_vcov_models <- function(ml) {
  cl <- lapply(ml, fixef)
  ct <- Reduce(intersect, lapply(cl, names))
  q_mat <- do.call(rbind, lapply(cl, function(b) b[ct]))
  qbar <- colMeans(q_mat)

  ubar <- Reduce("+", lapply(ml, function(m) {
    as.matrix(vcov(m))[ct, ct, drop = FALSE]
  })) / nrow(q_mat)

  bmat <- if (nrow(q_mat) > 1) {
    stats::cov(q_mat)
  } else {
    matrix(0, length(ct), length(ct), dimnames = list(ct, ct))
  }

  list(coef = qbar, vcov = ubar + (1 + 1 / nrow(q_mat)) * bmat)
}

fit_lmer_stable <- function(ff, fb, data, wt_col = "wt_scaled") {
  ctrl <- lme4::lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 200000),
    calc.derivs = FALSE,
    check.rankX = "message+drop.cols",
    check.conv.singular = "ignore"
  )

  data$.wt <- data[[wt_col]]

  fit <- try(
    lme4::lmer(
      ff,
      data = data,
      weights = .wt,
      REML = FALSE,
      na.action = na.omit,
      control = ctrl
    ),
    silent = TRUE
  )

  if (inherits(fit, "try-error") || lme4::isSingular(fit, tol = 1e-4)) {
    fit <- lme4::lmer(
      fb,
      data = data,
      weights = .wt,
      REML = FALSE,
      na.action = na.omit,
      control = ctrl
    )
  }

  fit
}

fit_country_slope_within <- function(df, pv_names, controls) {
  rhs <- paste(c("books_cwc", controls), collapse = " + ")

  fits <- lapply(pv_names, function(pv) {
    try(
      lm(as.formula(paste(pv, "~", rhs)), data = df, weights = wt_scaled),
      silent = TRUE
    )
  })

  fits <- Filter(function(x) !inherits(x, "try-error"), fits)

  if (length(fits) == 0) {
    return(tibble(
      estimate = NA_real_,
      std.error = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_
    ))
  }

  tidies <- lapply(fits, broom::tidy)
  q <- sapply(tidies, function(d) d$estimate[d$term == "books_cwc"][1])
  se <- sapply(tidies, function(d) d$std.error[d$term == "books_cwc"][1])
  pool_scalar(q, se^2)
}

fmt_coef <- function(e, s, p) {
  st <- ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", "")))
  paste0(sprintf("%.2f", e), st, " (", sprintf("%.2f", s), ")")
}

clean_term_labels <- function(x) {
  dplyr::recode(
    x,
    "(Intercept)" = "Intercept",
    "books_cwc" = "Books at home (within)",
    "books_cmean" = "Books at home (between)",
    "elec_deficit_log" = "Electrification deficit (log)",
    "books_cwc:elec_deficit_log" = "Books (within) × elec. deficit",
    "books_cwc_sq" = "Books (within)²",
    "books_cwc:sd_books_cwc_country" = "Books (within) × within-country books SD",
    "sd_books_cwc_country" = "Within-country books SD",
    "cycle_2021" = "Cycle 2021",
    "parent_ed_cwc" = "Parent education (within)",
    "parent_ed_cmean" = "Parent education (between)",
    "ses_cwc" = "SES index (within)",
    "ses_cmean" = "SES index (between)",
    .default = x
  )
}

clean_electrification <- function(x, tol = 1e-8) {
  x <- suppressWarnings(as.numeric(x))
  x[is.finite(x) & abs(x - 100) < tol] <- 100
  x
}

elec_group_fun <- function(x) {
  x <- clean_electrification(x)
  case_when(
    x < 95 ~ "Below 95%",
    x < 100 ~ "95 to <100%",
    TRUE ~ "100%"
  )
}

pirls_elec_group_fun <- function(iso3c, x) {
  x <- clean_electrification(x)
  case_when(
    toupper(as.character(iso3c)) == "ZAF" ~ "Below 95%",
    toupper(as.character(iso3c)) %in% c("BRA", "TTO") ~ "95 to <100%",
    is.finite(x) & x < 95 ~ "Below 95%",
    is.finite(x) & x < 100 ~ "95 to <100%",
    TRUE ~ "100%"
  )
}

compute_model_fit <- function(ml) {
  fs <- lapply(ml, function(m) {
    vc <- as.data.frame(VarCorr(m))
    tau2 <- sum(vc$vcov[vc$grp != "Residual"])
    sig2 <- vc$vcov[vc$grp == "Residual"][1]
    vf <- var(as.numeric(model.matrix(m) %*% fixef(m)))

    list(
      icc = tau2 / (tau2 + sig2),
      r2m = vf / (vf + tau2 + sig2),
      r2c = (vf + tau2) / (vf + tau2 + sig2)
    )
  })

  tibble(
    ICC = sprintf("%.3f", mean(sapply(fs, `[[`, "icc"))),
    `Marginal R²` = sprintf("%.3f", mean(sapply(fs, `[[`, "r2m"))),
    `Conditional R²` = sprintf("%.3f", mean(sapply(fs, `[[`, "r2c")))
  )
}
