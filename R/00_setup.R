dir.create("output", showWarnings = FALSE, recursive = TRUE)
dir.create(file.path("output", "rds"), showWarnings = FALSE, recursive = TRUE)

required_packages <- c(
  "tidyverse", "lme4", "broom", "broom.mixed", "scales", "glue",
  "flextable", "officer", "ggrepel", "haven"
)

invisible(lapply(required_packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}))

suppressPackageStartupMessages({
  library(tidyverse)
  library(lme4)
  library(broom)
  library(broom.mixed)
  library(scales)
  library(glue)
  library(flextable)
  library(officer)
  library(ggrepel)
  library(haven)
})

cache_tag <- "journal_ready_v27"
rebuild_pirls <- FALSE
rebuild_pasec <- FALSE
rebuild_pasec_download <- FALSE

theme_journal <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(
        face = "bold", size = rel(1.28), colour = "#1F1F1F",
        margin = margin(b = 6)
      ),
      plot.subtitle = element_text(
        size = rel(0.96), colour = "#3A3A3A",
        lineheight = 1.15, margin = margin(b = 10)
      ),
      plot.caption = element_text(
        size = rel(0.76), colour = "#4A4A4A", hjust = 0,
        lineheight = 1.12, margin = margin(t = 8)
      ),
      axis.title = element_text(size = rel(0.98), colour = "#1F1F1F"),
      axis.text = element_text(size = rel(0.90), colour = "#1F1F1F"),
      panel.grid.major.x = element_line(colour = "#E2E2E2", linewidth = 0.35),
      panel.grid.major.y = element_line(colour = "#E6E6E6", linewidth = 0.35),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = element_text(size = rel(0.92), face = "bold"),
      legend.text = element_text(size = rel(0.86)),
      legend.margin = margin(t = 2, b = 2),
      legend.key.width = unit(1.4, "cm"),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(14, 16, 14, 14)
    )
}

pal <- list(
  g100 = "#222222",
  g95 = "#5C5C5C",
  g90 = "#A6A6A6",
  line = "#1F1F1F",
  ribbon = "#D9D9D9",
  accent = "#3F3F3F"
)

cache_name <- function(stem) paste0(stem, "_", cache_tag)
rds_file <- function(stem) file.path("output", "rds", paste0(cache_name(stem), ".rds"))

load_or_build <- function(stem, expr, force = FALSE, allow_empty = FALSE) {
  path <- rds_file(stem)
  if (!force && file.exists(path)) {
    obj <- readRDS(path)
    if (is.data.frame(obj) && !allow_empty && nrow(obj) == 0) {
      NULL
    } else {
      message("Loaded: ", path)
      return(obj)
    }
  }
  obj <- eval.parent(substitute(expr))
  saveRDS(obj, path)
  obj
}

save_plot <- function(p, stem, w, h, dpi = 400) {
  pdf_device <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
  ggsave(
    file.path("output", paste0(stem, ".pdf")),
    p,
    width = w,
    height = h,
    device = pdf_device,
    bg = "white"
  )
  ggsave(
    file.path("output", paste0(stem, ".png")),
    p,
    width = w,
    height = h,
    dpi = dpi,
    bg = "white"
  )
}

save_ft <- function(ft_obj, stem) {
  flextable::save_as_docx(
    "Table" = ft_obj,
    path = file.path("output", paste0(stem, ".docx"))
  )
}

wrap_note <- function(x, width = 120) stringr::str_wrap(x, width = width)

style_ft <- function(ft, highlight_rows = NULL, italic_rows = NULL, font_size = 9.2) {
  ft <- ft %>%
    theme_booktabs() %>%
    fontsize(size = font_size, part = "all") %>%
    font(fontname = "Arial", part = "all") %>%
    bold(part = "header") %>%
    color(color = "#1F1F1F", part = "all") %>%
    bg(bg = "#F3F3F3", part = "header") %>%
    border_inner_h(border = fp_border(color = "#D9D9D9", width = 0.6), part = "body") %>%
    border_outer(border = fp_border(color = "#AFAFAF", width = 0.9), part = "all") %>%
    padding(padding.top = 5, padding.bottom = 5, padding.left = 6, padding.right = 6, part = "all") %>%
    valign(valign = "center", part = "all") %>%
    line_spacing(space = 1.05, part = "all") %>%
    align(align = "center", part = "header")

  if (!is.null(highlight_rows)) {
    ft <- ft %>%
      bg(i = highlight_rows, bg = "#F7F7F7", part = "body") %>%
      bold(i = highlight_rows, bold = TRUE, part = "body")
  }

  if (!is.null(italic_rows)) {
    ft <- ft %>% italic(i = italic_rows, italic = TRUE, part = "body")
  }

  ft %>% autofit() %>% fit_to_width(max_width = 6.9)
}

find_existing_file <- function(candidates) {
  hits <- candidates[file.exists(candidates)]
  if (length(hits) > 0) return(hits[[1]])

  search_roots <- unique(c(
    ".", "..", getwd(), dirname(getwd()),
    if (dir.exists("data")) "data",
    if (dir.exists("output")) "output",
    if (dir.exists("output/rds")) "output/rds",
    if (dir.exists("/mnt/data")) "/mnt/data"
  ))

  rh <- unlist(lapply(search_roots, function(root) {
    if (!dir.exists(root)) return(character(0))
    c(
      list.files(
        root,
        pattern = "^p2_pirls_analytic.*\\.(csv|rds)$",
        recursive = TRUE,
        full.names = TRUE,
        ignore.case = TRUE
      ),
      list.files(
        root,
        pattern = "^analytic_data.*\\.rds$",
        recursive = TRUE,
        full.names = TRUE,
        ignore.case = TRUE
      )
    )
  }), use.names = FALSE)

  rh <- unique(rh[file.exists(rh)])
  if (length(rh) > 0) return(rh[[1]])

  stop("Could not find the prepared analytic dataset.")
}

read_prepared_analytic <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") return(readr::read_csv(path, show_col_types = FALSE))
  if (ext == "rds") return(readRDS(path))
  stop("Unsupported format: ", path)
}

get_analytic_data <- function() {
  if (exists("pirls_analytic", envir = .GlobalEnv, inherits = FALSE)) {
    obj <- get("pirls_analytic", envir = .GlobalEnv)
    if (is.data.frame(obj) && nrow(obj) > 0) return(obj)
  }

  if (exists("out", envir = .GlobalEnv, inherits = FALSE)) {
    obj <- get("out", envir = .GlobalEnv)
    ok <- is.data.frame(obj) &&
      nrow(obj) > 0 &&
      all(c("iso3c", "weight", "electrification") %in% names(obj))
    if (ok) return(obj)
  }

  read_prepared_analytic(find_existing_file(c(
    paste0("output/p2_pirls_analytic_v", 10:5, ".csv"),
    "output/p2_pirls_analytic.csv",
    paste0("p2_pirls_analytic_v", 10:5, ".csv"),
    "p2_pirls_analytic.csv",
    paste0("/mnt/data/p2_pirls_analytic_v", 10:5, ".csv"),
    paste0("output/rds/p2_pirls_analytic_v", 10:5, ".rds"),
    "output/rds/p2_pirls_analytic.rds"
  )))
}

set_flextable_defaults(
  split = TRUE,
  table_align = "center",
  keep_with_next = TRUE,
  theme_fun = theme_booktabs
)

