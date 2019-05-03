# create a function to make a string into LaTeX inline math mode
math_mode <- function(x, format = "") {
  if (format == "$") {
    paste0("$", x, "$")
  } else {
    paste0("\\(", x, "\\)")
  }
}

path_figures <- here("figures")
golden <- (1 + sqrt(5)) / 2
save_plot <- function(plot = last_plot(), filename, w = 10, aspect_ratio = golden) {
  ggsave(
    plot = plot,
    filename = paste0(filename, ".svg"),
    path = path_figures,
    width = w,
    height = w / aspect_ratio,
    units = "cm"
  )
}

# set up tables
path_tables <- here("tables")
get_model_summary <- function(model) {
  column_names <- list(
    "Term" = "latex",
    "Estimate" = "estimate",
    "Std. err." = "std.error",
    "\\(t\\)-value" = "statistic",
    "\\(p\\)-value" = "p.value"
  )
  model %>%
    tidy() %>%
    inner_join(basis_functions_latex, by = c("term" = "name")) %>%
    select(!!!column_names)
}

options(knitr.kable.NA = "") # remove NAs from exported tables

# create a function to format numbers in scientific notation
scientific <- function(x) {
  p <- floor(log10(abs(x)))
  if (abs(p) > getOption("scipen")) {
    b <- round(x / 10^p, 3)
    return(math_mode(paste0(b, " \\times 10^{", p, "}")))
  } else {
    return(as.character(x))
  }
}

save_table <- function(table, label = NA, caption = NA, digits = 3) {
  table %>%
    kable(
      format = "latex",
      label = label,
      caption = caption,
      digits = digits,
      linesep = "",
      escape = FALSE,
      booktabs = TRUE
    ) %>%
    kable_styling(latex_options = "hold_position") %>%
    cat(file = paste0(path_tables, label, ".tex"))
}

# set up ggplot
theme_set(
  theme_minimal() +
    theme(
      axis.title.x = element_text(margin = margin(t = 10)),
      legend.position = "none"
    )
)

# create a function to add cartesian axis labels
cartesian_labels <- function() {
  labs(x = "$x$", y = "$y$")
}

# create a custom colour scheme
unnamed_colours <- c(
  "#000000",
  "#0072B2",
  "#D95F02",
  "#66A61E",
  "#E69F00",
  "#E7298A",
  "#56B4E9",
  "#F0E442",
  "#666666"
)

model_colours <- c(
  train = "#000000", # black
  truth = "#0072B2", # blue
  nn = "#E69F00", # yellow
  mr = "#D95F02", # orange
  sr = "#66A61E", # green
  lasso = "#666666", # grey
  gp = "#E7298A", # pink
  combined = "#56B4E9" # light blue
)

model_types <- names(model_colours)
