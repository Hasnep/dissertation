# basis-functions ----

# create a list of basis functions
basis_functions <- list(
  "x" = ~.,
  "x_squared" = ~ .^2,
  "sin_x" = ~ sin(.),
  "sin_2x" = ~ sin(2 * .),
  "sin_half_x" = ~ sin(. / 2),
  "cos_x" = ~ cos(.),
  "cos_2x" = ~ cos(2 * .),
  "cos_half_x" = ~ cos(. / 2)
)

# create a lookup table to help export tables to LaTeX using knitr::kable()
basis_functions_latex <- tibble(
  name = c("x", "x_squared", "sin_x", "sin_2x", "sin_half_x",
           "cos_x", "cos_2x", "cos_half_x"),
  latex = c("x", "x^2", "\\sin(x)", "\\sin(2x)", "\\sin(x/2)",
            "\\cos(x)", "\\cos(2x)", "\\cos(x/2)")
) %>%
  mutate(
    latex_old = math_mode(latex, "$"),
    latex = math_mode(latex)
  ) %>%
  bind_rows(c(name = "(Intercept)", latex = "Intercept", latex_old = "Intercept"), .)

# create a dataframe of the pedictors for regression
mr_train_df <- map_df(basis_functions, ~ map_dbl(ml_pred_x, .)) %>%
  mutate(y = ml_pred_y)

# also create a smaller dataframe of the pedictors for use with GPs
mr_train_small_df <- filter(mr_train_df, seq(ml_pred_n) %% smaller_ratio == 0)
