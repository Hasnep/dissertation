# compare-coefs ----

# compare the coefficients of the regression models
bind_rows(
  mutate(get_model_summary(mr_model), model = "mr"),
  mutate(get_model_summary(sr_model), model = "sr"),
  mutate(lasso_coefs, model = "lasso")
) %>%
  select(Term, Estimate, model) %>%
  spread(key = model, value = Estimate) %>%
  arrange(match(Term, basis_functions_latex$latex)) %>%
  select(Term, mr, sr, lasso) %>%
  rename(
    "Term" = Term,
    "Multiple\nregression" = mr,
    "Stepwise\nregression" = sr,
    "LASSO" = lasso
  ) %>%
  set_names(linebreak(names(.), align = "r")) %>%
  save_table(
    label = "compare-coefs",
    caption = "The coefficients of the three linear regression models."
  )
