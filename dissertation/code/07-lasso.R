# lasso ----

# reduce the model using lasso regularisation
lasso_model <- mr_train_df %>%
  select(-y) %>%
  as.matrix() %>%
  glmnet(x = .,
         y = mr_train_df$y,
         family = "gaussian",
         lambda = exp(seq(2, -3, len = 100)))

# perform leave-one-out cross validation to optimise lambda
lasso_cv <- cv.glmnet(
  x = as.matrix(select(mr_train_df, -y)),
  y = mr_train_df$y,
  lambda = exp(seq(2, -3, len = 50)),
  nfolds = ml_pred_n,
  grouped = FALSE
)

# plot changing lambda over time
gg_lasso_lambda <- lasso_model$beta %>%
  as.matrix() %>%
  t() %>%
  as_tibble() %>%
  mutate(`(Intercept)` = as.numeric(lasso_model$a0)) %>%
  mutate_all(~ replace(., abs(.) < 1e-4, NA)) %>% # remove when estimate is zero
  mutate(log_lambda = log(lasso_model$lambda)) %>%
  gather(key = "term", value = "estimate", -log_lambda, na.rm = TRUE) %>%
  inner_join(basis_functions_latex, by = c("term" = "name")) %>%
  mutate(Term = latex_old) %>%
  arrange(match(Term, basis_functions_latex$latex_old)) %>% # sort terms
  ggplot(mapping = aes(x = log_lambda, y = estimate, colour = Term)) +
  geom_vline(xintercept = log(lasso_cv$lambda.1se),
             linetype = "dashed",
             colour = "red",
             alpha = 0.5) +
  geom_line() +
  scale_y_continuous(limits = c(NA, 5)) +
  scale_colour_manual(values = unnamed_colours) +
  labs(x = math_mode("\\log(\\lambda)", "$")) +
  theme(legend.position = "bottom")
save_plot(plot = gg_lasso_lambda,
          filename = "lasso-lambda",
          aspect_ratio = 0.8 * golden)

# extract the optimised coefficients
lasso_coefs <- lasso_cv %>%
  coef(s = "lambda.1se") %>%
  as.matrix() %>%
  as_tibble(rownames = "term") %>%
  rename(estimate = `1`) %>%
  filter(abs(estimate) > 1e-4) %>%
  inner_join(basis_functions_latex, by = c("term" = "name")) %>%
  select(term = latex, estimate) %>%
  set_names(str_to_sentence(names(.)))

# save the coefficients as a table
save_table(lasso_coefs,
  label = "lasso-coefs",
  caption = "The optimised coefficients using leave-one-out cross-validation on the value of \\(\\lambda\\)."
)

# predict from the lasso model
lasso_pred_y <- mr_train_df %>%
  select(-y) %>%
  as.matrix() %>%
  predict(lasso_cv, newx = ., s = "lambda.1se") %>%
  as.numeric()

lasso_pred_df <- tibble(
  x = mr_train_df$x,
  y = lasso_pred_y,
  model = factor("lasso", levels = model_types)
)

# plot the fit
gg_lasso_fit <- bind_rows(lasso_pred_df, ml_pred_df) %>%
  ggplot(mapping = aes(x = x, y = y, colour = model)) +
  geom_line() +
  scale_colour_manual(values = model_colours) +
  cartesian_labels()
save_plot(plot = gg_lasso_fit, filename = "lasso-fit")
