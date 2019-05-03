# regression and gp ----

# predict from lasso model
lasso_pred_small_y <- mr_train_small_df %>%
  select(-y) %>%
  as.matrix() %>%
  predict(lasso_cv, newx = ., s = "lambda.1se")

# calculate residuals
lasso_resid <- ml_pred_small_df$y - lasso_pred_small_y

lasso_resid_df <- tibble(
  x = ml_pred_small_df$x,
  y = lasso_resid,
  model = "residual"
)

# fit a gaussian process model
gp_resid_model <- mlegp(X = mr_train_small_df$x, Z = lasso_resid)

# predict from the GP
gp_resid_pred <- predict(gp_resid_model, newData = matrix(mr_train_small_df$x)) %>%
  as.numeric()

combined_pred_df <- tibble(
  x = ml_pred_small_df$x,
  lasso = lasso_pred_small_y,
  gp = gp_resid_pred,
  y = lasso + gp,
  model = factor("combined", levels = model_types)
)

# plot the GP fitting the residuals
gg_gp_resids_fit <- ggplot(data = combined_pred_df, mapping = aes(x, y = gp)) +
  geom_point() +
  geom_line() +
  scale_colour_manual(values = model_colours) +
  labs(x = "$x$", y = "residuals")
save_plot(plot = gg_gp_resids_fit, filename = "gp-resids-fit")

# plot the fit
gg_combined_fit <- bind_rows(combined_pred_df, ml_pred_df) %>%
  ggplot(mapping = aes(x, y, colour = model)) +
  geom_line() +
  scale_colour_manual(values = model_colours) +
  cartesian_labels()
save_plot(plot = gg_combined_fit, filename = "combined-fit")
