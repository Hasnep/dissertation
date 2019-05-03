# gaussian process ----

# fit a gaussian process model
gp_model <- mlegp(X = ml_pred_small_df$x, Z = ml_pred_small_df$y)

# predict from the GP inclusing standard errors
confidence_level <- 0.99
gp_pred_df <- predict(gp_model, newData = matrix(ml_pred_x), se.fit = TRUE) %>%
  map(as.numeric) %>%
  as_tibble() %>%
  rename(y = fit, se = se.fit) %>%
  mutate(
    x = ml_pred_x,
    y_lwr = y - qnorm((1 - confidence_level) / 2) * se,
    y_upr = y + qnorm((1 + confidence_level) / 2) * se,
    model = factor("gp", levels = model_types)
  )

gg_gp_fit <- gp_pred_df %>%
  bind_rows(ml_pred_df) %>%
  ggplot(mapping = aes(x, y, ymin = y_lwr, ymax = y_upr, colour = model, fill = model)) +
  geom_ribbon(linetype = 0,alpha=0.5) +
  geom_line() +
  scale_colour_manual(values = model_colours) +
  cartesian_labels()
save_plot(plot = gg_gp_fit, filename = "gp-fit")
