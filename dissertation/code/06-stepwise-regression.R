# stepwise regression ----

# initial model
sr_model_empty <- lm(y ~ 0, data = mr_train_df)

# reduce the model using stepwise regression
sr_model <- stepAIC(sr_model_empty,
  scope = list(upper = mr_model, lower = sr_model_empty),
  direction = "both",
  trace = FALSE
)

# output the results of the reduced model
sr_model %>%
  get_model_summary() %>%
  save_table(
    label = "stepwise-regression",
    caption = "The results of the model reduced using stepwise \\ac{AIC}."
  )

# predict from the stepwise regression model
sr_pred_df <- predict(sr_model, mr_train_df) %>%
  as.numeric() %>%
  tibble(
    x = mr_train_df$x,
    y = .,
    model = factor("sr", levels = model_types)
  )

# plot the fit
gg_sr_fit <- bind_rows(sr_pred_df, ml_pred_df) %>%
  ggplot(mapping = aes(x = x, y = y, colour = model)) +
  geom_line() +
  scale_colour_manual(values = model_colours) +
  cartesian_labels()
save_plot(plot = gg_sr_fit, filename = "stepwise-regression-fit")
