# multiple regression ----

# fit a linear model
mr_model <- lm(y ~ . + 1, data = mr_train_df)

# output the results of the linear model
mr_model %>%
  get_model_summary() %>%
  save_table(
    label = "multiple-regression",
    caption = "The results of the multiple regression model."
  )

# predict from the multiple regression model
mr_pred_df <- predict(mr_model, mr_train_df) %>%
  as.numeric() %>%
  tibble(
    x = mr_train_df$x,
    y = .,
    model = factor("mr", levels = model_types)
  )

# plot the fit
gg_mr_fit <- bind_rows(mr_pred_df, ml_pred_df) %>%
  ggplot(mapping = aes(x = x, y = y, colour = model)) +
  geom_line() +
  scale_colour_manual(values = model_colours) +
  cartesian_labels()
save_plot(plot = gg_mr_fit, filename = "multiple-regression-fit")
