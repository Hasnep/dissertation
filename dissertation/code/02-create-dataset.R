# create dataset ----

# create a nonlinear function
my_function <- function(x) {
  x + 5 * sin(x)
}

# create a training dataset
ml_train_n <- 32 * 8
ml_train_limits <- c(-15, 15)
ml_train_x <- seq(ml_train_limits[1], ml_train_limits[2], len = ml_train_n)

ml_truth_y <- my_function(ml_train_x)

ml_train_df <- tibble(
  x = ml_train_x,
  y = ml_truth_y + rnorm(ml_train_n, mean = 0, sd = 0.5),
  model = factor("train", levels = model_types)
)

# plot the dataset
gg_sin_x_dataset <- ggplot(data = ml_train_df, mapping = aes(x, y)) +
  geom_point(mapping = aes(colour = "train"), size = 0.25) +
  stat_function(fun = my_function, n = ml_train_n, mapping = aes(colour = "truth")) +
  scale_colour_manual(values = model_colours) +
  cartesian_labels()
save_plot(plot = gg_sin_x_dataset, filename = "sin-x-dataset")
