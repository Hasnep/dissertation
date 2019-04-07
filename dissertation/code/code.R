# set up
library(tidyverse)
library(keras)
use_python("C:/Users/Hannes/Miniconda3/envs/r-tensorflow")
set.seed(123)

# set up ggplot
theme_set(theme_minimal() + theme(legend.position = "none"))
gg_width <- 20
gg_height <- gg_width / 1.6
gg_units <- "cm"

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
  y = ml_truth_y + rnorm(ml_train_n, mean = 0, sd = 0.5)
)

# plot the dataset
gg_sin_x_dataset <- ggplot(data = ml_train_df, mapping = aes(x, y)) +
  stat_function(fun = my_function, n = ml_train_n, mapping = aes(colour = "truth"), size = 1) +
  geom_point(mapping = aes(colour = "train"), size = 0.5) +
  labs(x = "$x$", y = "$y$")
ggsave(plot = gg_sin_x_dataset, filename = "sin-x-dataset.svg", path = "figures", width = gg_width, height = gg_height, units = gg_units)

# create a blank neural network model
blank_model <- function() {
  keras_model_sequential() %>%
    layer_dense(units = 10, kernel_initializer = "RandomNormal", activation = "tanh", input_shape = 1) %>%
    layer_dense(units = 10, activation = "tanh") %>%
    layer_dense(units = 10, activation = "tanh") %>%
    layer_dense(units = 10, activation = "tanh") %>%
    layer_dense(units = 10, activation = "tanh") %>%
    layer_dense(units = 10, activation = "tanh") %>%
    layer_dense(units = 10, activation = "tanh") %>%
    layer_dense(units = 10, activation = "tanh") %>%
    layer_dense(units = 1, activation = "linear") %>%
    compile(
      loss = "mse",
      optimizer = optimizer_rmsprop(),
      metrics = list("mean_squared_error")
    )
}

# set up training variables
ml_test_n <- 500
ml_test_x <- seq(ml_train_limits[1], ml_train_limits[2], len = ml_test_n)
n_epochs <- 1000

# prallocate a df for comapring
orders <- c("in_order", "reversed", "shuffled")
ml_compare_df <- tibble(
  x = ml_test_x,
  in_order = 0,
  reversed = 0,
  shuffled = 0
)


for (order_as in orders) {
  # fit the model
  ml_model <- blank_model()
  training_history <- fit(
    ml_model,
    x = ml_train_df$x,
    y = ml_train_df$y,
    epochs = n_epochs,
    validation_split = 0.2,
    shuffle = TRUE,
    verbose = 0
  )

  # predict from model
  predictions <- as.numeric(predict(ml_model, ml_test_x))
  ml_compare_df <- mutate(ml_compare_df, !!order_as := predictions)
}

gg_compare <- ggplot()


ggsave(plot = gg_compare_in_order, filename = "compare-in-order.svg", path = "figures", width = gg_width, height = gg_height, units = gg_units)
ggsave(plot = gg_compare_reversed, filename = "compare-reversed.svg", path = "figures", width = gg_width, height = gg_height, units = gg_units)
ggsave(plot = gg_compare_shuffled, filename = "compare-shuffled.svg", path = "figures", width = gg_width, height = gg_height, units = gg_units)
