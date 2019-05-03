# machine learning ----

# set up training variables
n_epochs <- 2000

# shuffle training dataset
ml_train_df <- arrange(ml_train_df, sample(ml_train_n))

# create a blank neural network model
blank_model <- function() {
  keras_model_sequential() %>%
    layer_dense(units = 10, kernel_initializer = "RandomNormal",
                activation = "tanh", input_shape = 1) %>%
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

model_filepath <- here("models", "neural-network.h5")
training_history_filepath <- here("models", "training-history.RData")
if (load_neural_network) {
  if (!file.exists(model_filepath)) {
    # error if file does not exist
    stop("Neural network file '", model_filepath, "' not found!")
  } else if (!file.exists(training_history_filepath)) {
    # error if file does not exist
    stop("Neural network file '", training_history_filepath, "' not found!")
  }else{
    # load the neural network from a file
    ml_model <- load_model_hdf5(model_filepath)
    load(training_history_filepath)
  }
} else {
  # fit the model
  message("Fitting the neural network")
  ml_model <- blank_model()
  ml_training_history <- fit(
    ml_model,
    x = ml_train_df$x,
    y = ml_train_df$y,
    batch_size = 32,
    epochs = n_epochs,
    shuffle = TRUE,
    verbose = 0
  )
  message("Neural network fitted")

  # save the neural network
  save_model_hdf5(ml_model, filepath = model_filepath)
  save(ml_training_history, file = training_history_filepath)

  # plot the training history
  gg_training_history <- tibble(
    epoch = seq(n_epochs),
    mse = ml_training_history$metrics$mean_squared_error
  ) %>%
    filter(epoch %% 10 == 0) %>%
    ggplot(mapping = aes(x = epoch, y = mse)) +
    geom_point(size = 2, stroke = 0) +
    expand_limits(y = 0) +
    labs(y = "mean squared error")
  save_plot(plot = gg_training_history, filename = "training-history")
}

# predict from the neural network
ml_pred_n <- 2^10
ml_pred_x <- seq(ml_train_limits[1], ml_train_limits[2], len = ml_pred_n)
ml_pred_y <- as.numeric(predict(ml_model, ml_pred_x))
ml_pred_df <- tibble(
  x = ml_pred_x,
  y = ml_pred_y,
  model = factor("nn", levels = model_types)
)

gg_nn_fit <- ggplot(data = ml_pred_df, mapping = aes(x, y, colour = model)) +
  stat_function(fun = my_function, n = ml_train_n, mapping = aes(colour = "truth")) +
  geom_line() +
  scale_colour_manual(values = model_colours) +
  cartesian_labels()
save_plot(plot = gg_nn_fit, filename = "nn-fit")

# also create a smaller dataset for fitting GPs
smaller_ratio <- 4
ml_pred_small_df <- filter(ml_pred_df, seq(ml_pred_n) %% smaller_ratio == 0)
