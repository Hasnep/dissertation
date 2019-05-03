# set up ----
library(kableExtra) # for outputting LaTeX tables with kable()
library(MASS) # for stepwise regression
library(broom) # to tidy() model outputs
library(here) # to get relative file paths
library(glmnet) # for LASSO regularisation
library(mlegp) # for Gaussian process regression
library(tidyverse)
library(magrittr) # for pipe aliases e.g. use_series()
library(keras) # for ANNs

# set seed for R and Keras and disable other sources of randomness
use_session_with_seed(321)

source("01-helper-functions.R")

source("02-create-dataset.R")

# machine learning ----
load_neural_network <- TRUE
source("03-machine-learning.R")

# multiple regression ----
source("04-basis-functions.R")
source("05-multiple-regression.R")
source("06-stepwise-regression.R")
source("07-lasso.R")
source("08-compare-coefs.R")

# gaussian process ----
source("09-gaussian-process.R")

# regression and gp ----
source("10-regression-and-gp.R")
