# Load necessary libraries
packages <- c( "dplyr",
               "quantmod",
               "glmnet",  
               "here",
               "MASS",
               "foreach",
               "progress")

# Check if packages are installed
packages_to_install <- packages[!packages %in% installed.packages()[,"Package"]]

# Install packages if not already installed
if(length(packages_to_install)) install.packages(packages_to_install, repos = "https://cloud.r-project.org/")

# Load packages
lapply(packages, require, character.only = TRUE)

#####----------------------------------------------------------------------#####


# Set the working directory to the project root using here()
setwd(here())

# Read the CSV file using here() to construct the file path
df <- read.csv(here("portfolio.data.csv"))
# View(df)

#####----------------------------------------------------------------------#####

#Mutating data - dates
df$X <- as.Date(df$X, format = "%Y%m%d")
df$X <- format(df$X, "%Y-%m-%d")

View(df)
# The data contains four tables. We only need the first one.
# # Subset the dataframe from the beginning to 25566 exclusive. 2022-12-30 is the 25399th row as in the project description.
# df <- df[1:25566, ]

#Sample testing, only first 260 observations
df <- df[1:260, ]
# View(df)

#####----------------------------------------------------------------------#####

#Formating the Return Matrix, excluding the first column (date)
returnMatrix <- df[,-1]
returnMatrix <- as.matrix(returnMatrix)
dim(returnMatrix) #Now the dimension: is 25566 x 100

#Store Numbers of Observations and Assets (for dimensions of matrices)
num_day <- dim(returnMatrix)[1]
num_asset <- dim(returnMatrix)[2]

#Demean the return matrix (Demean outside the for loop or inside? This is outside). 
demeaned_return <- scale(as.numeric(returnMatrix), center = TRUE, scale = FALSE)
demeaned_return <- matrix(demeaned_return, nrow = num_day, byrow = TRUE)
#dim(demeaned_return) --> the same dimension as the original matrix

#Generate the equally-weighted portfolio wE (dimension: 100x1)
eqWeight <- 1/num_asset
wEW <- matrix(eqWeight, nrow = num_asset, ncol = 1)

#Set up matrix N (dimension N 100 x 99)
N_matrix <- diag(99)
N_matrix <- rbind(N_matrix, rep(-1,99))

#Setting window size = 252 days (according to the project)
window_size <- 252
num_windows <- num_day - window_size +1 

#####----------------------------------------------------------------------#####


# Function to calculate betas using Lasso or Ridge regression
calculate_betas <- function(X, y, alpha) {
  # Fit Lasso or Ridge regression using glmnet
  model <- cv.glmnet(x = X, y = y, alpha = alpha)
  best_lambda <- model$lambda.min  # Get the lambda that minimizes CV error
  betas <- coef(model, s = "lambda.min")[-1]  # Exclude the intercept (the first row)
  return(betas)
}

# Function to calculate portfolio weights from betas
calculate_portfolio_weights <- function(betas, N, we) {
  w_portfolio <- we - N %*% betas
  return(w_portfolio)
}

#####----------------------------------------------------------------------#####

# Create an empty object for OLS model
OLS <- NULL

# Set the number of cores to use (adjust this based on your system)
num_cores <- 4

# Initialize progress bar
pb <- progress_bar$new(total = num_windows, format = "[:bar] :percent :elapsed ETA: :eta", clear = FALSE)

# Create a parallelized foreach loop with progress bar
results <- foreach(i = 1:num_windows, .packages = c("MASS", "glmnet", "quantmod"), .combine = 'list') %dopar% {
  pb$tick()  # Increment progress bar
  
  # Extract data from the demeaned return matrix for the current window
  window_R <- demeaned_return[i:(i + window_size - 1), , drop = FALSE]
  Y_matrix <- window_R %*% wEW
  X_matrix <- window_R %*% N_matrix
  
  OLS <- lm(Y_matrix ~ 0 + X_matrix)
  beta <- coef(OLS)
  
  w_matrix <- wEW - (N_matrix %*% beta)
  
  alpha_lasso <- 1
  alpha_ridge <- 0
  
  betas_lasso <- calculate_betas(X_matrix, Y_matrix, alpha_lasso)
  betas_ridge <- calculate_betas(X_matrix, Y_matrix, alpha_ridge)
  
  w_portfolio_lasso <- calculate_portfolio_weights(betas_lasso, N_matrix, wEW)
  w_portfolio_ridge <- calculate_portfolio_weights(betas_ridge, N_matrix, wEW)
  
  Y_matrix_lasso <- window_R %*% w_portfolio_lasso
  Y_matrix_ridge <- window_R %*% w_portfolio_ridge
  
  list(Y_matrix_lasso = Y_matrix_lasso, Y_matrix_ridge = Y_matrix_ridge)
}

# Combine the results and close the progress bar
Y_matrix_lasso <- do.call(rbind, lapply(results, `[[`, "Y_matrix_lasso"))
Y_matrix_ridge <- do.call(rbind, lapply(results, `[[`, "Y_matrix_ridge"))

# Further operations or analysis can be performed using Y_matrix_lasso and Y_matrix_ridge

  
#####----------------------------------------------------------------------#####


Y_matrix_lasso




# # Extract coefficients from OLS model
# ols_coefficients <- coef(OLS)
# # Extract residuals from OLS model
# ols_residuals <- residuals(OLS)
# # Get summary statistics of OLS model
# ols_summary <- summary(OLS)
# 
# #####----------------------------------------------------------------------#####
# 
# # Assuming you have calculated betas_lasso and betas_ridge using calculate_betas function
# 
# # Extract coefficients from Lasso and Ridge models
# lasso_coefficients <- betas_lasso
# ridge_coefficients <- betas_ridge
# 
# 
# # Calculate daily portfolio return
# daily_portfolio_return <- sum(weights * asset_returns)
# 
# # Assuming previous_portfolio_value is the portfolio value from the previous day
# # Calculate total portfolio value for the specific date
# total_portfolio_value <- previous_portfolio_value * (1 + daily_portfolio_return)
# 
# 
# # Log OLS model information
# print("OLS Coefficients:")
# print(ols_coefficients)
# print("OLS Residuals:")
# print(ols_residuals)
# print("OLS Summary:")
# print(ols_summary)
# 
# # Log Lasso and Ridge coefficients
# print("Lasso Coefficients:")
# print(lasso_coefficients)
# print("Ridge Coefficients:")
# print(ridge_coefficients)
# 
# # Log Portfolio Return
# print(paste("Total Portfolio Value on Specific Date:", total_portfolio_value))
# 
# 
# 
# 

#####------OLD CODE-----------------------------------------------------------#####

# 
# #Example of 2023-01-03 to test the code
# example_R <- demeaned_return[1:252,] #dimension 252x100
# example_Y <- example_R %*% wEW   #dimension 252 x1 
# example_X <- example_R %*% N_matrix #dimension 252x99 (252x100 x 100x99)
# example_model <- lm(example_Y ~ 0 + example_X)
# example_beta <- coef(model)
# 


#...then calculate the return somehow
#(From project: This portfolio’s return on 2023-01-03 is 0.366)