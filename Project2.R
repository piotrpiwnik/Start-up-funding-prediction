# Load necessary libraries
packages <- c( "dplyr",
               "quantmod",
               "glmnet",  
               "here",
               "progress")

# Check if packages are installed
packages_to_install <- packages[!packages %in% installed.packages()[,"Package"]]

# Install packages if not already installed
if(length(packages_to_install)) install.packages(packages_to_install, repos = "https://cloud.r-project.org/")

# Load packages
lapply(packages, require, character.only = TRUE)

#####----------------------------------------------------------------------#####


# Set the working directory to the project root using here()
setwd("C:/Users/swiet/OneDrive/Pulpit/Study/Exchange/Predictive Analytics")


# Read the CSV file using here() to construct the file path
df <- read.csv("./portfolio.data.csv")


#####----------------------------------------------------------------------#####

#Mutating data - dates
df$X <- as.Date(df$X, format = "%Y%m%d")
df$X <- format(df$X, "%Y-%m-%d")


# The data contains four tables. We only need the first one.
# Subset the dataframe from the beginning to 25566 exclusive. 2022-12-30 is the 25399th row as in the project description.
df <- df[1:25566, ]

#Additional line for sample testing, only first 419 observations (2021-12-31 - 2023-08-31)
df <- df[25148:25566,]

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

# Initialize progress bar
pb <- progress_bar$new(total = num_windows, format = "[:bar] :percent :elapsed ETA: :eta", clear = FALSE)

# Create vector that will contain returns of the portfolios for each window 
sum_wEW <- c()
sum_lasso <- c()
sum_ridge <- c()

# Create vector with Sharpe Ratio 
shr_wEW <- c()
shr_lasso <- c()
shr_ridge <- c()

#For loop for rolling/sliding window
for(i in 1: num_windows){
  
  pb$tick()  # Increment progress bar
  
  
  #Extract data from the demeaned return matrix for the current window
  window_R <-  demeaned_return[i:(i+window_size -1), , drop =F]
  
  #Calculate y = return matrix * equally weighted portfolio (matrix multiplication)
  Y_matrix <- window_R %*% wEW  #dimension Y (252x1)
  
  #Calculate X = R.N (matrix multiplication)
  X_matrix <- window_R %*% N_matrix #dimension (252x99)
  
  #Predict y = X.beta without intercept
  OLS <- lm(Y_matrix ~ 0 + X_matrix)
  beta <- coef(OLS) 
  
  #MinVar portfolio: w = wEW - N.beta (matrix multiplication)
  w_matrix <- wEW - (N_matrix %*% beta) 
  
  # Convert betas to portfolios using Lasso and Ridge
  alpha_lasso <- 1  # Lasso
  alpha_ridge <- 0  # Ridge
  betas_lasso <- calculate_betas(X_matrix, Y_matrix, alpha_lasso)
  betas_ridge <- calculate_betas(X_matrix, Y_matrix, alpha_ridge)
  
  w_portfolio_lasso <- calculate_portfolio_weights(betas_lasso, N_matrix, wEW)
  w_portfolio_ridge <- calculate_portfolio_weights(betas_ridge, N_matrix, wEW)
  
  return_eW <- demeaned_return %*% wEW
  return_lasso <- demeaned_return %*% w_portfolio_lasso
  return_ridge <- demeaned_return %*% w_portfolio_ridge
  
  sum_wEW <- sum_wEW + sum(return_eW)
  sum_lasso <- sum_lasso + sum(return_lasso)
  sum_ridge <- sum_ridge + sum(return_ridge)
  
  shr_wEW <- shr_wEW  + sum(return_eW)/sd(return_eW)
  shr_lasso <- shr_lasso + sum(return_lasso)/sd(return_lasso)
  shr_ridge <- shr_ridge + sum(return_ridge)/sd(return_ridge)
  
}

#####----------------------------------------------------------------------#####

#Okay, so now we have the weights for both lasso and ridge. Now we have to get 
#the return for each day for eW portfolio, Lasso and Ridge.


#####----------------------------------------------------------------------#####

# #Example of 2023-01-03 to test the code
example_return <- as.numeric(returnMatrix[1:252,1:6])
example_return <- matrix(example_return, nrow=252)
example_R <- scale(as.numeric(example_return), center=TRUE, scale=FALSE) #dimension 252x100
example_R <- matrix(example_R, nrow=252, byrow=TRUE) 
example_wEW <- matrix(1/6, nrow = 6, ncol = 1)
example_Y <- example_R %*% example_wEW   #dimension 252 x1 
N_matrix_ex <- diag(5)
N_matrix_ex <- rbind(N_matrix_ex,rep(-1,5))
example_X <- example_R %*% N_matrix_ex #dimension 252x99 (252x100 x 100x99)
example_model <- lm(example_Y ~ 0 + example_X)
example_beta <- coef(example_model)
w_ew <- example_wEW - (N_matrix_ex %*% example_beta)
return_ex <- example_R %*% w_ew
w_ew
# 


#...then calculate the return somehow
#(From project: This portfolio’s return on 2023-01-03 is 0.366)