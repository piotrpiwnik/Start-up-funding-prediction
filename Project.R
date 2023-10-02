library(dplyr)
library(quantmod)
library(glmnet)
library(readr)

# Specify the URL of the CSV file on GitHub
github_url <- "https://raw.githubusercontent.com/username/repositoryname/main/portfolio.data.csv"

# Use read_csv to directly read the CSV file from the GitHub URL into a data frame
df <- read_csv(url(github_url))

#Mutating data - dates
df$X <- as.Date(df$X, format = "%Y%m%d")
df$X <- format(df$X, "%Y-%m-%d")
#View(df)

#Formating the Return Matrix, excluding the first column (date)
returnMatrix <- df[,-1]
returnMatrix <- as.matrix(returnMatrix)
dim(returnMatrix) #dimension: 102184 x 100

#Store Numbers of Observations and Assets (for dimensions of matrices)
num_day <- dim(returnMatrix)[1]
num_asset <- dim(returnMatrix)[2]

#Demean the return matrix (Demean outside the for loop or inside? This is outside)
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


#For loop for rolling/sliding window
for(i in 1: num_windows){
  
  #Extract data from the demeaned return matrix for the current window
  window_R <-  demeaned_return[i:(i+window_size -1), , drop =F]
  
  #Calculate y = return matrix * equally weighted portfolio (matrix multiplication)
  Y_matrix <- window_R %*% wEW  #dimension Y (252x1)
  
  #Calculate X = R.N (matrix multiplication)
  X_matrix <- window_R %*% N_matrix #dimension (252x99)
  
  #Predict y = X.beta without intercept
  OLS <- lm(y~ 0 + X_matrix)
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
  
}

#Example of 2023-01-03 to test the code
example_R <- demeaned_return[1:252,] #dimension 252x100
example_Y <- example_R %*% wEW   #dimension 252 x1 
example_X <- example_R %*% N_matrix #dimension 252x99 (252x100 x 100x99)
model <- lm(example_Y ~ 0 + example_X) #ERROR here (there are some NAs and I cant find the reason for it)
beta <- coef(model)
...
#then calculate the return somehow
#(From project: This portfolio’s return on 2023-01-03 is 0.366)