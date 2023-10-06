# Load necessary libraries
packages <- c( "dplyr",
               "quantmod",
               "glmnet",  
               "here",
               "progress", 
               "quadprog",
               "ggplot2")

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
dim(returnMatrix) #Now the dimension: is 419 x 100

#Store Numbers of Observations and Assets (for dimensions of matrices)
num_day <- dim(returnMatrix)[1]
num_asset <- dim(returnMatrix)[2]

#Demean the return matrix (Demean outside the for loop or inside? This is outside). 
demeaned_return <- scale(as.numeric(returnMatrix), center = TRUE, scale = FALSE)
demeaned_return <- matrix(demeaned_return, nrow = num_day, byrow = TRUE)
dim(demeaned_return) #--> the same dimension as the original matrix - 419x100

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

#This part needs to be edited

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

# Initialize progress bar
pb <- progress_bar$new(total = num_windows, format = "[:bar] :percent :elapsed ETA: :eta", clear = FALSE)


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

}
#####----------------------------------------------------------------------#####

#Okay, so now we have the weights for both lasso and ridge. Now we have to get 
#the return for each day for eW portfolio, Lasso and Ridge.


# Calculate daily returns for eW portfolio, Lasso, Ridge, and MinVar portfolios
# return_minvar <- demeaned_return %*% w_portfolio_minvar
return_eW <- demeaned_return %*% wEW
return_lasso <- demeaned_return %*% w_portfolio_lasso
return_ridge <- demeaned_return %*% w_portfolio_ridge


# Extract the "X" column from the original df
X_column <- df$X

# Convert the returns matrices to data frames
return_eW_df <- data.frame(date = X_column, Return_eW = return_eW)
return_lasso_df <- data.frame(date = X_column, Return_lasso = return_lasso)
return_ridge_df <- data.frame(date = X_column, Return_ridge = return_ridge)

# Merge the data frames based on the "date" column
merged_df <- merge(return_eW_df, return_lasso_df, by = "date", all.x = TRUE)  # Merge return_eW_df and return_lasso_df
merged_df <- merge(merged_df, return_ridge_df, by = "date", all.x = TRUE)  # Merge with return_ridge_df

#####----------------------------------------------------------------------#####

# Assuming risk-free rate is available and stored in a variable called rf_rate
rf_rate <- 0.02  # Replace this with your actual risk-free rate

# Calculate excess returns for eW portfolio, Lasso portfolio, and Ridge portfolio
merged_df$Excess_Return_eW <- merged_df$Return_eW - rf_rate
merged_df$Excess_Return_lasso <- merged_df$Return_lasso - rf_rate
merged_df$Excess_Return_ridge <- merged_df$Return_ridge - rf_rate

# Calculate Sharpe ratio for eW portfolio
sharpe_ratio_eW <- mean(merged_df$Excess_Return_eW) / sd(merged_df$Excess_Return_eW)

# Calculate Sharpe ratio for Lasso portfolio
sharpe_ratio_lasso <- mean(merged_df$Excess_Return_lasso) / sd(merged_df$Excess_Return_lasso)

# Calculate Sharpe ratio for Ridge portfolio
sharpe_ratio_ridge <- mean(merged_df$Excess_Return_ridge) / sd(merged_df$Excess_Return_ridge)

# Print Sharpe ratios
print(paste("Sharpe Ratio for eW Portfolio:", round(sharpe_ratio_eW, 2)))
print(paste("Sharpe Ratio for Lasso Portfolio:", round(sharpe_ratio_lasso, 2)))
print(paste("Sharpe Ratio for Ridge Portfolio:", round(sharpe_ratio_ridge, 2)))




#####----------------------------------------------------------------------#####

#Convert the date column to Date type
merged_df$date <- as.Date(merged_df$date, format = "%Y-%m-%d")

# Create a line chart with returns over time
ggplot(merged_df, aes(x = date)) +
  geom_line(aes(y = Return_eW, color = "eW Return"), size = 1) +
  geom_line(aes(y = Return_lasso, color = "Lasso Return"), size = 1) +
  geom_line(aes(y = Return_ridge, color = "Ridge Return"), size = 1) +
  labs(x = "Date", y = "Returns", color = "Legend") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_manual(values = c("eW Return" = "blue", "Lasso Return" = "green", "Ridge Return" = "red"))

#####----------------------------------------------------------------------#####

# Calculate cumulative returns for each type of return
merged_df$Cumulative_eW <- cumsum(merged_df$Return_eW)
merged_df$Cumulative_lasso <- cumsum(merged_df$Return_lasso)
merged_df$Cumulative_ridge <- cumsum(merged_df$Return_ridge)

# Plot cumulative returns over time
ggplot(merged_df, aes(x = date)) +
  geom_line(aes(y = Cumulative_eW, color = "eW Cumulative Return"), size = 1) +
  geom_line(aes(y = Cumulative_lasso, color = "Lasso Cumulative Return"), size = 1) +
  geom_line(aes(y = Cumulative_ridge, color = "Ridge Cumulative Return"), size = 1) +
  labs(x = "Date", y = "Cumulative Returns", color = "Legend") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_manual(values = c("eW Cumulative Return" = "blue", 
                                "Lasso Cumulative Return" = "green", 
                                "Ridge Cumulative Return" = "red"))

