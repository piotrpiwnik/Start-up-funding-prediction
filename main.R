install.packages("dplyr")
library(dplyr)

#import data set as "df", file name (df.csv) (i formatted my own file by adding a column to format date as "yyyy-mm-dd"), skipped first column (date formatted as "yyyymmdd")
library(readr)
df <- read_csv("df.csv", col_types = cols(...1 = col_skip()))
View(df)

#create a subset file with data from 31/12/2021 to 31/07/2023 
#(1584 observations of 101 variables including date)
subset <- df %>% filter(df$Date >= '2021-12-31')

#format the return matrix, excluding the first column (date)
returnMatrix <- as.matrix(subset[,-1])
print(dim(returnMatrix)) #dimension: 102184 x 100

#store numbers of observations and assets (for dimensions of matrices)
num_day <- dim(returnMatrix)[1]
num_asset <- dim(returnMatrix)[2]

#demean the return matrix (Demean outside the for loop or inside? This is outside)
demeaned_return <- scale(returnMatrix, center = TRUE, scale = FALSE)

#generate the equally-weighted portfolio (dimension: 100x1)
eqWeight <- 1/num_asset
wEW <- matrix(eqWeight, nrow = num_asset, ncol = 1)

#setting window size = 252 days
window_size <- 252
num_windows <- num_day - window_size +1 

#set up matrix N
N_matrix <- diag(99)
N_matrix <- rbind(N_matrix, rep(-1,99)) #dimension N (100 x 99)

#For loop for rolling/sliding window
for(i in 1: num_windows){
  #Extract data from the demeaned return matrix for the current window
  window_R <-  demeaned_return[i:(i+window_size -1),]
  
#MinVar Portfolio
  #Calculate y = return matrix * equally weighted portfolio (matrix multiplication)
  Y_matrix <- window_R %*% wEW  #dimension Y (252x1)
  
  #Calculate X = R.N (matrix multiplication)
  X_matrix <- window_R %*% N_matrix #dimension (252x99)
  
  #Predict y = X.beta without intercept
  OLS <- lm(y~ 0 + x)
  beta <- coef(OLS) #not sure about this part, beta dimension (99x1)
  
  #MinVar portfolio: w = wEW - N.beta (matrix multiplication)
  w_matrix <- wEW - (N_matrix %*% beta) 
  
  #... To be continued here
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

