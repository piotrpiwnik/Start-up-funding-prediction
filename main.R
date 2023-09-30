install.packages("dplyr")
library(dplyr)

#import data set as "df" (i formatted my own file)
#skip first column
library(readr)
df <- read_csv("df.csv", col_types = cols(...1 = col_skip()))
View(df)

#create subset file from 31/12/2021 to 31/07/2023 
#(1584 observations of 101 variables including date)
subset <- df %>% filter(df$Date >= '2021-12-31')

#format return matrix, excluding the first date column
returnMatrix <- as.matrix(subset[,-1])
print(dim(returnMatrix)) #dimension: 102184 x 100

#store numbers of observations and assets (for dimensions of matrices)
num_day <- dim(returnMatrix)[1]
num_asset <- dim(returnMatrix)[2]

#demean return matrix (Demean outside the for loop or inside?)
demeaned_return <- scale(returnMatrix, center = TRUE, scale = FALSE)

#generate equally-weighted portfolio (dimension: 100x1)
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
  
  #Calculate MinVar Portfolio
  #Calculate y = return matrix * equally weighted portfolio (matrix multiplication)
  Y_matrix <- window_R %*% wEW  #dimension Y (252x1)
  
  #Calculate X = R.N (matrix multiplication)
  X_matrix <- window_R %*% N
}
