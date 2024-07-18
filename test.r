library(tidyverse)
library(PerformanceAnalytics)
library(r2r)
library(survival)


data(edhec)
dim(edhec)
# create 13 weights that sum to 1
#Generate 13 random numbers
random_numbers <- runif(13)

# Normalize so that they sum to 1
weights <- random_numbers / sum(random_numbers)
weights <- round(weights, 2)
weights[13] <- 1 - sum(weights[1:12])

print(weights)
edhec_matrix <- as.matrix(edhec[, 1:13])


# Calculate the ES of the portfolio
es_result <- ES(edhec_matrix, weights = weights,method = "historical", portfolio_method= 'component', p = 0.05)

# create a dataframe with multiple weights rows
weights_df <- data.frame(matrix(weights, nrow = 100, ncol = 13, byrow = TRUE))
#randomize the weights
weights_df <- weights_df[sample(nrow(weights_df)), ]


 


calculate_es <- function(window_data, alloc, alpha, decayfaktor) {
    CovariancePort <- RiskPortfolios::covEstimation(rets = as.matrix(window_data),
        control = list(type = 'ewma', lambda = decayfaktor)) 
    MeanPort <- RiskPortfolios::meanEstimation(rets = as.matrix(window_data),
        control = list(type = 'ewma', lambda = decayfaktor)) # nolint
     
    ESneu <- PerformanceAnalytics::ES(window_data, p = alpha, weights = alloc, method = "gaussian", 
                                portfolio_method = "component", mu = MeanPort, sigma = CovariancePort)

    return(ESneu$ES)
}



ES_results <- apply(alloc_df, 1, function(alloc) {
  rollapplyr(edhec_matrix, width = 10, 
             FUN = function(window_data) calculate_es(window_data, alloc, 0.95, 0.998), 
             by.column = FALSE, fill = NA)
})

# Create subset of the last 3 years of edhec
sub1 <- edhec['2000/2002', 1:13]

# Create hashmap with the Covariance and Mean for each of the rolling windows of the last 10 data points

cov_map <- hashmap()

for (i in 10:nrow(sub1)) {

  window_data <- sub1[(i-9):i, 1:13]

  CovariancePort <- RiskPortfolios::covEstimation(rets = as.matrix(window_data),
                                                  control = list(type = 'ewma', lambda = 0.998)) 
  MeanPort <- RiskPortfolios::meanEstimation(rets = as.matrix(window_data),
                                             control = list(type = 'ewma', lambda = 0.998)) # nolint
  cov_map[[start(window_data)]] <- list(CovariancePort, MeanPort)
}
#check keys


# Check the first element of the hashmap
cov_map[[start(sub1)]][[1]]

# Adjust the function to use the hashmap
calculate_es <- function(window_data, alloc, alpha, decayfaktor) {
    CovariancePort <- cov_map[[start(window_data)]][[1]]
    MeanPort <- cov_map[[start(window_data)]][[2]]
    
    ESneu <- PerformanceAnalytics::ES(window_data, p = alpha, weights = alloc, method = "gaussian", 
                                portfolio_method = "component", mu = MeanPort, sigma = CovariancePort)

    return(ESneu$ES)
}

# Apply the function to the rolling windows
ES_results <- apply(weights_df, 1, function(alloc) {
  rollapplyr(sub1, width = 10, 
             FUN = function(window_data) calculate_es(window_data, alloc, 0.95, 0.998), 
             by.column = FALSE, fill = NA)
})


library(parallel)

# Set up a cluster
cl <- makeCluster(detectCores() - 1)

# Export the necessary objects and libraries to the cluster
clusterExport(cl, varlist = c("sub1", "cov_map", "calculate_es", "weights_df"))
clusterEvalQ(cl, library(PerformanceAnalytics))

ES_results_list <- parLapply(cl, 1:nrow(weights_df), function(i) {
  alloc <- as.numeric(weights_df[i, ])
  if (is.numeric(alloc) && is.numeric(sub1)) {
    rollapplyr(sub1, width = 10, 
               FUN = function(window_data) {
                 if (is.numeric(window_data)) {
                   calculate_es(window_data, alloc, 0.95, 0.998)
                 } else {
                   stop("window_data is not numeric")
                 }
               }, 
               by.column = FALSE, fill = NA)
  } else {
    stop("alloc or sub1 is not numeric")
  }
})
stopCluster(cl)

#add dates to the ES results
ES_results_df <- 
# Create Kaplan-Meier survival curves for the ES results
# A ES value of 0.026 is considered a failure

# Create a dataframe with the time to failure for each column ES

time_to_failure <- apply(ES_results, 2, function(col) {
  time_to_failure <- which(col > 0.026)
  if (length(time_to_failure) == 0) {
    time_to_failure <- length(col)
  } else {
    time_to_failure <- time_to_failure[1]
  }
  return(time_to_failure)
})

#vary the ES value in the function to create multiple survival curves from 0.01 to 0.1

failure_thresholds <- seq(0.01, 0.1, by = 0.01)

time_to_failure_df <- data.frame(matrix(NA, nrow = nrow(ES_results), ncol = length(failure_thresholds)))

for (i in 1:length(failure_thresholds)) {
  time_to_failure_df[, i] <- apply(ES_results, 2, function(col) {
    time_to_failure <- which(col > failure_thresholds[i])
    if (length(time_to_failure) == 0) {
      time_to_failure <- length(col)
    } else {
      time_to_failure <- time_to_failure[1]
    }
    return(time_to_failure)
  })
}
# Create a matrix with 1 weight vector * edhec_matrix
edhec_weighted_returns <- as.matrix(edhec) %*% as.vector(t(weights_df[1, ]))
edhec_weighted_returns <- as.data.frame(edhec_weighted_returns) 

# Create a cumprod of the returns
edhec_weighted_cumprod <- cumprod(1 + edhec_weighted_returns) %>%
    rownames_to_column(var = "Date") %>%
    rename(Cumulative_Return = 2)

# Create a new column and copy only the value of the first entry each year
edhec_weighted_cumprod$Hindcast <- NA

# create a column with the year
edhec_weighted_cumprod$Year <- year(as.Date(edhec_weighted_cumprod$Date))

# get the rolling 10 year return for each year
edhec_weighted_cumprod_roll <- edhec_weighted_cumprod %>%
    group_by(Year) %>%
    mutate(Rolling_10Y = Cumulative_Return - lag(Cumulative_Return, 10))
    #set the last entry of each year to NA
    #mutate(Rolling_10Y = ifelse(Year != lag(Year), NA, Rolling_10Y))


#### New Test

# Assuming you have the mean (mu) and variance (sigma_squared) of the annual return
mu <- 0.05 # Example mean annual return
sigma_squared <- 0.02 # Example variance of annual return

# Target value at n+10
T <- 1.5 # Example target cumulative return at n+10

# Calculate expected return and standard deviation at n+10
expected_return_n10 <- mu * 10
std_dev_n10 <- sqrt(sigma_squared * 10)

# Calculate Z-score for the target value T
Z <- (T - expected_return_n10) / std_dev_n10

# Calculate the probability of reaching the target value T using the pnorm function for the normal CDF
probability_reaching_T <- pnorm(Z)

# Output the probability
probability_reaching_T


# Second Idea ------

set.seed(123) # For reproducibility
V0 <- 0.8 # Current value after drop
T <- 2 # Target value
mu <- 0.05 # Annual expected return
sigma <- 0.2 # Annual volatility
n <- 10 # Number of periods to target
m <- 10000 # Number of simulations

# Function to simulate one path
simulate_path <- function(V0, mu, sigma, n) {
  dt <- 1 # Assuming each period is one year
  path <- numeric(n)
  path[1] <- V0
  for (t in 2:n) {
    Z <- rnorm(1, 0, 1)
    path[t] <- path[t-1] * exp((mu - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z)
  }
  return(path)
}

# Run simulations
final_values <- replicate(m, simulate_path(V0, mu, sigma, n)[n])

# Calculate probability of reaching or exceeding target
probability <- sum(final_values >= T) / m

# Output the probability
probability


# Third Idea------

S0 <- 0.8 # Current value after drop
ST <- 2 # Target value
mu <- 0.05 # Annual expected return
sigma <- 0.2 # Annual volatility
T <- 10 # Time from X to N in years

# Calculate Z-score
Z <- (log(ST/S0) - (mu - 0.5 * sigma^2) * T) / (sigma * sqrt(T))

# Calculate the probability of reaching or exceeding the target value
probability <- 1 - pnorm(Z)

# Output the probability
probability

# Fourth Idea -----

# Assuming you have P(A), P(B|A), and P(B) calculated
P_A <- 0.5 # Prior probability of reaching the goal
P_B_given_A <- 0.8 # Probability of being at 0.8 of the target after 5 years given that the goal is reached
P_B <- 0.6 # Overall probability of being at 0.8 of the target after 5 years

# Calculate P(A|B) using Bayes' Theorem
P_A_given_B <- (P_B_given_A * P_A) / P_B

# Output the posterior probability
P_A_given_B


# Fifth Idea -----

# Assuming 'goal_percentage' is the return goal over 10 years, e.g., 1.5 for a 50% return
goal_percentage <- 1.5

# Calculate P(A): Probability of reaching the goal
total_periods <- nrow(edhec_weighted_cumprod_roll)
successful_periods <- sum(edhec_weighted_cumprod_roll$Rolling_10Y >= goal_percentage, na.rm = TRUE)
P_A <- successful_periods / total_periods

# Assuming you have a way to determine if an investment is at 0.8 of the target after 5 years
# This might require additional data manipulation not shown here

# Calculate P(B): Probability of being at 0.8 of the target after 5 years
# This is a placeholder for the actual calculation
P_B <- 0.4 # Example value

# Calculate P(B|A): Probability of reaching the goal given being at 0.8 of the target after 5 years
# This requires specific data on which periods were at 0.8 of the target at the halfway point and reached the goal
# Placeholder for actual calculation
P_B_given_A <- 0.7 # Example value

# Calculate P(A|B) using Bayes' Theorem
P_A_given_B <- (P_B_given_A * P_A) / P_B