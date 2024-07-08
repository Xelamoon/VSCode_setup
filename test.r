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
