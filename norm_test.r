library(tidyverse)
library(PerformanceAnalytics)
library(r2r)
library(survival)


data(edhec)
dim(edhec)
# create 13 weights that sum to 1
#Generate 13 random numbers
random_numbers <- runif(13)
df <- as.data.frame(edhec)

# Create df with total return
df_total <- df %>%
  mutate(total = rowSums(df))

# df with mean and var

df_mean_sd <- df_total %>%
  summarise(total_mean = mean(total), total_sd = var(total))

# Calculate the year gap between the first and last date
year_gap <- df %>%
  rownames_to_column() %>%
  mutate(rowname = as.Date(rowname, format = "%Y-%m-%d")) %>%
  arrange(rowname) %>%
  summarise(year_gap = as.numeric(difftime(rowname[n()], rowname[1], units = "days")/365)) %>%
  pull()

# calculate the expected return using the black scholes meron model
E_r <- df_mean_sd$total_mean
var_r <-df_mean_sd$total_sd 
yearly_interest <- 0.00


# yearly mean
yearly_return <- ((E_r - yearly_interest)  - 0.5 * var_r) * year_gap 
sd_yearly <- sqrt(var_r * year_gap)

# log return from first value to last value first caluclate the cummulative return

b <- pnorm(log(14.9/1.3), mean = yearly_return, sd = sd_yearly, lower.tail = FALSE)

b *100

# do the caluclation again but use the z score normalisation

z_score <- (log(14.9/1.3) - yearly_return) / sd_yearly

a <- pnorm(z_score, mean = 0, sd = 1, lower.tail = FALSE)
a*100
a == b

# qnorm to get the 99% confidence interval

exp(qnorm(p = c(.025, .975), mean = log(1.3) + yearly_return, sd = sd_yearly))

qnorm(p = 0.3151601, mean = yearly_return, sd = sd_yearly, lower.tail = FALSE)

dnorm(14.9, mean = yearly_return, sd = sd_yearly)
