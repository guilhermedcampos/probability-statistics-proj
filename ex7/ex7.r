"""
A risk manager at an insurance company assumes that the compensation amount (in thousands of euros) for a certain portfolio of policies is represented
by the random variable X with a probability density function given by:

f(x) = (theta / a^theta) * x^(-theta - 1) ; for x >= a

where a=4 and θ is an unknown positive parameter related to the occurrence of large compensations.

A random sample of size n=11n=11 from XX produced the following set of observations:
4.37,4.3,5.15,5.11,5.15,4.66,6.15,5.72,5.87,5.64,4.05

    Use the mle function from the base stats4 package to obtain the maximum likelihood estimate of θ, using the value 3.6 as the initial value for the
    numerical search and without changing any other optional argument of that function.

    Use the estimate obtained in 1 to determine the maximum likelihood estimate of the 75th percentile (quantile) of XX.

    Calculate the absolute deviation between the estimate obtained in 2 and the true value of the quantile when θ=3.6 
    Present the result with 4 decimal places.
"""
# Load the required package
library(stats4)

# Sample data
data <- c(4.37, 4.3, 5.15, 5.11, 5.15, 4.66, 6.15, 5.72, 5.87, 5.64, 4.05)
a <- 4

# Define the density function
density_f <- function(x, theta) {
  a <- 4
  result <- numeric(length(x))  # Initialize a vector to store results
  for (i in 1:length(x)) {
    if (x[i] < a) {
      result[i] <- 0
    } else {
      result[i] <- theta * a^theta * x[i]^(-theta - 1)
    }
  }
  return(result)
}

# Define the logarithmic function of the density
log_density_f <- function(theta) {
  -sum(log(density_f(data, theta)))
}

# Obtain the MLE of θ using the mle function
mle_result <- mle(minuslogl = log_density_f, start = list(theta = 3.6))

# Extract the estimate of θ
theta_mle <- coef(mle_result)
cat("MLE estimate of θ:", theta_mle, "\n")

# Estimate the 0.75 probability quantile
a <- 4
p <- 0.75
q_75 <- a / (1 - p)^(1 / theta_mle)
cat("Estimate of the 0.75 quantile:", q_75, "\n")

# True value of the quantile for θ = 3.6
theta_true <- 3.6
q_75_true <- a / (1 - p)^(1 / theta_true)
cat("True value of the 0.75 quantile:", q_75_true, "\n")

# Calculate the absolute deviation
absolute_deviation <- abs(q_75 - q_75_true)
final_result <- round(absolute_deviation, 4)
cat("Absolute deviation rounded to 4 decimal places:", final_result, "\n")













