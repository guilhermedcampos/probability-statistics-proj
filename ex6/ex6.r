"""
Consider that X1,…,X40 are independent and identically distributed random variables following an exponential distribution with an expected value of a=4, 
where Xi represents the duration of the ii-th electronic component, for i=1,…,40.

To calculate the reliability of the total duration of the 40 electronic components exceeding 126, that is, to calculate P(Y>126), where Y = ∑(i=1, 40) Xi. 
Consider the following approaches:

    Simulated Value: Fixing the seed at 1973, generate 1000 samples of size n=40n=40 from the specified exponential distribution and calculate both 
    the simulated value of YY for each sample and the proportion of simulated values of YY that are greater than 126.

    Exact Value: Knowing that the exact distribution of Y is a gamma distribution with shape parameter 40 and scale parameter 1/4, obtain the exact value
    of the reliability of the total duration of the 40 electronic components exceeding 126.

Obtain the probability in question using each of the two approaches, and indicate the absolute difference between the results obtained from approaches 1 and 2, 
  multiplied by 100 and rounded to 4 decimal places.
"""
set.seed(1973)  # Setting the random seed for reproducibility

# Parameters
n <- 40  # Number of components (sample size)
a <- 4  # Expected value (mean) of the exponential distribution
lambda <- 1/a  # Rate parameter for the exponential distribution (inverse of the mean)
num_samples <- 1000  # Number of samples to generate
threshold <- 126  # Threshold value for the reliability calculation

# Initialize a vector to store the sums of Y
values_Y <- numeric(num_samples)

# Generate 1000 samples of size n=40 from the exponential distribution with expected value a=4
for (i in 1:num_samples) {
  sample <- rexp(n, rate = lambda)  # Generate a sample from the exponential distribution
  values_Y[i] <- sum(sample)  # Calculate and store the sum Y for each sample
}

# Calculate the proportion of Y values greater than 126
simulated_proportion <- mean(values_Y > threshold)

# Exact approach using the gamma distribution
shape <- n  # Shape parameter (k)
rate <- 1/4  # Rate parameter (θ), inverse of the expected value

# Calculate P(Y > 126) using the complementary CDF of the gamma distribution
exact_proportion <- pgamma(threshold, shape, rate, lower.tail = FALSE)

# Calculate the absolute difference between the exact and simulated proportions, multiply by 100, and round to 5 decimal places
diff <- round(abs(exact_proportion - simulated_proportion) * 100, 5)
diff  # Output the final difference

