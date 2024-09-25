"""
Let (Z1,Z2,…,Zn+1)be independent random variables with a standard normal distribution. It is proven that the random variable

T = sqrt(n) * Z1 / sqrt(Z2^2 + Z3^2 + ... + Zn+1^2)

has a t-Student distribution with nn degrees of freedom.

As empirical confirmation, the goal is to evaluate the probability p=P(T≤1.5)p=P(T≤1.5) by following these steps:

    Consider n=23.
    Fix the seed at 1950 and generate r=300r=300 samples, each consisting of m=170 values of T, based on the generation of standard normal variables.
    For each generated sample, determine the proportion of values less than or equal to 1.5.
    Finally, calculate the average of these proportions as an approximation of p.

Calculate the absolute difference between this approximation and the value obtained through the R function that directly calculates the probability that a random variable with t-distribution t(23)t(23) takes values less than or equal to 1.5. 
Multiply the resulting value by 100 and report the final result rounded to 5 decimal places.
"""
set.seed(1950)  # Setting the random seed for reproducibility

n <- 23  # Degrees of freedom (n)
r <- 300  # Number of samples
m <- 170  # Number of T values in each sample
p <- numeric(r)  # Vector to store the proportions for each sample

# Generate samples and compute T values for each one
for (i in 1:r) {
  # Generate m values of T
  T_values <- replicate(m, {
    Z <- rnorm(n + 1)  # Generate n+1 standard normal variables Z
    sqrt(n) * Z[1] / sqrt(sum(Z[-1]^2))  # Calculate T using the formula for t-distribution
  })
  p[i] <- mean(T_values <= 1.5)  # Calculate the proportion of T values less than or equal to 1.5
}

# Calculate the average of the proportions across all samples (empirical approximation of p)
p_aproximado <- mean(p)

# Calculate the true value of p using the pt() function for the t-distribution
p_real <- pt(1.5, df = n)  # pt() gives the cumulative probability for t-distribution with n degrees of freedom

# Compute the absolute difference between the empirical approximation and the true value
diferenca <- abs(p_aproximado - p_real) * 100  # Multiply by 100 as per the problem's instructions

# Round the final result to 5 decimal places
resultado_final <- round(diferenca, 5)
resultado_final  # Output the final result


