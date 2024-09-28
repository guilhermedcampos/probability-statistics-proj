"""
Let (X1,…,X100) be a random sample from a population with a Poisson distribution with parameter λ. 
To test the hypothesis H0:λ=2.90H against H1:λ=3.15, a hypothesis test is constructed that leads to the 
rejection of H0 when (avg X) > k with k=3.234.

Fixing the seed at 2822, generate m=5000 pairs of samples of size n=100n=100 from the Poisson distribution,
one under H0 and the other under H1 in each pair, and calculate the relative frequencies of each of 
the two types of errors that the test leads to. 
Based on the results obtained, calculate an approximate value of the ratio between the probability of a Type II error and the 
probability of a Type I error.
"""
set.seed(2822)

type_1_errors <- 0
type_2_errors <- 0

for (i in 1:5000) {
  sample_h0 <- rpois(100, 2.90)
  sample_11 <- rpois(100, 3.15)
  
  mean_h0 <- mean(sample_h0)
  mean_h1 <- mean(sample_11)
  
  if (mean_h0 > 3.234) {
    type_1_errors <- type_1_errors + 1
  }
  if (mean_h1 <= 3.234) {
    type_2_errors <- type_2_errors + 1
  }
}

rel_freq_type_1 <- type_1_errors / 5000
rel_freq_type_2 <- type_2_errors / 5000
error_ratio <- rel_freq_type_2 / rel_freq_type_1
print(error_ratio)

