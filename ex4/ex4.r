"""
A system consists of 9 electrical circuits, where each circuit emits signals coded from the set {1,2,…,10}. 
Each circuit emits a signal i with probability i/55, for i∈{1,2,…,10}, independently of the other circuits.
If at least one of the circuits emits signal 2, a sound warning is produced. 
If at least one of the circuits emits signal 1, the system shuts down.

Fixing the seed at 2255, simulate 150 realizations of the state of a system with the characteristics described above,
and calculate the proportion of times a sound warning is produced in a system that is not shut down. 
This proportion, rounded to 2 decimal places, is:
"""
set.seed(2255)  # Set random seed for reproducibility

# Probabilities vector for emitting signals 1 to 10
probs <- c(1/55, 2/55, 3/55, 4/55, 5/55, 6/55, 7/55, 8/55, 9/55, 10/55)

# Possible signal values that each circuit can emit
values <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

# Number of simulations (i.e., realizations of the system)
num_simulations <- 150

# Initialize counters for the number of warnings without shutdown and the number of systems not shut down
warnings_without_shutdown <- 0 
systems_not_shutdown <- 0

# Loop through the 150 simulations
for (i in 1:num_simulations) {
  
  # Simulate the signals emitted by the 9 circuits, each emitting a signal with probability defined by 'probs'
  signals <- sample(values, size = 9, replace = TRUE, prob = probs)
  
  # Check if at least one circuit emitted signal 2 (sound warning is triggered)
  sound_warning <- any(signals == 2)
  
  # Check if at least one circuit emitted signal 1 (system shutdown is triggered)
  system_shutdown <- any(signals == 1)
  
  # If the system is not shut down, increment the 'systems_not_shutdown' counter
  if (!system_shutdown) {
    systems_not_shutdown <- systems_not_shutdown + 1
  }
  
  # If a sound warning was triggered and the system is not shut down, increment the 'warnings_without_shutdown' counter
  if (sound_warning & !system_shutdown) {
    warnings_without_shutdown <- warnings_without_shutdown + 1
  }
}

# Calculate the proportion of warnings without shutdowns
proportion <- warnings_without_shutdown / systems_not_shutdown

# Round the proportion to 2 decimal places
rounded_proportion <- round(proportion, 2)

# Output the final rounded proportion
rounded_proportion

































