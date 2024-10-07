# Parameters
W_infinity <- fitted_weight_Linf
K <- fitted_weight_K
t0 <- fitted_weight_t0

# Define a function to calculate weight using VBGF
calculate_weight <- function(age) {
  W <- W_infinity * (1 - exp(-K * (age - t0)))
  return(W)
}

# Calculate weights for each age
ages <- 1:10  # Change this to however many years you want
weights <- sapply(ages, calculate_weight)

# Show the results
weights
weight_differences <- diff(weights)

# Calculate the mean increase in weight per year
mean_increase <- mean(weight_differences, na.rm = TRUE)
mean_increase
