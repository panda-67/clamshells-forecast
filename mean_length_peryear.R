# Parameters
L_infinity <- fitted_Linf
K <- fitted_K
t0 <- fitted_t0

# Define a function to calculate weight using VBGF
calculate_length <- function(age) {
    L <- L_infinity * (1 - exp(-K * (age - t0)))
    return(L)
}

# Calculate lengths for each age
ages <- 1:5 # Change this to however many years you want
lengths <- sapply(ages, calculate_length)

# Show the results
lengths
length_differences <- diff(lengths)

# Calculate the mean increase in weight per year
mean_increase <- mean(length_differences, na.rm = TRUE)
mean_increase
