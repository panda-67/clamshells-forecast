# Parameters
W_infinity <- fitted_width_Linf
K <- fitted_width_K
t0 <- fitted_width_t0

# Define a function to calculate weight using VBGF
calculate_width <- function(age) {
    W <- W_infinity * (1 - exp(-K * (age - t0)))
    return(W)
}

# Calculate weights for each age
ages <- 1:5 # Change this to however many years you want
widths <- sapply(ages, calculate_width)

# Show the results
widths
width_differences <- diff(widths)

# Calculate the mean increase in weight per year
mean_increase <- mean(width_differences, na.rm = TRUE)
mean_increase
