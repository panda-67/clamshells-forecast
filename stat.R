# Clamshesll anaylysis

library(readxl)
library(openxlsx)
library(tidyverse)
library(ggplot2)
library(ggthemes)

setwd("/Volumes/Garage/Daily/Tiram II")

path <- "/Volumes/Garage/Daily/Tiram II/dataset.xlsx"

# Read each sheet from the Excel file and add a 'pool' column
sheet1 <- read_excel(path = path, sheet = "20_sample") %>%
    mutate(pool = "20 individu")

sheet2 <- read_excel(path = path, sheet = "40_sample") %>%
    mutate(pool = "40 individu")

sheet3 <- read_excel(path = path, sheet = "60_sample") %>%
    mutate(pool = "60 individu")

# Combine all sheets into one data frame, keeping track of their origin
df <- bind_rows(sheet1, sheet2, sheet3)

dataframe <- df %>%
    fill(salinity, temperature, pH, .direction = "downup")

dataframe <- dataframe %>%
    mutate(month = factor(month, levels = c("jul", "aug", "sep", "oct", "nov", "dec", "jan")))

## PREVIEW
# Calculate the mean of length, width, and weight for each month and location
tibble_summary <- dataframe %>%
    group_by(month, location, pool) %>%
    summarise(
        mean_length = mean(length, na.rm = TRUE),
        mean_width = mean(width, na.rm = TRUE),
        mean_weight = mean(weight, na.rm = TRUE),
        .groups = "drop"
    )

# Reshape the summary data from wide to long format
tibble_long <- tibble_summary %>%
    pivot_longer(
        cols = c(mean_length, mean_width, mean_weight),
        names_to = "Measurement",
        values_to = "Mean_Value",
        names_prefix = "mean_"
    )

# Plot the combined growth patterns using monthly means, split by location
ggplot(tibble_long, aes(x = month, y = Mean_Value, color = Measurement, group = interaction(Measurement, pool))) +
    geom_line(aes(linetype = pool)) +
    geom_point(aes(shape = pool)) +
    labs(
        title = "Mean Growth Pattern (Length, Width, Weight) Over Time",
        x = "Month",
        y = "Mean Value",
        color = "Measurement",
        linetype = "pool",
        shape = "pool"
    ) +
    theme_solarized_2() +
    theme(plot.title = element_text(size = 13)) +
    facet_wrap(~location) # Split plot by location

## MODEL
# Assuming "length" as the response variable
model <- lm(
    length ~ salinity + temperature + pH + month + location + pool,
    data = dataframe
)

summary(model)

# Fit the Von Bertalanffy Growth Model
library(minpack.lm)

# Define the VBGF function
VBGF_length <- function(Linf, K, t0, t) {
    Linf * (1 - exp(-K * (t - t0)))
}

VBGF_width <- function(Linf, K, t0, t) {
    Linf * (1 - exp(-K * (t - t0)))
}

VBGF_weight <- function(Linf, K, t0, t) {
    Linf * (1 - exp(-K * (t - t0)))
}

# Convert months to age (assuming t = 0 for July)
month_to_age <- function(month) {
    month_mapping <- c(jul = 0, aug = 1 / 12, sep = 2 / 12, oct = 3 / 12, nov = 4 / 12, dec = 5 / 12, jan = 6 / 12)
    return(month_mapping[month])
}

dataframe$age <- month_to_age(tolower(dataframe$month))

# Use nlsLM() to fit the model based on observed length
fit <- nlsLM(length ~ VBGF_length(Linf, K, t0, age),
    data = dataframe,
    start = list(Linf = max(dataframe$length) + 5, K = 0.1, t0 = -0.5),
    lower = c(Linf = 10, K = 0, t0 = -1),
    upper = c(Linf = 70, K = 5, t0 = 1),
    control = nls.control(maxiter = 1000, warnOnly = TRUE)
)

# View summary of the fit to see the estimated parameters
summary(fit)

# Extract the fitted parameters
fitted_Linf <- coef(fit)["Linf"]
fitted_K <- coef(fit)["K"]
fitted_t0 <- coef(fit)["t0"]

# Predicted lengths using the fitted parameters
dataframe$predicted_length <- VBGF_length(
    Linf = fitted_Linf, K = fitted_K, t0 = fitted_t0, t = dataframe$age
)

# Use nlsLM() to fit the model based on observed width
fit_width <- nlsLM(width ~ VBGF_width(Linf, K, t0, age),
    data = dataframe,
    start = list(Linf = max(dataframe$width) + 5, K = 0.1, t0 = -0.5),
    lower = c(Linf = 10, K = 0, t0 = -1),
    upper = c(Linf = 40, K = 5, t0 = 1),
    control = nls.control(maxiter = 1000, warnOnly = TRUE)
)

# View summary of the fit to see the estimated parameters
summary(fit_width)

# Extract the fitted parameters
fitted_width_Linf <- coef(fit_width)["Linf"]
fitted_width_K <- coef(fit_width)["K"]
fitted_width_t0 <- coef(fit_width)["t0"]

# Predicted lengths using the fitted parameters
dataframe$predicted_width <- VBGF_width(
    Linf = fitted_width_Linf, K = fitted_width_K, t0 = fitted_width_t0, t = dataframe$age
)

# Use nlsLM() to fit the model based on observed weight
fit_weight <- nlsLM(weight ~ VBGF_weight(Linf, K, t0, age),
    data = dataframe,
    start = list(Linf = max(dataframe$weight) + 5, K = 0.1, t0 = -0.5),
    lower = c(Linf = 0, K = 0, t0 = -1),
    upper = c(Linf = 20, K = 5, t0 = 1),
    control = nls.control(maxiter = 1000, warnOnly = TRUE)
)

# View summary of the fit to see the estimated parameters
summary(fit_weight)

# Extract the fitted parameters
fitted_weight_Linf <- coef(fit_weight)["Linf"]
fitted_weight_K <- coef(fit_weight)["K"]
fitted_weight_t0 <- coef(fit_weight)["t0"]

# Predicted lengths using the fitted parameters
dataframe$predicted_weight <- VBGF_weight(
    Linf = fitted_weight_Linf, K = fitted_weight_K, t0 = fitted_weight_t0, t = dataframe$age
)

# Merge the 'length' values from overall_mean with the original dataframe
dataframe <- dataframe %>%
    left_join(overall_mean_length %>% select(month, pool, mean_length),
        by = c("month", "pool")
    )

ggplot(dataframe, aes(x = age, y = mean_length, color = pool)) +
    geom_point() +
    geom_line(aes(y = predicted_length), color = "red", linetype = "dashed") +
    labs(
        title = "Observed vs Fitted Lengths using VBGF",
        x = "Age (years)", y = "Length (mm)"
    ) +
    theme_solarized_2() +
    theme(plot.title = element_text(size = 13))

dataframe <- dataframe %>%
    left_join(overall_mean_width %>% select(month, pool, mean_width),
        by = c("month", "pool")
    )

ggplot(dataframe, aes(x = age, y = mean_width, color = pool)) +
    geom_point() +
    geom_line(aes(y = predicted_width), color = "red", linetype = "dashed") +
    labs(
        title = "Observed vs Fitted Widths using VBGF",
        x = "Age (years)", y = "Length (mm)"
    ) +
    theme_solarized_2() +
    theme(plot.title = element_text(size = 13))


dataframe <- dataframe %>%
    left_join(overall_mean_weight %>% select(month, pool, mean_weight),
        by = c("month", "pool")
    )

ggplot(dataframe, aes(x = age, y = mean_weight, color = pool)) +
    geom_point() +
    geom_line(aes(y = predicted_weight), color = "red", linetype = "dashed") +
    labs(
        title = "Observed vs Fitted Weights using VBGF",
        x = "Age (years)", y = "Length (mm)"
    ) +
    theme_solarized_2() +
    theme(plot.title = element_text(size = 13))

## Prdicted to feature years
# Step 1: Generate ages for the next 5 to 10 years
future_years <- seq(from = 0.5, to = 5, by = 0.1) # Age in years, using 0.1 to make it smooth

# Step 2: Predict lengths for future ages using fitted parameters
future_lengths <- VBGF_length(Linf = fitted_Linf, K = fitted_K, t0 = fitted_t0, t = future_years)
future_widths <- VBGF_width(Linf = fitted_width_Linf, K = fitted_width_K, t0 = fitted_width_t0, t = future_years)
future_weight <- VBGF_weight(Linf = fitted_weight_Linf, K = fitted_weight_K, t0 = fitted_weight_t0, t = future_years)

# Step 3: Create a new dataframe to hold future predictions
future_data <- data.frame(age = future_years, predicted_length = future_lengths, predicted_width = future_widths, predicted_weight = future_weight)

# Step 4: Plot observed and predicted lengths including future predictions

ggplot() +
    # Plot observed data (blue points)
    geom_point(data = dataframe, aes(x = age, y = mean_length, color = pool)) +

    # Plot fitted data (predicted values for existing ages, red dashed lines, grouped by pool)
    geom_line(data = dataframe, aes(x = age, y = predicted_length, linetype = pool), color = "red", linetype = "dashed") +

    # Plot future predicted data (green lines for future ages)
    geom_line(data = future_data, aes(x = age, y = predicted_length), color = "orange") +

    # Labels and title
    labs(
        title = "Observed vs Fitted and Future Predicted Lengths using VBGF",
        x = "Age (years)", y = "Length (mm)"
    ) +

    # Theme settings
    theme_solarized_2() +
    theme(plot.title = element_text(size = 13))

ggplot() +
    # Plot observed data (blue points)
    geom_point(data = dataframe, aes(x = age, y = mean_width, color = pool)) +

    # Plot fitted data (predicted values for existing ages, red dashed lines, grouped by pool)
    geom_line(data = dataframe, aes(x = age, y = predicted_width, linetype = pool), color = "red", linetype = "dashed") +

    # Plot future predicted data (green lines for future ages)
    geom_line(data = future_data, aes(x = age, y = predicted_width), color = "orange") +

    # Labels and title
    labs(
        title = "Observed vs Fitted and Future Predicted Widths using VBGF",
        x = "Age (years)", y = "Width (mm)"
    ) +

    # Theme settings
    theme_solarized_2() +
    theme(plot.title = element_text(size = 13))

ggplot() +
    # Plot observed data (blue points)
    geom_point(data = dataframe, aes(x = age, y = mean_weight, color = pool)) +

    # Plot fitted data (predicted values for existing ages, red dashed lines, grouped by pool)
    geom_line(data = dataframe, aes(x = age, y = predicted_weight, linetype = pool), color = "red", linetype = "dashed") +

    # Plot future predicted data (green lines for future ages)
    geom_line(data = future_data, aes(x = age, y = predicted_weight), color = "orange") +

    # Labels and title
    labs(
        title = "Observed vs Fitted and Future Predicted Weights using VBGF",
        x = "Age (years)", y = "Weight (g)"
    ) +

    # Theme settings
    theme_solarized_2() +
    theme(plot.title = element_text(size = 13))
