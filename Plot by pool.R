# Clamshesll anaylysis by pool

library(readxl)
library(openxlsx)
library(tidyverse)
library(ggplot2)
library(ggthemes)

setwd("/Volumes/Garage/Daily/Tiram II")

path <- "/Volumes/Garage/Daily/Tiram II/dataset.xlsx"

ordered_month <- c("jul", "aug", "sep", "oct", "nov", "dec", "jan")

# Read each sheet from the Excel file and add a 'Sheet' column
pool_20 <- read_excel(path = path, sheet = "20_sample") %>%
  mutate(pool = "20 individu") %>%
  mutate(month = factor(month, levels = ordered_month)) %>%
  fill(salinity, temperature, pH, .direction = "downup")

pool_40 <- read_excel(path = path, sheet = "40_sample") %>%
  mutate(pool = "40 individu") %>%
  mutate(month = factor(month, levels = ordered_month)) %>%
  fill(salinity, temperature, pH, .direction = "downup")

pool_60 <- read_excel(path = path, sheet = "60_sample") %>%
  mutate(pool = "60 individu") %>%
  mutate(month = factor(month, levels = ordered_month)) %>%
  fill(salinity, temperature, pH, .direction = "downup")

# Calculate the mean of length, width, and weight for each month and location
mean_20 <- pool_20 %>%
  group_by(month, location, pool) %>%
  summarise(mean_length = mean(length, na.rm = TRUE),
            mean_width = mean(width, na.rm = TRUE),
            mean_weight = mean(weight, na.rm = TRUE),
            .groups = 'drop')

mean_40 <- pool_40 %>%
  group_by(month, location, pool) %>%
  summarise(mean_length = mean(length, na.rm = TRUE),
            mean_width = mean(width, na.rm = TRUE),
            mean_weight = mean(weight, na.rm = TRUE),
            .groups = 'drop')

mean_60 <- pool_60 %>%
  group_by(month, location, pool) %>%
  summarise(mean_length = mean(length, na.rm = TRUE),
            mean_width = mean(width, na.rm = TRUE),
            mean_weight = mean(weight, na.rm = TRUE),
            .groups = 'drop')

# Reshape the summary data from wide to long format
df_20 <- mean_20 %>%
  pivot_longer(cols = c(mean_length, mean_width, mean_weight), 
               names_to = "Measurement", 
               values_to = "Mean_Value",
               names_prefix = "mean_")

df_40 <- mean_40 %>%
  pivot_longer(cols = c(mean_length, mean_width, mean_weight), 
               names_to = "Measurement", 
               values_to = "Mean_Value",
               names_prefix = "mean_")

df_60 <- mean_60 %>%
  pivot_longer(cols = c(mean_length, mean_width, mean_weight), 
               names_to = "Measurement", 
               values_to = "Mean_Value",
               names_prefix = "mean_")

df <- bind_rows(df_20, df_40, df_60)

ggplot(df, aes(x = month, y = Mean_Value, color = Measurement, group = interaction(Measurement, location))) +
  geom_line(aes(linetype = location)) + 
  geom_point(aes(shape = Measurement)) +
  labs(title = "Mean Growth Pattern (Length, Width, Weight) Over Time", 
       x = "Month", 
       y = "Mean Value",
       color = "Measurement",
       linetype = "Location") +
  theme_solarized_2() +
  theme(plot.title = element_text(size = 13)) +
  facet_wrap(~pool)

# Calculate the overall mean for each month and measurement across all pools
overall_mean <- df %>%
  group_by(month, pool, Measurement) %>%
  summarise(Mean_Value = mean(Mean_Value, na.rm = TRUE), .groups = 'drop')

ggplot(overall_mean, aes(x = month, y = Mean_Value, color = Measurement, group = Measurement)) +
  geom_line() + 
  geom_point(aes(shape = Measurement)) +
  labs(title = "Overall Mean Growth Pattern (Length, Width, Weight) Over Time", 
       x = "Month", 
       y = "Mean Value",
       color = "Measurement") +
  theme_solarized_2() +
  theme(plot.title = element_text(size = 13)) +
  facet_wrap(~ pool)

# Filter the overall_mean data for 'length' measurements
overall_mean_length <- overall_mean %>%
  filter(Measurement == "length") %>%
  group_by(month, pool) %>%
  summarise(mean_length = mean(Mean_Value, na.rm = TRUE), .groups = 'drop')

# Filter the overall_mean data for 'length' measurements
overall_mean_width <- overall_mean %>%
  filter(Measurement == "width") %>%
  group_by(month, pool) %>%
  summarise(mean_width = mean(Mean_Value, na.rm = TRUE), .groups = 'drop')

# Filter the overall_mean data for 'length' measurements
overall_mean_weight <- overall_mean %>%
  filter(Measurement == "weight") %>%
  group_by(month, pool) %>%
  summarise(mean_weight = mean(Mean_Value, na.rm = TRUE), .groups = 'drop')

