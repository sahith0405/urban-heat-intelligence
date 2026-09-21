# ============================================================
# Urban Heat Intelligence
# Script: 01_data_cleaning.R
# Purpose: Clean and validate NASA POWER spatial weather data
# ============================================================

library(tidyverse)
library(lubridate)

cat("\n============================================\n")
cat("URBAN HEAT INTELLIGENCE - DATA CLEANING\n")
cat("============================================\n")

# ------------------------------------------------------------
# 1. File paths
# ------------------------------------------------------------

input_file <- "data/raw/spatial/hyderabad_grid_weather.csv"

output_file <- "data/processed/hyderabad_weather_clean.csv"

# ------------------------------------------------------------
# 2. Load raw data
# ------------------------------------------------------------

cat("\n[1/8] Loading raw dataset...\n")

weather <- read_csv(
  input_file,
  show_col_types = FALSE
)

cat("Rows loaded:", nrow(weather), "\n")
cat("Columns loaded:", ncol(weather), "\n")

# ------------------------------------------------------------
# 3. Standardize column names
# ------------------------------------------------------------

cat("\n[2/8] Standardizing column names...\n")

weather <- weather %>%
  rename(
    location_id = location_id,
    latitude = latitude,
    longitude = longitude,
    date = date,
    temperature = T2M,
    temperature_max = T2M_MAX,
    temperature_min = T2M_MIN,
    humidity = RH2M,
    wind_speed = WS2M,
    solar_radiation = ALLSKY_SFC_SW_DWN
  )

# ------------------------------------------------------------
# 4. Data types
# ------------------------------------------------------------

cat("\n[3/8] Converting data types...\n")

weather <- weather %>%
  mutate(
    location_id = as.character(location_id),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    date = as.Date(date),
    temperature = as.numeric(temperature),
    temperature_max = as.numeric(temperature_max),
    temperature_min = as.numeric(temperature_min),
    humidity = as.numeric(humidity),
    wind_speed = as.numeric(wind_speed),
    solar_radiation = as.numeric(solar_radiation)
  )

# ------------------------------------------------------------
# 5. Remove invalid / missing observations
# ------------------------------------------------------------

cat("\n[4/8] Checking missing and invalid values...\n")

before_rows <- nrow(weather)

weather <- weather %>%
  filter(
    !is.na(location_id),
    !is.na(latitude),
    !is.na(longitude),
    !is.na(date),
    !is.na(temperature),
    !is.na(temperature_max),
    !is.na(temperature_min),
    !is.na(humidity),
    !is.na(wind_speed),
    !is.na(solar_radiation)
  )

after_missing <- nrow(weather)

cat(
  "Rows removed because of missing values:",
  before_rows - after_missing,
  "\n"
)

# ------------------------------------------------------------
# 6. Validate physical ranges
# ------------------------------------------------------------

cat("\n[5/8] Validating physical ranges...\n")

# Temperature consistency
invalid_temperature <- weather %>%
  filter(
    temperature_max < temperature_min |
    temperature < temperature_min |
    temperature > temperature_max
  )

cat(
  "Temperature consistency violations:",
  nrow(invalid_temperature),
  "\n"
)

# Humidity should normally be 0-100%
invalid_humidity <- weather %>%
  filter(
    humidity < 0 |
    humidity > 100
  )

cat(
  "Humidity violations:",
  nrow(invalid_humidity),
  "\n"
)

# Wind speed cannot be negative
invalid_wind <- weather %>%
  filter(
    wind_speed < 0
  )

cat(
  "Wind-speed violations:",
  nrow(invalid_wind),
  "\n"
)

# Solar radiation cannot be negative
invalid_solar <- weather %>%
  filter(
    solar_radiation < 0
  )

cat(
  "Solar-radiation violations:",
  nrow(invalid_solar),
  "\n"
)

# Keep only physically valid records
weather <- weather %>%
  filter(
    temperature_max >= temperature_min,
    temperature >= temperature_min,
    temperature <= temperature_max,
    humidity >= 0,
    humidity <= 100,
    wind_speed >= 0,
    solar_radiation >= 0
  )

# ------------------------------------------------------------
# 7. Add useful analytical features
# ------------------------------------------------------------

cat("\n[6/8] Creating analytical features...\n")

weather <- weather %>%
  mutate(
    year = year(date),
    month = month(date),
    month_name = month(date, label = TRUE, abbr = FALSE),
    day = day(date),
    day_of_year = yday(date),

    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Summer",
      month %in% c(6, 7, 8, 9) ~ "Monsoon",
      month %in% c(10, 11) ~ "Post-Monsoon",
      TRUE ~ "Unknown"
    ),

    temperature_range = temperature_max - temperature_min
  )

# ------------------------------------------------------------
# 8. Remove duplicate observations
# ------------------------------------------------------------

cat("\n[7/8] Checking duplicate observations...\n")

duplicate_count <- weather %>%
  count(location_id, date) %>%
  filter(n > 1) %>%
  nrow()

cat(
  "Duplicate location-date combinations:",
  duplicate_count,
  "\n"
)

weather <- weather %>%
  distinct(
    location_id,
    date,
    .keep_all = TRUE
  )

# ------------------------------------------------------------
# 9. Sort and export
# ------------------------------------------------------------

cat("\n[8/8] Sorting and exporting cleaned dataset...\n")

weather <- weather %>%
  arrange(
    location_id,
    date
  )

write_csv(
  weather,
  output_file
)

# ------------------------------------------------------------
# Final validation
# ------------------------------------------------------------

cat("\n============================================\n")
cat("FINAL DATASET VALIDATION\n")
cat("============================================\n")

cat("Rows:", nrow(weather), "\n")
cat("Columns:", ncol(weather), "\n")
cat("Locations:", n_distinct(weather$location_id), "\n")
cat("Start date:", as.character(min(weather$date)), "\n")
cat("End date:", as.character(max(weather$date)), "\n")

cat("\nMissing values:\n")
print(colSums(is.na(weather)))

cat("\nSeason distribution:\n")
print(
  weather %>%
    count(season)
)

cat("\nYear distribution:\n")
print(
  weather %>%
    count(year)
)

cat("\nTemperature summary:\n")
print(
  weather %>%
    summarise(
      min_temperature = min(temperature),
      mean_temperature = mean(temperature),
      median_temperature = median(temperature),
      max_temperature = max(temperature),
      sd_temperature = sd(temperature)
    )
)

cat("\nOutput file:\n")
cat(output_file, "\n")

cat("\n============================================\n")
cat("DATA CLEANING COMPLETED SUCCESSFULLY\n")
cat("============================================\n")
