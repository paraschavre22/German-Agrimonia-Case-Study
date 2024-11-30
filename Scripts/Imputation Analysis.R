library(dplyr)
install.packages("ggplot2")
install.packages("scales")

library(ggplot2)
setwd(setwd("C:/Users/Asus/Downloads/Daniel Project"))
original_data <- read.csv("AgrImOnIAdataGE.csv")
original_data <- original_data %>%
  select(Latitude, Longitude, Altitude,
         AQ_pm10, AQ_pm25, AQ_co, AQ_nox, AQ_so2, WE_temp_2m,
         WE_wind_speed_10m_mean, WE_tot_precipitation,
         WE_solar_radiation, EM_nh3_livestock_mm, EM_nh3_agr_soils,
         EM_nh3_agr_waste_burn, EM_nox_traffic, LI_bovine, LA_land_use)


original_data %>%
  summarise_all(~sum(is.na(.)))
imputed_data <- read.csv("german_agrimonia_data_imputed.csv")

imputed_data <- imputed_data %>%
  select(Latitude, Longitude, Altitude,
         AQ_pm10, AQ_pm25, AQ_co, AQ_nox, AQ_so2, WE_temp_2m,
         WE_wind_speed_10m_mean, WE_tot_precipitation,
         WE_solar_radiation, EM_nh3_livestock_mm, EM_nh3_agr_soils,
         EM_nh3_agr_waste_burn, EM_nox_traffic, LI_bovine, LA_land_use)


# Assuming original_data is a data set containing the original values before imputation

# Convert factor columns to numeric
original_data[] <- lapply(original_data, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else as.numeric(x)
})
imputed_data[] <- lapply(imputed_data, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else as.numeric(x)
})

# Define MSE and RMSE functions
mse <- function(original_data, imputed_data) {
  if (!all(is.numeric(original_data) & is.numeric(imputed_data))) {
    stop("Both original_data and imputed_data must be numeric")
  }
  mean((original_data - imputed_data)^2, na.rm = TRUE)
}

rmse <- function(original_data, imputed_data) {
  sqrt(mse(original_data, imputed_data))
}

# Apply the function to each column
error_metrics <- sapply(names(imputed_data), function(col) {
  if (col %in% names(original_data)) {
    list(
      MSE = mse(original_data[[col]], imputed_data[[col]]),
      RMSE = rmse(original_data[[col]], imputed_data[[col]])
    )
  } else {
    list(MSE = NA, RMSE = NA)
  }
})
error_metrics


# Summary statistics
summary(imputed_data)

# Visualize distributions
library(ggplot2)

# Histograms for numeric variables
numeric_vars <- sapply(imputed_data, is.numeric)

for (var in names(imputed_data)[numeric_vars]) {
  print(ggplot(imputed_data, aes_string(var)) +
          geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
          ggtitle(paste("Distribution of", var)) +
          theme_minimal())
}


library(dplyr)

# Assume that there is a column `is_imputed` indicating whether the value was imputed
# Separate imputed and non-imputed data


# Perform KS test
ks_results <- sapply(names(imputed_data)[numeric_vars], function(var) {
  ks.test(imputed_data[[var]], original_data[[var]])
})

ks_results

# For variables where the p-value is close to 1
# (e.g., Latitude, Longitude, Altitude, WE_temp_2m, WE_wind_speed_10m_mean,
#   WE_tot_precipitation, WE_solar_radiation, and LI_bovine), the null hypothesis 
# cannot be rejected. This means that there is no significant difference between the 
# distributions of the imputed and original data for these variables.
# 
# For variables where the p-value is very close to 0 
# (e.g., AQ_pm10, AQ_pm25, AQ_co, AQ_nox, AQ_so2, EM_nh3_livestock_mm, EM_nh3_agr_soils,
#   EM_nh3_agr_waste_burn, EM_nox_traffic, and LA_land_use), the null hypothesis is rejected.
# This suggests that there are significant differences between the distributions of the
# imputed and original data for these variables.



