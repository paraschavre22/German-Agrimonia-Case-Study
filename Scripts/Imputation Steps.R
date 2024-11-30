library(readr)
library(dplyr)
library(tidyr)
library(mice)
library(tidyverse)
library(missForest)
library(MatchIt)
library(caret)

setwd("C:/Users/Asus/Downloads/Daniel Project")
data_ga <- read.csv("AgrImOnIAdataGE.csv")
str(data_ga)
summary(data_ga)
german_agrimonia_data <- data_ga %>%
  select(IDStations, Latitude, Longitude, Time, Altitude,
         AQ_pm10, AQ_pm25, AQ_co, AQ_nox, AQ_so2, WE_temp_2m,
         WE_wind_speed_10m_mean, WE_tot_precipitation,
         WE_solar_radiation, EM_nh3_livestock_mm, EM_nh3_agr_soils,
         EM_nh3_agr_waste_burn, EM_nox_traffic, LI_bovine, LA_land_use)
summary(german_agrimonia_data)


german_agrimonia_data$IDStations <- as.factor(german_agrimonia_data$IDStations)
german_agrimonia_data$Time <- as.factor(german_agrimonia_data$Time)
summary(german_agrimonia_data)

missing_counts <- german_agrimonia_data %>%
  summarise_all(~sum(is.na(.)))

# Print the missing value counts
View(german_agrimonia_data)

# vars_to_impute <- c('EM_nh3_livestock_mm' ,'EM_nh3_agr_soils',
#                     'EM_nh3_agr_waste_burn','EM_nox_traffic', 
#                     'AQ_pm10', 'AQ_pm25', 'AQ_co','AQ_nox' , 'AQ_so2', "LI_bovine")
# 
# complete_data <- german_agrimonia_data[complete.cases(german_agrimonia_data), ]
# missing_data <- german_agrimonia_data[!complete.cases(german_agrimonia_data), ]
# 
# # Regression imputation loop
# for (var in vars_to_impute) {
#   model <- train(get(var) ~ . , data = complete_data, method = 'lm')
#   
#   missing_data[var][is.na(missing_data[var])] <- predict(model, newdata = missing_data)
#   
# }
# 
# any(is.na(complete_data))
# any(is.na(missing_data))
# str(missing_data)

# Load necessary library
library(mice)

# Load necessary library
library(mice)

# Define the variable you want to impute
variable_to_impute <- c('EM_nh3_livestock_mm' ,'EM_nh3_agr_soils', 'EM_nh3_agr_waste_burn','EM_nox_traffic', 'LI_bovine')

# Create a copy of the original data
data_copy <- german_agrimonia_data

# Impute missing values for the specified variable using PMM
imputed_data <- complete(mice(data_copy[, variable_to_impute, drop = FALSE], 
                              method = "pmm", seed = 123))

# Replace the missing values in the original dataset with the imputed values
german_agrimonia_data$EM_nh3_livestock_mm[is.na(german_agrimonia_data$EM_nh3_livestock_mm)] <- imputed_data$EM_nh3_livestock_mm
german_agrimonia_data$EM_nh3_agr_soils[is.na(german_agrimonia_data$EM_nh3_agr_soils)] <- imputed_data$EM_nh3_agr_soils
german_agrimonia_data$EM_nh3_agr_waste_burn[is.na(german_agrimonia_data$EM_nh3_agr_waste_burn)] <- imputed_data$EM_nh3_agr_waste_burn
german_agrimonia_data$EM_nox_traffic[is.na(german_agrimonia_data$EM_nox_traffic)] <- imputed_data$EM_nox_traffic
german_agrimonia_data$LI_bovine[is.na(german_agrimonia_data$LI_bovine)] <- imputed_data$LI_bovine
# Check for missing values in the imputed variable
any(is.na(german_agrimonia_data$LI_bovine))
str(german_agrimonia_data)
#-----------------------------------------------------------------------------------------------------------------------------------
variable_to_impute2 <- c('AQ_pm10', 'AQ_pm25', 'AQ_co','AQ_nox' , 'AQ_so2')


data_copy2 <- german_agrimonia_data

# Impute missing values for the specified variable using PMM
imputed_data2 <- complete(mice(data_copy2[, variable_to_impute2, drop = FALSE], 
                              method = "pmm", seed = 123))

german_agrimonia_data$AQ_pm10[is.na(german_agrimonia_data$AQ_pm10)] <- imputed_data2$AQ_pm10
german_agrimonia_data$AQ_pm25[is.na(german_agrimonia_data$AQ_pm25)] <- imputed_data2$AQ_pm25
german_agrimonia_data$AQ_co[is.na(german_agrimonia_data$AQ_co)] <- imputed_data2$AQ_co
german_agrimonia_data$AQ_nox[is.na(german_agrimonia_data$AQ_nox)] <- imputed_data2$AQ_nox
german_agrimonia_data$AQ_so2[is.na(german_agrimonia_data$AQ_so2)] <- imputed_data2$AQ_so2


summary(german_agrimonia_data)
# Assuming imputed_data2 contains your imputed dataset
write.csv(german_agrimonia_data, file = "german_agrimonia_data_imputed.csv", row.names = FALSE)

# Check for missing values in the dataset
missing_values <- colSums(is.na(german_agrimonia_data))

# Display the number of missing values for each column
print(missing_values)

# Check if there are any missing values in the dataset
if (sum(missing_values) > 0) {
  print("There are missing values in the dataset.")
} else {
  print("There are no missing values in the dataset.")
}

View(german_agrimonia_data)
