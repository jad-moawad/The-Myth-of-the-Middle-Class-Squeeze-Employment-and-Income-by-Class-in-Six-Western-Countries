############################################################################################################
##### Authors: Jad Moawad & Daniel Oesch
##### Project: The Myth of the Middle Class Squeeze: Employment and Income by Class in Six Western Countries, 1980-2020
##### Data: 	Luxembourg Income Study (LIS)
############################################################################################################

# Table A.2: Descriptive statistics 

# Table mean income by country --------------------------------------------
# Load necessary libraries
library(dplyr)
library(tidyr)

# Read data
# data_path <- paste(USR_DIR, "/path/to/your/directory", sep="")
# raw_data <- read.dta13(data_path, convert.factors=TRUE)

# Read data
data_path <- paste(USR_DIR, "/jmoawa/reg.dta", sep="")
raw_data <- read.dta13(data_path, convert.factors=TRUE)

# Subset the data with specified conditions
filtered_data <- subset(raw_data, select = c(cntry, class, year, disposable_income, labor_income, nhhmem, hwgt), 
                        age >= 25 & age <= 60 & labor_income > 0 & disposable_income > 0)

# Keep the same analytical sample
filtered_data <- filtered_data %>%
  na.omit()

# Filter data for specific countries and years
selected_data <- filtered_data %>%
  filter(
    (cntry == "us" & year %in% c(1978, 2018)) |
      (cntry == "uk" & year %in% c(1998, 2018)) |
      (cntry == "fr" & year %in% c(1984, 2018)) |
      (cntry == "es" & year %in% c(1980, 2018)) |
      (cntry == "de" & year %in% c(1984, 2018)) |
      (cntry == "pl" & year %in% c(1999, 2018)))

# Convert 'cntry' column to factor
selected_data$cntry = as.factor(selected_data$cntry)       

# Calculate mean earnings by social class of the first and last modules for each country
# For disposable income
disposable_income <- selected_data %>%             
  group_by(cntry, year) %>%             
  summarise(mean_income = weighted.mean(disposable_income, hwgt, na.rm = TRUE)) 
data <- as.data.frame(disposable_income)

# Print data
head(data, 50)

# Class composition -------------------------------------------------------
library(dplyr)
library(readstata13)  

# Read data
# data_path <- paste(USR_DIR, "/path/to/your/directory", sep="")
# raw_data <- read.dta13(data_path, convert.factors=TRUE)

# Read data
data_path <- paste(USR_DIR, "/jmoawa/reg.dta", sep = "")
raw_data <- read.dta13(data_path, convert.factors = TRUE)

# Subset the data with specified conditions
filtered_data <- raw_data %>%
  filter(age >= 25 & age <= 60 & labor_income > 0 & disposable_income > 0) %>%
  select(cntry, class, year, disposable_income, labor_income, nhhmem, hwgt)

# Keep the same analytical sample
filtered_data <- filtered_data %>%
  na.omit()

# Select countries of interest and remove rows with missing values
selected_countries_data <- filtered_data %>%
  filter(cntry %in% c("es", "de", "us", "uk", "pl", "fr")) %>%
  mutate(cntry = as.factor(cntry))

# Summarize the data by grouping
final_data <- selected_countries_data %>%
  group_by(cntry, year, class) %>%
  summarise(n = n()) %>%
  mutate(t = (n / sum(n)) * 100)

# Print data
head(data, 50)

# Household size -------------------------------------------------------
library(dplyr)   
library(tidyr)
library(readstata13)  

# Read data
# data_path <- paste(USR_DIR, "/path/to/your/directory", sep="")
# raw_data <- read.dta13(data_path, convert.factors=TRUE)

# Read data
data_path <- paste(USR_DIR, "/jmoawa/reg.dta", sep = "")
raw_data <- read.dta13(data_path, convert.factors = TRUE)

# Subset the data with specified conditions
filtered_data <- raw_data %>%
  filter(age >= 25 & age <= 60 & labor_income > 0 & disposable_income > 0) %>%
  select(cntry, class, year, labor_income, disposable_income, nhhmem, hwgt)

# Keep the same analytical sample
filtered_data <- filtered_data %>%
  na.omit()

# Filter data for specific countries and years
selected_data <- filtered_data %>%
  filter(
    (cntry == "us" & year %in% c(1978, 2018)) |
      (cntry == "uk" & year %in% c(1998, 2018)) |
      (cntry == "fr" & year %in% c(1984, 2018)) |
      (cntry == "es" & year %in% c(1980, 2018)) |
      (cntry == "de" & year %in% c(1984, 2018)) |
      (cntry == "pl" & year %in% c(1999, 2018))
  )

# Convert 'cntry' column to factor
selected_data <- selected_data %>%
  mutate(cntry = as.factor(cntry))

# Keep the same analytical sample
selected_data <- selected_data %>%
  na.omit()

# Calculate mean household members by country and year
t <- selected_data %>%
  group_by(cntry, year) %>%
  summarise(mean_nhhmem = mean(nhhmem, na.rm = TRUE))

# Convert to data frame and drop levels
data <- as.data.frame(t)
data <- droplevels(data)

# Print data
head(data, 50)

# % of Male and Female -------------------------------------------------------

library(dplyr)
library(tidyr)

# Read data
# data_path <- paste(USR_DIR, "/path/to/your/directory", sep="")
# raw_data <- read.dta13(data_path, convert.factors=TRUE)

# Read data
data_path <- paste(USR_DIR, "/jmoawa/reg.dta", sep="")
raw_data <- read.dta13(data_path, convert.factors=TRUE)

# Subset the data with specified conditions
filtered_data <- subset(raw_data, select = c(cntry, head_sex, year, labor_income, disposable_income, nhhmem, hwgt), 
                        age >= 25 & age <= 60 & labor_income > 0 & disposable_income > 0)

# Keep the same analytical sample
filtered_data <- filtered_data %>%
  na.omit()

# Filter data for specific countries and years
selected_data <- filtered_data %>%
  filter(
    (cntry == "us" & year %in% c(1978, 2018)) |
      (cntry == "uk" & year %in% c(1998, 2018)) |
      (cntry == "fr" & year %in% c(1984, 2018)) |
      (cntry == "es" & year %in% c(1980, 2018)) |
      (cntry == "de" & year %in% c(1984, 2018)) |
      (cntry == "pl" & year %in% c(1999, 2018)))

# Convert 'cntry' column to factor
selected_data$cntry = as.factor(selected_data$cntry)  

percentage <- selected_data %>%
  group_by(cntry, year, head_sex) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)
data <- as.data.frame(percentage)

# Print data
head(data, 50)

# Mean age ----------------------------------------------------------------
library(dplyr)
library(tidyr)

# Read data
# data_path <- paste(USR_DIR, "/path/to/your/directory", sep="")
# raw_data <- read.dta13(data_path, convert.factors=TRUE)

# Read data
data_path <- paste(USR_DIR, "/jmoawa/reg.dta", sep="")
raw_data <- read.dta13(data_path, convert.factors=TRUE)

# Subset the data with specified conditions
filtered_data <- subset(raw_data, select = c(cntry, class, year, disposable_income, age, labor_income, nhhmem, hwgt), 
                        age >= 25 & age <= 60 & labor_income > 0 & disposable_income > 0)

# Keep the same analytical sample
filtered_data <- filtered_data %>%
  na.omit()

# Filter data for specific countries and years
selected_data <- filtered_data %>%
  filter(
    (cntry == "us" & year %in% c(1978, 2018)) |
      (cntry == "uk" & year %in% c(1998, 2018)) |
      (cntry == "fr" & year %in% c(1984, 2018)) |
      (cntry == "es" & year %in% c(1980, 2018)) |
      (cntry == "de" & year %in% c(1984, 2018)) |
      (cntry == "pl" & year %in% c(1999, 2018)))

# Convert 'cntry' column to factor
selected_data$cntry = as.factor(selected_data$cntry)       

# Calculate mean earnings by social class of the first and last modules for each country
# For disposable income
age_data <- selected_data %>%             
  group_by(cntry, year) %>%             
  summarise(mean_age = weighted.mean(age, hwgt, na.rm = TRUE)) 
data <- as.data.frame(age_data)

# Print data
head(data, 50)

# Observations per country ------------------------------------------------

library(dplyr)
library(tidyr)

# Read data
# data_path <- paste(USR_DIR, "/path/to/your/directory", sep="")
# raw_data <- read.dta13(data_path, convert.factors=TRUE)

# Read data
data_path <- paste(USR_DIR, "/jmoawa/reg.dta", sep="")
raw_data <- read.dta13(data_path, convert.factors=TRUE)

# Subset the data with specified conditions
filtered_data <- subset(raw_data, select = c(cntry, year, labor_income, disposable_income, nhhmem, hwgt), 
                        age >= 25 & age <= 60 & labor_income > 0 & disposable_income > 0)

# Keep the same analytical sample
filtered_data <- filtered_data %>%
  na.omit()

# Filter data for specific countries and years
selected_data <- filtered_data %>%
  filter(
      (cntry == "us" & year %in% c(1978, 2018)) |
      (cntry == "uk" & year %in% c(1998, 2018)) |
      (cntry == "fr" & year %in% c(1984, 2018)) |
      (cntry == "es" & year %in% c(1980, 2018)) |
      (cntry == "de" & year %in% c(1984, 2018)) |
      (cntry == "pl" & year %in% c(1999, 2018)))

# Convert 'cntry' column to factor
selected_data$cntry = as.factor(selected_data$cntry)  

# Calculate the number of observations by did and year
data <- selected_data %>%
  group_by(year, cntry) %>%
  summarise(observations = n())
data <- as.data.frame(data)

# Print data
head(data, 50)

