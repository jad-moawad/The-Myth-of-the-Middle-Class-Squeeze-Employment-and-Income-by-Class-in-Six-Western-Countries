############################################################################################################
##### Authors: Jad Moawad & Daniel Oesch
##### Project: The Myth of the Middle Class Squeeze: Employment and Income by Class in Six Western Countries, 1980-2020
##### Data: 	Luxembourg Income Study (LIS)
############################################################################################################

# Figure 6: annual mean change in household disposable income based on a five-class schema, in % 

# Load required libraries
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyr)

# Read data
# data_path <- paste(USR_DIR, "/path/to/your/directory", sep="")
# raw_data <- read.dta13(data_path, convert.factors=TRUE)

# Read data
data_path <- paste(USR_DIR, "/jmoawa/reg.dta", sep="")
raw_data <- read.dta13(data_path, convert.factors=TRUE)

# Subset the data with specified conditions
filtered_data <- subset(raw_data, select = c(cntry, class_scheme_5, year, labor_income, disposable_income, nhhmem, hwgt), 
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

# Calculate mean earnings by social class of the first and last modules for each country for disposable income
data <- selected_data %>%             
  group_by(cntry, year, class_scheme_5) %>%             
  summarise(mean_income = weighted.mean(disposable_income, hwgt, na.rm = TRUE)) %>%
  mutate(income_type = "Disposable income")

data <- as.data.frame(data)

# Remove rows with missing data
data = na.omit(df)

# Filter the data for specified countries and only include "Disposable income" type
disposable_income <- data %>%
  filter((cntry %in% c("de", "es", "pl", "fr", "uk", "us")),
         income_type == "Disposable income")

# Convert country codes to uppercase
disposable_income$cntry = toupper(disposable_income$cntry)

# Get the first and last entry for each country and class combination, based on year
summary_disposable = disposable_income %>%
  group_by(cntry, class_scheme_5) %>%
  arrange(year) %>%
  filter(row_number() == 1 | row_number() == n())

# Calculate the base_year as the relative change from the first year's mean income
# Calculate year_difference as the number of years since the first occurrence
summary_disposable = summary_disposable %>%
  group_by(cntry, class_scheme_5) %>%
  mutate(base_year = (mean_income - first(mean_income)) / first(mean_income),
         year_difference = (year - first(year))) %>%
  filter(row_number() == n())

# Calculate annual change based on the base year and year difference
summary_disposable = summary_disposable %>%
  group_by(cntry, class_scheme_5) %>%
  mutate(annual_chg = (base_year / year_difference) * 100)

# Round the annual change to 1 decimal point
summary_disposable$annual_chg2 = round(summary_disposable$annual_chg, 1)

# Convert 'cntry' column to factor 
summary_disposable$cntry = as.factor(summary_disposable$cntry)

# Recode the country variable to add date ranges
summary_disposable = summary_disposable %>% dplyr::mutate(cntry = fct_recode(cntry, 
                                                                             "DE, 1984-2018" = "DE", 
                                                                             "ES, 1980-2018" = "ES",
                                                                             "FR, 1984-2018" = "FR",  
                                                                             "PL, 1999-2018" = "PL", 
                                                                             "UK, 1998-2018" = "UK",
                                                                             "US, 1978-2018" = "US"))

# Reorder class levels in the desired order
summary_disposable = summary_disposable %>% mutate(class_scheme_5 = forcats::fct_relevel(class_scheme_5, "Low-skilled working class", "Skilled working class", "Lower-middle class", "Middle class", "Upper-middle class")) 

# Set the number and position of the dashed vertical lines in the figure
summary_disposable$end_of_country <- ifelse(summary_disposable$year == max(summary_disposable$year), 1, 0)

# Plot
ggplot(summary_disposable, aes(x = year, y = annual_chg, fill = class_scheme_5, color = class_scheme_5)) +
  geom_col(position = "dodge") +
  geom_text(aes(y = annual_chg + ifelse(annual_chg >= 0, 0.05, -0.2), label = annual_chg2), vjust =  -0.04, colour = "black", position = position_dodge(.9), size = 4.5) +
  facet_wrap(~cntry, scales = "free_x", nrow = 1) +  
  scale_y_continuous(limits = c(-0.5, 6.7), breaks = seq(0, 6, by = 2)) +
  scale_color_manual(values = c("white", "white","white", "white", "white")) +
  scale_fill_manual(values = c("#C91D42", "#f198ad","#D9D9D9", "#6477d8", "#1F2E7A")) + 
  theme_classic() + 
  ylab("Household disposable income") +
  xlab(NULL) +
  theme( 
    axis.title.x = element_text(color="Black", size=15),
    axis.title.y = element_text(color="Black", size=15, hjust = 0.5),
    plot.title = element_text(hjust = 0.5, color="Black", size=20),
    legend.text = element_text(color="Black", size=14),
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.text.y = element_text(color="black", size=14),
    strip.text = element_text(size=14),
    strip.background = element_rect(colour = "white", fill = "white"),
    plot.caption=element_text(hjust = 0, size=25),
    strip.placement = "outside",
    axis.ticks.x = element_blank(),
    axis.line.x.bottom = element_blank())+
  geom_vline(data = subset(summary_disposable, end_of_country == 1 & cntry != tail(unique(summary_disposable$cntry), n = 1)), aes(xintercept = year + 0.5), linetype="dashed", color = "black")


