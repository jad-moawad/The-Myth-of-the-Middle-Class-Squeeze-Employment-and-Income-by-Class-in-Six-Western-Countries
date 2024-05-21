############################################################################################################
##### Authors: Jad Moawad & Daniel Oesch
##### Project: The Myth of the Middle Class Squeeze: Employment and Income by Class in Six Western Countries, 1980-2020
##### Data: 	Luxembourg Income Study (LIS)
############################################################################################################

# Figure A.2: Annual mean change in household disposable income by income quartile, in %

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
filtered_data <- subset(raw_data, select = c(cntry, class, year, disposable_income, labor_income, nhhmem, hwgt), 
                        age >= 25 & age <= 60 & labor_income > 0 & disposable_income > 0)

# Keep the same analytical
filtered_data <- drop_na(filtered_data, cntry, class, year, labor_income, disposable_income, nhhmem, hwgt)

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

# Create a quartile column 
selected_data <- selected_data %>%
  group_by(cntry, year) %>%
  mutate(quartile = cut(disposable_income,breaks = quantile(disposable_income, c(0, 0.25, 0.5, 0.75, 1)),labels = 1:4, include.lowest = TRUE))

# Label the quartile levels 
selected_data$quartile <- factor(selected_data$quartile, levels = c(1,2,3,4), labels = c("Bottom quartile", "Second quartile", "Third quartile", "Top quartile"))

# Calculate mean earnings by quartile of the first and last modules for each country
disposable_income = selected_data %>%             
  group_by(cntry, year, quartile) %>%             
  summarise(mean_income = weighted.mean(disposable_income, hwgt, na.rm = TRUE))

data = as.data.frame(disposable_income)

# Remove rows with missing data
data = na.omit(df)

# Convert country codes to uppercase
data$cntry = toupper(data$cntry)

# Get the first and last entry for each country and class combination, based on year
summary_quartile = data %>%
  group_by(cntry, quartile) %>%
  arrange(year) %>%
  filter(row_number() == 1 | row_number() == n())

# Calculate base_year which is a difference from mean of the year and the first mean of the year
# also, calculate the difference between the year and its first occurrence
summary_quartile = summary_quartile %>%
  group_by(cntry, quartile) %>%
  mutate(base_year = (mean_income - first(mean_income)) / first(mean_income),
         year_difference = (year - first(year))) %>%
  filter(row_number() == n())

# Calculate annual change based on the base year and year difference
summary_quartile = summary_quartile %>%
  group_by(cntry, quartile) %>%
  mutate(annual_chg = (base_year / year_difference) * 100)

# Round the annual change to 1 decimal point
summary_quartile$annual_chg2 = round(summary_quartile$annual_chg, 1)

# Convert 'cntry' column to factor 
summary_quartile$cntry = as.factor(summary_quartile$cntry)

# Recode the country variable to add date ranges
summary_quartile = summary_quartile %>% dplyr::mutate(cntry = fct_recode(cntry, 
                                                                             "DE, 1984-2018" = "DE", 
                                                                             "ES, 1980-2018" = "ES",
                                                                             "FR, 1984-2018" = "FR",  
                                                                             "PL, 1999-2018" = "PL", 
                                                                             "UK, 1998-2018" = "UK",
                                                                             "US, 1978-2018" = "US"))

# Reorder quartile levels in the desired order
summary_quartile = summary_quartile %>% mutate(quartile = forcats::fct_relevel(quartile, "Bottom quartile", "Second quartile", "Third quartile", "Top quartile")) 

# To add a dashed vertical line between countries
summary_quartile$end_of_country <- ifelse(summary_quartile$year == max(summary_quartile$year), 1, 0)

# Plotting the data
ggplot(summary_quartile, aes(x = year, y = annual_chg, fill = quartile, color = quartile)) +
  geom_col(position = "dodge") +
  geom_text(aes(y = annual_chg + ifelse(annual_chg >= 0, 0.05, -0.2), label = annual_chg2), vjust =  -0.04, colour = "black", position = position_dodge(.9), size = 4.5) +
  facet_wrap(~cntry, scales = "free_x", nrow = 1) +  
  scale_y_continuous(limits = c(-0.5, 6.7), breaks = seq(0, 6, by = 2)) +
  scale_color_manual(values = c("white", "white", "white", "white")) +
  scale_fill_manual(values = c("#C91D42", "#f198ad", "#6477d8", "#1F2E7A")) + 
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
  geom_vline(data = subset(summary_quartile, end_of_country == 1 & cntry != tail(unique(summary_quartile$cntry), n = 1)), aes(xintercept = year + 0.5), linetype="dashed", color = "black")

