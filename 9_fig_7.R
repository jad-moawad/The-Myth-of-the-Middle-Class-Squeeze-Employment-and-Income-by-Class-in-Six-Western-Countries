############################################################################################################
##### Authors: Jad Moawad & Daniel Oesch
##### Project: The Myth of the Middle Class Squeeze: Employment and Income by Class in Six Western Countries, 1980-2020
##### Data: 	Luxembourg Income Study (LIS)
############################################################################################################

# Figure 7: Annual change in household disposable income in small European countries, in %  

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

# Keep the same analytical sample
filtered_data <- filtered_data %>%
  na.omit()

# Filter data for specific countries and years
selected_data <- filtered_data %>%
  filter(
      (cntry == "at" & year %in% c(1994, 2018)) |
      (cntry == "ch" & year %in% c(1992, 2018)) |
      (cntry == "dk" & year %in% c(1992, 2018)) |
      (cntry == "fi" & year %in% c(1987, 2016)) |
      (cntry == "ie" & year %in% c(1994, 2018)) |
      (cntry == "nl" & year %in% c(1990, 2018)))

# Convert 'cntry' column to factor
selected_data$cntry = as.factor(selected_data$cntry)       

# Calculate mean earnings by social class of the first and last modules for each country for disposable income
disposable_income <- selected_data %>%             
  group_by(cntry, year, class) %>%             
  summarise(mean_income = weighted.mean(disposable_income, hwgt, na.rm = TRUE)) 

data <- as.data.frame(disposable_income)

# Remove rows with missing data
initial_data = na.omit(data)

# Convert country codes to uppercase
initial_data$cntry = toupper(initial_data$cntry)

# Get the first and last entry for each country and class combination, based on year
summary_disposable = initial_data %>%
  group_by(cntry, class) %>%
  arrange(year) %>%
  filter(row_number() == 1 | row_number() == n())

# Calculate the base_year as the relative change from the first year's mean income
# Calculate year_difference as the number of years since the first occurrence
summary_disposable = summary_disposable %>%
  group_by(cntry, class) %>%
  mutate(base_year = (mean_income - first(mean_income)) / first(mean_income),
         year_difference = (year - first(year))) %>%
  filter(row_number() == n())

# Calculate annual change based on the base year and year difference
summary_disposable = summary_disposable %>%
  group_by(cntry, class) %>%
  mutate(annual_chg = (base_year / year_difference) * 100)

# Round the annual change to 1 decimal point
summary_disposable$annual_chg2 = round(summary_disposable$annual_chg, 1)

# Convert 'cntry' column to factor 
summary_disposable$cntry = as.factor(summary_disposable$cntry)

# Recode the country variable to add date ranges
summary_disposable = summary_disposable %>% dplyr::mutate(cntry = fct_recode(cntry, 
                                                                             "AT, 1994-2018" = "AT", 
                                                                             "CH, 1992-2018" = "CH",
                                                                             "DK, 1992-2018" = "DK",  
                                                                             "FI, 1987-2016" = "FI", 
                                                                             "IE, 1994-2018" = "IE",
                                                                             "NL, 1990-2018" = "NL"))

# Reorder class in the desired order
summary_disposable = summary_disposable %>% mutate(class = forcats::fct_relevel(class, "Low-skilled working class", "Skilled working class", "Middle class", "Upper-middle class")) 

# Helps with aesthetics plotting the figure
summary_disposable$year = 0

# Set the number and position of the dashed vertical lines in the figure
summary_disposable$end_of_country <- ifelse(summary_disposable$year == max(summary_disposable$year), 1, 0)

# Plotting the data
ggplot(summary_disposable, aes(x = year, y = annual_chg, fill = class, color = class)) +
  geom_col(position = "dodge") +
  geom_text(aes(y = annual_chg + ifelse(annual_chg >= 0, 0.025, -0.1), label = annual_chg2), vjust =  -0.04, colour = "black", position = position_dodge(.9), size = 4.5) +
  facet_wrap(~cntry, scales = "free_x", nrow = 1) +  
  scale_y_continuous(limits = c(-0.5, 3.2), breaks = seq(0, 3, by = 1)) +
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
    axis.line.x.bottom = element_blank()) +
  geom_vline(data = subset(summary_disposable, end_of_country == 1 & cntry != tail(unique(summary_disposable$cntry), n = 1)), aes(xintercept = year + 0.5), linetype="dashed", color = "black")
