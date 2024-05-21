############################################################################################################
##### Authors: Jad Moawad & Daniel Oesch
##### Project: The Myth of the Middle Class Squeeze: Employment and Income by Class in Six Western Countries, 1980-2020
##### Data: 	Luxembourg Income Study (LIS)
############################################################################################################

# Figure B.3: evolution of indexed real household labor income by social class over four decades 

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
filtered_data <- subset(raw_data, select = c(cntry, class, class_scheme_5, year, labor_income, disposable_income, nhhmem, hwgt), 
                        age >= 25 & age <= 60 & labor_income > 0 & disposable_income > 0)

# Keep the same analytical sample
filtered_data <- filtered_data %>%
  na.omit()

# Subset the data with specified conditions
selected_countries <- filtered_data %>%
  filter(cntry %in% c("es", "de", "us", "uk", "pl", "fr")) 

# Calculate mean earnings of class over time using weights
class_earnings_summary <- selected_countries %>%
  group_by(cntry, year, class_scheme_5) %>%
  summarise(mean_earning = weighted.mean(disposable_income, hwgt, na.rm = TRUE)) %>%
  ungroup()

# Convert the result to a data frame and drop unused levels
data <- as.data.frame(class_earnings_summary)

# Convert country codes to uppercase for consistency
data$cntry = toupper(data$cntry)

# Reorder class levels in the data
data =  data %>% mutate(class_scheme_5 = fct_relevel(class_scheme_5, "Upper-middle class", "Middle class", "Lower-middle class", "Skilled working class", "Low-skilled working class")) 

# Create a summary dataset that calculates the mean earnings of each module relative to the base year.
# The base year is defined as the first year in the dataset for each country, and the mean earnings for 
# the "Low-skilled working class" in that year are set to 100. 
summary_data = data %>%
  group_by(cntry) %>%
  mutate(base_year = (mean_earning / first(mean_earning[class_scheme_5 == "Low-skilled working class"])) * 100)

# Plot
summary_data %>% 
  ggplot(aes(x=year, y=base_year, color=class_scheme_5))+
  geom_line(linewidth=1.5)+
  geom_hline(yintercept=100, linetype="dashed", color = "black")+
  scale_y_continuous(limits = c(88, 386), breaks = seq(100, 300, by = 100)) +
  scale_x_continuous(limits = c(1978, 2018), breaks = seq(1980, 2020, by = 10)) +
  facet_wrap(~cntry, scales = "free_x")+
  theme_minimal()+
  ylab(NULL) + xlab(NULL) +
  scale_color_manual(values=c("#1F2E7A","#6477d8","#D9D9D9","#f198ad","#C91D42"))+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size=20),
        panel.background = element_rect(fill = "white"), 
        panel.grid.minor = element_blank(),
        panel.margin = unit(1.2, "lines"),
        text = element_text(size=25),
        axis.title.y = element_text(size=20),
        axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=16),
        axis.text.x = element_text(size=16),
        legend.title = element_blank(), 
        legend.text = element_text(size=16),
        strip.background = element_blank(),
        panel.border = element_blank()) 





