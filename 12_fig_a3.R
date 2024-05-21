############################################################################################################
##### Authors: Jad Moawad & Daniel Oesch
##### Project: The Myth of the Middle Class Squeeze: Employment and Income by Class in Six Western Countries, 1980-2020
##### Data: 	Luxembourg Income Study (LIS)
############################################################################################################

# Figure A.3: Annual mean change in household disposable income by class and decade, in % 

# Load required libraries
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyr)
library(ggpubr)

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
      (cntry == "us" & year %in% c(1978,1990, 2000, 2010, 2018)) |
      (cntry == "uk" & year %in% c(1998, 2000, 2010, 2018)) |
      (cntry == "fr" & year %in% c(1984,1989, 2000, 2010, 2018)) |
      (cntry == "es" & year %in% c(1980,1990, 2000, 2010, 2018)) |
      (cntry == "de" & year %in% c(1984, 1990, 2000, 2010, 2018)) |
      (cntry == "pl" & year %in% c(1999,2010,2018)))

# Convert 'cntry' column to factor
selected_data$cntry = as.factor(selected_data$cntry)       

# Calculate mean earnings by social class of the first and last modules for each country

# For disposable income
disposable_income <- selected_data %>%             
  group_by(cntry, year, class) %>%             
  summarise(mean_income = weighted.mean(disposable_income, hwgt, na.rm = TRUE)) 

data = as.data.frame(disposable_income)

# Remove any rows with missing data
data = na.omit(data)

# Create a function -------------------------------------------------------

# Required libraries
library(dplyr)
library(ggplot2)

# Remove any rows with missing data
data_df = na.omit(data)

# This function calculates mean income differences by decade
calculate_by_decade <- function(country, start_year, end_year) {
  # Filter data by country and year range
  filtered_data <- data_df %>%
    filter(cntry %in% c(country) & year >= start_year & year <= end_year)
  
  # Convert country codes to uppercase
  filtered_data$cntry = toupper(filtered_data$cntry)
  
  # Calculate the mean income differences
  summary_data = filtered_data %>%
    group_by(cntry, class) %>%
    arrange(year) %>%
    # Only consider the first and last year of each period
    filter(row_number() == 1 | row_number() == n()) %>%
    # Compute the annual change percentage in mean income
    group_by(cntry, class) %>%
    mutate(base_year = (mean_income - first(mean_income)) / first(mean_income),
           year_difference = (year - first(year))) %>%
    filter(row_number() == n()) %>%
    mutate(annual_chg = (base_year / year_difference) * 100)
  
  # Round off the annual change to 1 decimal place
  summary_data$annual_chg2 = round(summary_data$annual_chg, 1)
  
  # Modify country labels to include the year range
  summary_data$cntry = as.character(summary_data$cntry)
  summary_data$cntry[summary_data$cntry == toupper(country)] <- paste(toupper(country), start_year, "-", end_year)
  summary_data$cntry = as.factor(summary_data$cntry)
  
  # Reorder the class levels
  summary_data = summary_data %>% mutate(class = forcats::fct_relevel(class, "Low-skilled working class", "Skilled working class", "Middle class", "Upper-middle class"))
  
  return(summary_data)
}


# Germany -----------------------------------------------------------------

# Define countries and decades for analysis
countries <- c("de")
decades <- list(c(1984, 1990), c(1990, 2000), c(2000, 2010), c(2010, 2018))

# Compute results for each country and decade
results <- purrr::map_df(countries, function(country) {
  purrr::map_df(decades, ~calculate_by_decade(country, .x[1], .x[2]))
})

# Adjusting year for visualization purposes
results$year = 0

# To add a dashed vertical line between countries
results$end_of_country <- ifelse(results$year == max(results$year), 1, 0)

de=ggplot(results, aes(x=year, y=annual_chg, fill=class,color=class)) +
  geom_col(position = "dodge") +
  geom_text(aes(y = annual_chg + ifelse(annual_chg>=0,0.05, -1),label = annual_chg2), vjust =  -0.04, colour = "black",position = position_dodge(.9), size =4.5)+
  facet_wrap(~cntry, scales = "free_x",nrow=1)+  
  #guides(color = guide_legend(nrow = 2))+
  scale_y_continuous(limits = c(-2.5,6.7), breaks = seq(0,6, by = 2))+
  scale_color_manual(values = c("white","white","white","white"))+
  scale_fill_manual(values=c("#C91D42", "#f198ad","#6477d8","#1F2E7A"))+ 
  theme_classic()+ ylab(NULL)+xlab(NULL)+ ggtitle(NULL)+
  theme( 
    axis.title.x = element_text(color="Black", size=25),
    axis.title.y = element_text(color="Black", size=18, hjust = 0.5),
    plot.title = element_text(hjust = 0.5, color="Black", size=35),
    legend.text = element_text(color="Black", size=15),
    legend.title = element_blank(),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_text(color="black", size=16),
    strip.text = element_text(size=15),
    strip.background = element_rect(colour = "white", fill = "white"),
    plot.caption=element_text(hjust = 0, size=15),
    strip.placement = "outside",
    axis.ticks.x = element_blank(),
    axis.line.x.bottom = element_blank())+
  geom_vline(data = subset(results, end_of_country == 1 & cntry != tail(unique(results$cntry), n = 1)), aes(xintercept = year + 0.5), linetype="dashed", color = "black")

# Spain -----------------------------------------------------------------
countries <- c("es")
decades <- list(c(1980, 1990), c(1990, 2000), c(2000, 2010), c(2010, 2018))

results <- purrr::map_df(countries, function(country) {
  purrr::map_df(decades, ~calculate_by_decade(country, .x[1], .x[2]))
})

# Adjusting year for visualization purposes
results$year = 0

# To add a dashed vertical line between countries
results$end_of_country <- ifelse(results$year == max(results$year), 1, 0)

es=ggplot(results, aes(x=year, y=annual_chg, fill=class,color=class)) +
  geom_col(position = "dodge") +
  geom_text(aes(y = annual_chg + ifelse(annual_chg>=0,0.05, -1),label = annual_chg2), vjust =  -0.04, colour = "black",position = position_dodge(.9), size =4.5)+
  facet_wrap(~cntry, scales = "free_x",nrow=1)+  
  #guides(color = guide_legend(nrow = 2))+
  scale_y_continuous(limits = c(-3,6.7), breaks = seq(0,6, by = 2))+
  scale_color_manual(values = c("white","white","white","white"))+
  scale_fill_manual(values=c("#C91D42", "#f198ad","#6477d8","#1F2E7A"))+ 
  theme_classic()+ ylab(NULL)+xlab(NULL)+ ggtitle(NULL)+
  theme( 
    axis.title.x = element_text(color="Black", size=25),
    axis.title.y = element_text(color="Black", size=18, hjust = 0.5),
    plot.title = element_text(hjust = 0.5, color="Black", size=35),
    legend.text = element_text(color="Black", size=15),
    legend.title = element_blank(),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_text(color="black", size=16),
    strip.text = element_text(size=15),
    strip.background = element_rect(colour = "white", fill = "white"),
    plot.caption=element_text(hjust = 0, size=15),
    strip.placement = "outside",
    axis.ticks.x = element_blank(),
    axis.line.x.bottom = element_blank())+
  geom_vline(data = subset(results, end_of_country == 1 & cntry != tail(unique(results$cntry), n = 1)), aes(xintercept = year + 0.5), linetype="dashed", color = "black")

# France -----------------------------------------------------------------
countries <- c("fr")
decades <- list(c(1984, 1989), c(1989, 2000), c(2000, 2010), c(2010, 2018))

results <- purrr::map_df(countries, function(country) {
  purrr::map_df(decades, ~calculate_by_decade(country, .x[1], .x[2]))
})

# Adjusting year for visualization purposes
results$year = 0

# To add a dashed vertical line between countries
results$end_of_country <- ifelse(results$year == max(results$year), 1, 0)

fr=ggplot(results, aes(x=year, y=annual_chg, fill=class,color=class)) +
  geom_col(position = "dodge") +
  geom_text(aes(y = annual_chg + ifelse(annual_chg>=0,0.05, -1),label = annual_chg2), vjust =  -0.04, colour = "black",position = position_dodge(.9), size =4.5)+
  facet_wrap(~cntry, scales = "free_x",nrow=1)+  
  #guides(color = guide_legend(nrow = 2))+
  scale_y_continuous(limits = c(-3,6.7), breaks = seq(0,6, by = 2))+
  scale_color_manual(values = c("white","white","white","white"))+
  scale_fill_manual(values=c("#C91D42", "#f198ad","#6477d8","#1F2E7A"))+ 
  theme_classic()+ ylab(NULL)+xlab(NULL)+ ggtitle(NULL)+
  theme( 
    axis.title.x = element_text(color="Black", size=25),
    axis.title.y = element_text(color="Black", size=18, hjust = 0.5),
    plot.title = element_text(hjust = 0.5, color="Black", size=35),
    legend.text = element_text(color="Black", size=15),
    legend.title = element_blank(),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_text(color="black", size=16),
    strip.text = element_text(size=15),
    strip.background = element_rect(colour = "white", fill = "white"),
    plot.caption=element_text(hjust = 0, size=15),
    strip.placement = "outside",
    axis.ticks.x = element_blank(),
    axis.line.x.bottom = element_blank())+
  geom_vline(data = subset(results, end_of_country == 1 & cntry != tail(unique(results$cntry), n = 1)), aes(xintercept = year + 0.5), linetype="dashed", color = "black")

# Poland -----------------------------------------------------------------
countries <- c("pl")
decades <- list(c(1999, 2010), c(2010, 2018))

results <- purrr::map_df(countries, function(country) {
  purrr::map_df(decades, ~calculate_by_decade(country, .x[1], .x[2]))
})

# Create a data frame for Poland's missing decades to ensure each country displays
# 4 columns in visualizations for consistent comparison.

classes <- unique(results$class)
missing_decades <- expand.grid(
  cntry = c("PL missing","PL missing "),
  year = 0,
  class = classes,
  annual_chg2 = 0,
  stringsAsFactors = FALSE
)

# Merge the results with the data created for the missing decades 
results2 <- rbind(results, missing_decades)

# Adjusting year for visualization purposes
results2$year = 0

desired_order <- c("PL missing ", "PL missing", "PL 1999 - 2010", "PL 2010 - 2018")
results2$cntry <- factor(results2$cntry, levels = desired_order)

# To add a dashed vertical line between countries
results2$end_of_country <- ifelse(results2$year == max(results2$year), 1, 0)

pl=ggplot(results2, aes(x=year, y=annual_chg, fill=class,color=class)) +
  geom_col(position = "dodge") +
  geom_text(aes(y = annual_chg + ifelse(annual_chg>=0,0.05, -1),label = annual_chg2), vjust =  -0.04, colour = "black",position = position_dodge(.9), size =4.5)+
  facet_wrap(~cntry, scales = "free_x",nrow=1)+  
  #guides(color = guide_legend(nrow = 2))+
  scale_y_continuous(limits = c(-2.5,6.7), breaks = seq(0,6, by = 2))+
  scale_color_manual(values = c("white","white","white","white"))+
  scale_fill_manual(values=c("#C91D42", "#f198ad","#6477d8","#1F2E7A"))+ 
  theme_classic()+ ylab(NULL)+xlab(NULL)+ ggtitle(NULL)+
  theme( 
    axis.title.x = element_text(color="Black", size=25),
    axis.title.y = element_text(color="Black", size=18, hjust = 0.5),
    plot.title = element_text(hjust = 0.5, color="Black", size=35),
    legend.text = element_text(color="Black", size=15),
    legend.title = element_blank(),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_text(color="black", size=16),
    strip.text = element_text(size=15),
    strip.background = element_rect(colour = "white", fill = "white"),
    plot.caption=element_text(hjust = 0, size=15),
    strip.placement = "outside",
    axis.ticks.x = element_blank(),
    axis.line.x.bottom = element_blank())+
  geom_vline(data = subset(results2, cntry == "PL 1999 - 2010"), aes(xintercept = year + 0.5), linetype="dashed", color = "black")

# UK -----------------------------------------------------------------
countries <- c("uk")
decades <- list(c(1998, 2010), c(2010, 2018))

results <- purrr::map_df(countries, function(country) {
  purrr::map_df(decades, ~calculate_by_decade(country, .x[1], .x[2]))
})


# Create a data frame for UK's missing decades to ensure each country displays
# 4 columns in visualizations for consistent comparison.

classes <- unique(results$class)
missing_decades <- expand.grid(
  cntry = c("UK missing","UK missing "),
  year = 0,
  class = classes,
  annual_chg2 = 0,
  stringsAsFactors = FALSE
)

# Merge the results with the data created for the missing decades 
results2 <- rbind(results, missing_decades)

# Adjusting year for visualization purposes
results2$year = 0

desired_order <- c("UK missing ", "UK missing", "UK 1998 - 2010", "UK 2010 - 2018")
results2$cntry <- factor(results2$cntry, levels = desired_order)

# To add a dashed vertical line between countries
results2$end_of_country <- ifelse(results2$year == max(results2$year), 1, 0)

uk=ggplot(results2, aes(x=year, y=annual_chg, fill=class,color=class)) +
  geom_col(position = "dodge") +
  geom_text(aes(y = annual_chg + ifelse(annual_chg>=0,0.05, -1),label = annual_chg2), vjust =  -0.04, colour = "black",position = position_dodge(.9), size =4.5)+
  facet_wrap(~cntry, scales = "free_x",nrow=1)+  
  #guides(color = guide_legend(nrow = 2))+
  scale_y_continuous(limits = c(-2.5,6.7), breaks = seq(0,6, by = 2))+
  scale_color_manual(values = c("white","white","white","white"))+
  scale_fill_manual(values=c("#C91D42", "#f198ad","#6477d8","#1F2E7A"))+ 
  theme_classic()+ ylab(NULL)+xlab(NULL)+ ggtitle(NULL)+
  theme( 
    axis.title.x = element_text(color="Black", size=25),
    axis.title.y = element_text(color="Black", size=18, hjust = 0.5),
    plot.title = element_text(hjust = 0.5, color="Black", size=35),
    legend.text = element_text(color="Black", size=15),
    legend.title = element_blank(),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_text(color="black", size=16),
    strip.text = element_text(size=15),
    strip.background = element_rect(colour = "white", fill = "white"),
    plot.caption=element_text(hjust = 0, size=15),
    strip.placement = "outside",
    axis.ticks.x = element_blank(),
    axis.line.x.bottom = element_blank())+
  geom_vline(data = subset(results2, cntry == "UK 1998 - 2010"), aes(xintercept = year + 0.5), linetype="dashed", color = "black")

# US -----------------------------------------------------------------
countries <- c("us")
decades <- list(c(1978, 1990), c(1990, 2000), c(2000, 2010), c(2010, 2018))

results <- purrr::map_df(countries, function(country) {
  purrr::map_df(decades, ~calculate_by_decade(country, .x[1], .x[2]))
})

# Adjusting year for visualization purposes
results$year = 0

# To add a dashed vertical line between countries
results$end_of_country <- ifelse(results$year == max(results$year), 1, 0)

us=ggplot(results, aes(x=year, y=annual_chg, fill=class,color=class)) +
  geom_col(position = "dodge") +
  geom_text(aes(y = annual_chg + ifelse(annual_chg>=0,0.05, -1),label = annual_chg2), vjust =  -0.04, colour = "black",position = position_dodge(.9), size =4.5)+
  facet_wrap(~cntry, scales = "free_x",nrow=1)+  
  #guides(color = guide_legend(nrow = 2))+
  scale_y_continuous(limits = c(-2.5,6.7), breaks = seq(0,6, by = 2))+
  scale_color_manual(values = c("white","white","white","white"))+
  scale_fill_manual(values=c("#C91D42", "#f198ad","#6477d8","#1F2E7A"))+ 
  theme_classic()+ ylab(NULL)+xlab(NULL)+ ggtitle(NULL)+
  theme( 
    axis.title.x = element_text(color="Black", size=25),
    axis.title.y = element_text(color="Black", size=18, hjust = 0.5),
    plot.title = element_text(hjust = 0.5, color="Black", size=35),
    legend.text = element_text(color="Black", size=15),
    legend.title = element_blank(),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_text(color="black", size=16),
    strip.text = element_text(size=15),
    strip.background = element_rect(colour = "white", fill = "white"),
    plot.caption=element_text(hjust = 0, size=15),
    strip.placement = "outside",
    axis.ticks.x = element_blank(),
    axis.line.x.bottom = element_blank())+
  geom_vline(data = subset(results, end_of_country == 1 & cntry != tail(unique(results$cntry), n = 1)), aes(xintercept = year + 0.5), linetype="dashed", color = "black")

# merge all graphs together -----------------------------------------------

#Construct the right panel
plot <- ggarrange(de, es, fr, pl, uk, us, ncol=1, nrow=6,  
                      common.legend = TRUE, legend = "bottom")


