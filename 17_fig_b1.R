############################################################################################################
##### Authors: Jad Moawad & Daniel Oesch
##### Project: The Myth of the Middle Class Squeeze: Employment and Income by Class in Six Western Countries, 1980-2020
##### Data: 	Luxembourg Income Study (LIS)
############################################################################################################

# Figure B.1: the class composition of the workforce over time (in %)

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

# Select countries of interest
selected_countries_data = filtered_data %>% 
  filter(cntry %in% c("es", "de", "us", "uk", "pl", "fr"))

# Convert 'cntry' column to factor and select only the required columns
selected_countries_data = selected_countries_data %>% 
  mutate(cntry = as.factor(cntry)) %>%
  select(cntry, year, class_scheme_5) %>%
  na.omit()

# Summarize the data by grouping
data = selected_countries_data %>%
  group_by(cntry, year, class_scheme_5) %>%
  summarise(n = n()) %>%
  mutate(t = (n / sum(n)) * 100)

# Convert country names to uppercase
data$cntry <- toupper(data$cntry)

# Set factor levels for 'cntry' column
data$cntry <- factor(data$cntry, levels=c("DE", "ES", "FR", "PL", "UK", "US"))

# Rename the 'cntry' variable by adding date ranges
data <- data %>% mutate(
  cntry = fct_recode(
    cntry, 
    "DE, 1984-2018" = "DE", 
    "ES, 1980-2018" = "ES",
    "FR, 1984-2018" = "FR",  
    "PL, 1999-2018" = "PL", 
    "UK, 1998-2018" = "UK",
    "US, 1978-2018" = "US"
  )
)

# Reorder levels for 'class' column
data <- data %>% mutate(class_scheme_5 = fct_relevel(class_scheme_5, "Upper-middle class", "Middle class", "Lower-middle class","Skilled working class", "Low-skilled working class"))

# Plot
ggplot(data, aes(x=year, y=t, fill=class_scheme_5)) + 
  geom_area(alpha=0.6, size=.5, colour="white") +
  scale_y_continuous(limits=c(0, 101), breaks=seq(0, 100, by=25)) +
  scale_x_continuous(limits=c(1978, 2020), breaks=seq(1980, 2020, by=10)) +
  facet_wrap(~cntry, nrow=3,scales = "free_x") +
  scale_fill_manual(values=c("#1F2E7A", "#6477d8","#D9D9D9", "#f198ad", "#C91D42")) +
  theme_classic() + ylab(NULL) + xlab(NULL) +
  theme(
    legend.position="bottom",
    plot.title=element_text(hjust=0.5, size=25),
    panel.background=element_rect(fill="white"),
    panel.grid.minor=element_blank(),
    panel.margin=unit(1.2, "lines"),
    text=element_text(size=28),
    axis.title.y=element_text(size=20),
    axis.title.x=element_text(size=20),
    axis.text.y=element_text(size=25),
    axis.text.x=element_text(size=25),
    legend.title=element_blank(),
    legend.text=element_text(size=25),
    strip.background=element_blank(),
    panel.border=element_blank())+
  guides(colour = guide_legend(nrow = 2),
         fill=guide_legend(nrow=2,byrow=TRUE))

