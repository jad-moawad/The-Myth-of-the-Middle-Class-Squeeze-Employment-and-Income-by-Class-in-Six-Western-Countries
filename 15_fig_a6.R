############################################################################################################
##### Authors: Jad Moawad & Daniel Oesch
##### Project: The Myth of the Middle Class Squeeze: Employment and Income by Class in Six Western Countries, 1980-2020
##### Data: 	Luxembourg Income Study (LIS)
############################################################################################################

# Figure A.6: Employment change by class in small and affluent European countries (in percentage points)

# Load necessary libraries
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
filtered_data <- subset(raw_data, select = c(cntry, class, year, labor_income, disposable_income, nhhmem, hwgt), 
                        age >= 25 & age <= 60 & labor_income > 0 & disposable_income > 0)

# Keep the same analytical sample
filtered_data <- filtered_data %>%
  na.omit()

# Select countries of interest
data_selected_countries = filtered_data %>% 
  filter(cntry %in% c("at", "ch", "dk", "fi", "ie", "nl"))

# Convert 'cntry' column to factor and select only the required columns
data_final = data_selected_countries %>% 
  mutate(cntry = as.factor(cntry)) %>%
  select(cntry, year, class) %>%
  na.omit()

# Summarize the data by grouping
data = data_final %>%
  group_by(cntry, year, class) %>%
  summarise(n = n()) %>%
  mutate(t = (n / sum(n)) * 100)

initial_data <- as.data.frame(data)

# Convert country names to upper case
initial_data$cntry = toupper(initial_data$cntry)

# Reorder class levels
initial_data =  initial_data %>% mutate(class = fct_relevel(class,"Upper-middle class", "Middle class", "Skilled working class", "Low-skilled working class"))

# Calculate difference in % between the first and last year for each country and class combination
summary_data = initial_data %>%
  group_by(cntry, class) %>%
  arrange(year) %>%
  filter(row_number() == 1 | row_number() == n()) %>%
  mutate(diff = t - first(t)) %>%
  filter(year > 2000)

# Adjust country factor levels and labels
summary_data$cntry <- factor(summary_data$cntry, levels = c("AT","CH","DK","FI","IE","NL"))

# Rename the 'cntry' variable by adding date ranges
summary_data = summary_data %>% dplyr::mutate(cntry = fct_recode(cntry, 
                                                                 "AT, 1994-2018" = "AT", 
                                                                 "CH, 1992-2018" = "CH",
                                                                 "DK, 1992-2018" = "DK",  
                                                                 "FI, 1987-2016" = "FI", 
                                                                 "IE, 1994-2018" = "IE",
                                                                 "NL, 1990-2018" = "NL"))

# Round difference to 1 decimal point
summary_data$diff2 = round(summary_data$diff, 1)

# Reorder levels for 'class' column
summary_data = summary_data %>% mutate(class = forcats::fct_relevel(class,"Low-skilled working class", "Skilled working class","Middle class", "Upper-middle class"))

# Helps with aesthetics plotting the figure
summary_data$year = 0

# To add a dashed vertical line between countries
summary_data$end_of_country <- ifelse(summary_data$year == max(summary_data$year), 1, 0)

# Plot
ggplot(summary_data, aes(x=year, y=diff, fill=class,color=class)) +
  geom_col(position = "dodge") +
  geom_text(aes(y = diff + ifelse(diff>=0,0.5, -1.2),label = diff2), vjust =  -0.04, colour = "black",position = position_dodge(.9), size =4.5)+
  facet_wrap(~cntry, scales = "free_x",nrow=1)+  
  scale_color_manual(values = c("white","white","white","white","white"))+
  scale_fill_manual(values=c("#C91D42", "#f198ad","#6477d8","#1F2E7A"))+ 
  theme_classic()+ ylab(NULL)+xlab(NULL)+
  theme( 
    axis.title.x = element_text(color="Black", size=15,),
    axis.title.y = element_text(color="Black", size=15, hjust = 0.5),
    plot.title = element_text(hjust = 0.5, color="Black", size=20,),
    legend.text = element_text(color="Black", size=14,),
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
  geom_vline(data = subset(summary_data, end_of_country == 1 & cntry != tail(unique(summary_data$cntry), n = 1)), aes(xintercept = year + 0.5), linetype="dashed", color = "black")

