############################################################################################################
##### Authors: Jad Moawad & Daniel Oesch
##### Project: The Myth of the Middle Class Squeeze: Employment and Income by Class in Six Western Countries, 1980-2020
##### Data: 	Luxembourg Income Study (LIS)
############################################################################################################

# Figure 5: Household disposable income by class and cohort (adjusted predictions) 

# Load required libraries
library(forcats)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggeffects)

# Read data
# data_path <- paste(USR_DIR, "/path/to/your/directory", sep="")
# raw_data <- read.dta13(data_path, convert.factors=TRUE)

# Example - Read data
data_path <- paste(USR_DIR, "/jmoawa/reg.dta", sep="")
raw_data <- read.dta13(data_path, convert.factors=TRUE)

# Subset the data with specified conditions
filtered_data <- subset(raw_data, select = c(cntry, class, year, labor_income, disposable_income, nhhmem, hwgt, cohort, age), 
                        age >= 35 & age <= 50 & labor_income > 0 & disposable_income > 0)

# Keep the same analytical sample as the descriptives
filtered_data <- drop_na(filtered_data, cntry, class, year, labor_income, disposable_income, nhhmem, hwgt)

# Create a model to calculated disposable income for different cohorts and social clases by country
prediction_data <- filtered_data %>%
  group_by(cntry) %>% 
  do(
    model1 = ggpredict(
      lm(log(disposable_income) ~ class*cohort + age, data = .),
      terms = c("class", "cohort")
    )
  ) %>%
  gather(model_name, model, -cntry) %>% 
  unnest()

# Select and rename columns for clarity
formatted_data <- prediction_data %>% 
  select(cntry, class = x, cohort = group, predicted_income = predicted, 
         error = std.error, confidence_high = conf.high, confidence_low = conf.low)

data <- as.data.frame(formatted_data)

data$cntry =toupper(data$cntry)

# Select the countries which has data available for three cohort groups
filter_data_1= data %>%
  filter((cntry=="DE"|cntry=="FR"|cntry=="ES"|cntry=="US"))%>%
  group_by(cntry) %>%
  mutate(base_year = (predicted_income / first(predicted_income[class=="Low-skilled working class" & cohort=="1926-1945"]))*100)

# Create a separate data to add the labeling of numbers of predictions on the figure
label_1=filter_data_1
label_2=filter_data_1
label_1$base_year=ifelse((label_1$cohort!="1926-1945") &label_1$base_year>0,NA,label_1$base_year)
label_2$base_year=ifelse((label_2$cohort!="1966-1980") &label_2$base_year>0,NA,label_2$base_year)

# Select the countries which has data available for two cohort groups
filter_data_2= data %>%
  filter((cntry=="PL"|cntry=="UK") & cohort!="1926-1945") %>%
  group_by(cntry) %>%
  mutate(base_year = (predicted_income / first(predicted_income[class=="Low-skilled working class" & cohort=="1946-1965"]))*100)

# Create a separate data to add the labeling of numbers of predictions on the figure
label_3=filter_data_2
label_4=filter_data_2
label_3$base_year=ifelse((label_3$cohort!="1946-1965") &label_3$base_year>0,NA,label_3$base_year)
label_4$base_year=ifelse((label_4$cohort!="1966-1980") &label_4$base_year>0,NA,label_4$base_year)

# Merge all countries together
prediction_data=bind_rows(filter_data_1,filter_data_2)

# Plot the predictions
ggplot(prediction_data, aes(cohort, base_year, cohort = class, shape= class, color=class)) + 
  geom_line(aes(group = class), size=1.75)+
  geom_text(data = label_1,  aes(cohort, base_year, label = round(base_year,0)), hjust = 1.25, size=6,show.legend = FALSE) +
  geom_text(data = label_2,  aes(cohort, base_year, label = round(base_year,0)), hjust = -0.20, size=6,show.legend = FALSE) +
  geom_text(data = label_3,  aes(cohort, base_year, label = round(base_year,0)), hjust = 1.25, size=6,show.legend = FALSE) +
  geom_text(data = label_4,  aes(cohort, base_year, label = round(base_year,0)), hjust = -0.20, size=6,show.legend = FALSE) +
  scale_color_manual(values=c("#C91D42","#f198ad","#6477d8","#1F2E7A")) +
  facet_wrap(~cntry,scales = "free_x")+
  theme_classic()+
  labs(cohort= "Class", shape= "Class", linetype= "Class",color= "Class") +
  scale_y_continuous(limits = c(90,300), breaks = seq(100,300, by = 50))+
  xlab(NULL)+ ylab(NULL) + ggtitle(NULL) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5,size=20),
        panel.background = element_rect(fill = "white"), 
        panel.grid.minor = element_blank(),
        panel.margin = unit(1.2, "lines"),
        text = element_text(size=25),
        axis.text.y = element_text(size=18),
        axis.text.x = element_text(size=16),
        legend.title= element_blank(), 
        legend.text=element_text(size=18),
        strip.background = element_blank(),
        panel.border=element_blank()) +
  guides(fill = guide_legend(reverse = F))

