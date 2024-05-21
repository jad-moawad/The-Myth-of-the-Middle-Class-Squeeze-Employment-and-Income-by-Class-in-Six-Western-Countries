############################################################################################################
##### Authors: Jad Moawad & Daniel Oesch
##### Project: The Myth of the Middle Class Squeeze: Employment and Income by Class in Six Western Countries, 1980-2020
##### Data: 	Luxembourg Income Study (LIS)
############################################################################################################

# You can install any of these libraries using the following command: install.packages("name_of_library")
# Load libraries
library(dplyr)
library(tidyr)
library(broom)
library(forcats)
library(haven)

# Define function to create variables
create_vars <- function(df) {
  df <- df %>% 
    mutate(status = case_when(
      status1 %in% c(100, 110, 120, 230, 240) ~ 1, # Employee: dependent employed, regular employee, non regular employee, member of producers co-operative, contributing family worker
      status1 %in% c(200, 220) ~ 2, # Self-employed: self-employed, own-account worker
      status1 == 210 ~ 3, # Employer
      TRUE ~ NA_real_
    ),
    base_class = case_when(
      (occupation == 1 & status == 2) |
        (occupation == 1 & status == 1 & educ == 1) |
        (occupation == 2 & educ != 3) |
        occupation == 3 |
        (occupation == 4 & educ == 3) |
        (occupation >= 4 & occupation <= 8 & status == 3) ~ 3, #"Middle class"
      (occupation == 1 & status == 3) |
        (occupation == 1 & status == 1 & educ != 1) |
        (occupation == 2 & educ == 3) ~ 4, #"Upper-middle class",
      (occupation == 4 & status != 3 & educ != 3) |
        (occupation %in% c(5, 7, 10) & status == 1) |
        (occupation >= 5 & occupation <= 8 & status == 2) ~ 2, # "Skilled working class"
      (occupation == 6 & status == 1) |
        (occupation == 8 & status ==1) |
        occupation == 9 ~ 1, #"Low-skilled working class"
      TRUE ~ NA_real_
    ),
    class_scheme_5 = case_when(
      (occupation == 1 & status == 2) |
        (occupation == 1 & status == 1 & educ == 1) |
        (occupation == 2 & educ != 3) |
        occupation == 3 ~ 4, #"Middle class"
      (occupation == 1 & status == 3) |
        (occupation == 1 & status == 1 & educ != 1) |
        (occupation == 2 & educ == 3) ~ 5, #"Upper-middle class"
      (occupation == 6 & status == 1) |
        (occupation == 8 & status ==1) |
        occupation == 9 ~ 1, #"Low-skilled working class"
      (occupation == 4)  |
        (occupation >= 4 & occupation <= 8 & status != 1) ~ 3, # "Lower-middle class"
      (occupation %in% c(5, 7, 10) & status == 1) ~ 2, #"Skilled working class"
      TRUE ~ NA_real_
    ))
  return(df)
}

# Define function to filter French data by year
filter_FR_data <- function(data, start_year, end_year) {
  data %>% 
    filter(cntry == "FR", year >= start_year, year <= end_year) %>%
    create_vars()
}

# Load and process data
data <- read.dta13(paste(USR_DIR, "/jmoawa/data.dta", sep=""), convert.factors=TRUE)       
data <- data %>% 
  mutate(occupation = occb1) %>%
  create_vars()

# Create df_all without FR
df_all <- filter(data, !cntry %in% c("FR")) %>% create_vars()

# France data processing  ------------------------------------------------------------------

# Import early modules in France with consistent occupational data
france_early_modules_data <- read_dta(paste0(USR_DIR, "/jmoawa/france_old.dta")) %>% 
  mutate(
    cntry = 'FR',
    occupation = occb1) %>%
  create_vars()

# Import newer modules starting in 1996 
q <- read.dta13(file.path(USR_DIR, "jmoawa/data.dta"), convert.factors=TRUE) 

# Filtering by country and years
q_fr <- q %>% 
  filter(cntry=="FR", year %in% 1996:2018)
rm(q)

# Splitting into two in France based on differences in the occupational variable
france_data_1996_2016 <- q_fr %>% filter(year <= 2016)
france_data_2016_2018 <- q_fr %>% filter(year > 2016)

# Harmonize the occupation variable in France with respect to ISCO
france_data_1996_2016 <- france_data_1996_2016 %>% mutate(
  occupation = case_when(
    occ1_c %in% c(23,33,37) ~ 1, 
    occ1_c %in% c(31,34,35,38,42,44) ~ 2, 
    occ1_c == 43 | occ1_c %in% 45:48 ~3, 
    occ1_c %in% c(54,55) ~4, 
    occ1_c %in% c(22,52) ~5, 
    occ1_c %in% c(10:13,69) ~6, 
    occ1_c %in% c(21,62,63) ~7, 
    occ1_c %in% 64:66 ~8, 
    occ1_c %in% c(56,67,68) ~9, 
    occ1_c == 53 ~10, 
    TRUE ~ NA_real_
  )) %>%
  create_vars()

# Create the class variable for France 
france_data_2016_2018 <- france_data_2016_2018 %>% mutate(occupation = occb1)%>%
  create_vars()

# Joining and Mutate for final class
fr <- bind_rows(france_early_modules_data,france_data_1996_2016, france_data_2016_2018) 
rm(q,france_early_modules_data,france_data_1996_2016,france_data_2016_2018)

# Combine France with other countries data ---------------------------
df=bind_rows(df_all,fr)
rm(data,df_all,fr)

# Data cleaning and preparation ---------------------------
df$year = as.numeric(df$year) 
df$cntry = tolower(df$cntry) 

# Drop UK data before 1997 due to missing harmonized education
df = filter(df, !(cntry == "uk" & year <= 1997))

# Import PPP data
ppp_data <- read.dta13(file.path(INC_DIR, "ppp_2017.dta"), convert.factors = FALSE)            
ppp_data$cntry = ppp_data$iso2          

# Merge dataframes
df <- inner_join(x = df, y = ppp_data, by = c("cntry", "year"))

# Harmonize variables -----------------------------------------------------
#Limit age range to 25-60   
df <- df %>% filter(age >= 25 & age <= 60)

# Create a same class and dominant class measures of social class  
df$did = as.numeric(df$did)
df$hid = as.numeric(df$hid)
same_class_data <- df %>%     
  drop_na(base_class)  %>% 
  filter(relation < 3000) %>%
  group_by(hid, did) %>%      
  summarise(dominant_class = max(base_class, na.rm=T), 
            same_class = n_distinct(base_class, na.rm = TRUE) == 1,
            dominant_class_5 = max(class_scheme_5, na.rm=T), 
            same_class_5 = n_distinct(class_scheme_5, na.rm = TRUE) == 1) %>%
  mutate(same_class = if_else(is.infinite(dominant_class) | !same_class, NA_real_, dominant_class),
         same_class_5 = if_else(is.infinite(dominant_class_5) | !same_class_5, NA_real_, dominant_class_5))

df <- df %>%
  left_join(same_class_data, by = c("hid", "did"))

rm(same_class_data)

# Label class and create other factors
df <- df %>% 
  mutate(
    base_class = factor(base_class, levels = c(1,2,3,4), labels = c("Low-skilled working class", "Skilled working class", "Middle class","Upper-middle class")),
    class = factor(same_class, levels = c(1,2,3,4), labels = c("Low-skilled working class", "Skilled working class", "Middle class","Upper-middle class")),
    dominant_class = factor(dominant_class, levels = c(1,2,3,4), labels = c("Low-skilled working class", "Skilled working class", "Middle class","Upper-middle class")),
    class_scheme_5 = factor(same_class_5, levels = c(1,2,3,4,5), labels = c("Low-skilled working class", "Skilled working class", "Lower-middle class","Middle class","Upper-middle class")),
    cohort1 = year - age,
    cohort = case_when(
      cohort1 >= 1926 & cohort1 <= 1945 ~ "1926-1945", 
      cohort1 >= 1946 & cohort1 <= 1965 ~ "1946-1965", 
      cohort1 >= 1966 & cohort1 <= 1980 ~ "1966-1980",
      TRUE ~ NA_character_),
    head_sex = case_when(
      relation == 1000 & sex == 1 ~ "Male head", 
      relation == 1000 & sex == 2 ~ "Female head", 
      TRUE ~ NA_character_),
    hilabour = (hilabour / (nhhmem^0.5) / cpi) * 100,
    dhi = (dhi / (nhhmem^0.5) / cpi) * 100) %>% 
  mutate_at(vars(cohort, head_sex), as.factor)

# Adjust for high and low incomes
df <- df %>% 
  group_by(cntry, year) %>% 
  mutate(
    labor_income = ifelse(hilabour < 1, NA, hilabour),
    income_0 = ifelse(hilabour < 1, 0, hilabour),
    disposable_income = ifelse(dhi < 1, NA, dhi))

# Drop unnecessary variables and save data --------------------------------
keeps <- c("class","disposable_income","labor_income","occupation","cntry","year", "age", "cohort", "nhhmem","income_0","hwgt","head_sex","dominant_class","class_scheme_5")   
df= df[ , keeps, drop = FALSE]   

# Construct the file path for saving the dataset
# save.dta13(df,paste(USR_DIR,"/path/to/your/directory", sep = ""), version = 117,convert.factors=TRUE)                      
# df <-read.dta13(paste(USR_DIR, "/path/to/your/directory", sep=""),convert.factors=TRUE)   

# Example
save.dta13(df,paste(USR_DIR,"/jmoawa/reg.dta", sep = ""), version = 117,convert.factors=TRUE)         
df <-read.dta13(paste(USR_DIR, "/jmoawa/reg.dta", sep=""),convert.factors=TRUE)     

# Inspect the structure of the dataframe
str(df)