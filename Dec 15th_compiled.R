# Libraries and Datasets --------------------------------------------------
library(tidyverse)
library(haven)
library(gt)
library(webshot)
library(purrr)
library(webshot2)
library(broom)
library(modelsummary)
library(tinytex)
library(fixest)
library(glue)
library(sandwich)
library(lmtest)
library(gtsummary)


#tinytex::install_tinytex()


replication_dataset <- read_dta("dataverse_files/Replication_Dataset.dta")

# Full Sample -------------------------------------------------------------
# education: 
education <- 
  replication_dataset |> 
  count(EDU1, EDU2, EDU3, EDU4) |>  
  pivot_longer(cols = starts_with("EDU"), 
               names_to = "EDU", 
               values_to = "value") |> 
  filter(value == 1) |>
  select(EDU, n) |> 
  mutate(full_sample = round((n/sum(n))*100, 1)) |>  
  arrange(factor(EDU)) |>  
  rename(category = EDU)

#gender: 
gender <- 
  replication_dataset |>
  count(female) |>  
  mutate(full_sample = round(n/sum(n) * 100, 1)) |>  
  rename(category = female)##recode 0 and 1


#age: 
age <- 
  replication_dataset |>
  mutate(category = case_when( age >= 18 & age < 25 ~ "18-24", 
                                  age >= 25 & age < 35 ~ "25-34", 
                                  age >= 35 & age < 45 ~ "35-44", 
                                  age >= 45 & age < 55 ~ "45-54", 
                                  age >= 55 ~ "55+")) |>  
  count(category) |>  
  mutate(full_sample = round(n/sum(n) * 100, 1)) 

#income: 
reorder_levels <- c("Less than 15,000€ per year", "From 15,000€ to 29,999€ per year", 
                    "From 30,000€ to 44,999€ per year", "From 45,000€ to 69,999€ per year", 
                    "Above 70,000€ per year", "No Answer/DK")
income <- 
  replication_dataset |> 
  mutate(category = case_when(INC1 == 1 | INC2 == 1 | INC3 == 1 ~ "Less than 15,000€ per year", 
                                     INC4 == 1 | INC5 == 1 | INC6 == 1 ~ "From 15,000€ to 29,999€ per year", 
                                     INC7 == 1 | INC8 == 1 | INC9 == 1 ~ "From 30,000€ to 44,999€ per year", 
                                     INC10 == 1 | INC11 == 1 | INC12 == 1 ~ "From 45,000€ to 69,999€ per year", 
                                     INC13 == 1 | INC14 == 1 ~ "Above 70,000€ per year", 
                                     INC15 == 1 | INC16 == 1 ~ "No Answer/DK")) |>
  mutate(category = factor(category, levels = reorder_levels)) |> 
  count(category) |>  
  mutate(full_sample = round(n/sum(n) * 100, 1))


# Diesel Euro 4 -----------------------------------------------------------
# education: 
education_de4 <- 
  replication_dataset |>
  filter(dummy_diesel == 1 & dummy_euro_4 == 1 ) |> 
  count(EDU1, EDU2, EDU3, EDU4) |>  
  pivot_longer(cols = starts_with("EDU"), 
               names_to = "EDU", 
               values_to = "value") |> 
  filter(value == 1) |>
  select(EDU, n) |> 
  mutate(diesel_euro_4_cars = round((n/sum(n))*100, 1)) |>  
  arrange(factor(EDU)) |>  
  rename(category = EDU)

#gender: 
gender_de4 <- 
  replication_dataset |>
  filter(dummy_diesel == 1 & dummy_euro_4 == 1 ) |> 
  count(female) |>  
  mutate(diesel_euro_4_cars = round(n/sum(n) * 100, 1)) |>  
  rename(category = female)##recode 0 and 1


#age: 
age_de4 <- 
  replication_dataset |> 
  filter(dummy_diesel == 1 & dummy_euro_4 == 1 ) |> 
  mutate(category = case_when( age >= 18 & age < 25 ~ "18-24", 
                               age >= 25 & age < 35 ~ "25-34", 
                               age >= 35 & age < 45 ~ "35-44", 
                               age >= 45 & age < 55 ~ "45-54", 
                               age >= 55 ~ "55+")) |>  
  count(category) |>  
  mutate(diesel_euro_4_cars = round(n/sum(n) * 100, 1)) 

#income: 
income_de4 <- 
  replication_dataset |> 
  filter(dummy_diesel == 1 & dummy_euro_4 == 1 ) |> 
  mutate(category = case_when(INC1 == 1 | INC2 == 1 | INC3 == 1 ~ "Less than 15,000€ per year", 
                              INC4 == 1 | INC5 == 1 | INC6 == 1 ~ "From 15,000€ to 29,999€ per year", 
                              INC7 == 1 | INC8 == 1 | INC9 == 1 ~ "From 30,000€ to 44,999€ per year", 
                              INC10 == 1 | INC11 == 1 | INC12 == 1 ~ "From 45,000€ to 69,999€ per year", 
                              INC13 == 1 | INC14 == 1 ~ "Above 70,000€ per year", 
                              INC15 == 1 | INC16 == 1 ~ "No Answer/DK")) |>
  mutate(category = factor(category, levels = reorder_levels)) |>
  count(category) |>  
  mutate(diesel_euro_4_cars = round(n/sum(n) * 100, 1))


# Diesel Euro 5 -----------------------------------------------------------
# education: 
education_de5 <- 
  replication_dataset |>
  filter(dummy_diesel == 1 & dummy_euro_5 == 1 ) |> 
  count(EDU1, EDU2, EDU3, EDU4) |>  
  pivot_longer(cols = starts_with("EDU"), 
               names_to = "EDU", 
               values_to = "value") |> 
  filter(value == 1) |>
  select(EDU, n) |> 
  mutate(diesel_euro_5_cars = round((n/sum(n))*100, 1)) |>  
  arrange(factor(EDU)) |>  
  rename(category = EDU)

#gender: 
gender_de5 <- 
  replication_dataset |>
  filter(dummy_diesel == 1 & dummy_euro_5 == 1 ) |> 
  count(female) |>  
  mutate(diesel_euro_5_cars = round(n/sum(n) * 100, 1)) |>  
  rename(category = female)##recode 0 and 1


#age: 
age_de5 <- 
  replication_dataset |> 
  filter(dummy_diesel == 1 & dummy_euro_5 == 1 ) |> 
  mutate(category = case_when( age >= 18 & age < 25 ~ "18-24", 
                               age >= 25 & age < 35 ~ "25-34", 
                               age >= 35 & age < 45 ~ "35-44", 
                               age >= 45 & age < 55 ~ "45-54", 
                               age >= 55 ~ "55+")) |>  
  count(category) |>  
  mutate(diesel_euro_5_cars = round(n/sum(n) * 100, 1)) 

#income: 
income_de5 <- 
  replication_dataset |> 
  filter(dummy_diesel == 1 & dummy_euro_5 == 1 ) |> 
  mutate(category = case_when(INC1 == 1 | INC2 == 1 | INC3 == 1 ~ "Less than 15,000€ per year", 
                              INC4 == 1 | INC5 == 1 | INC6 == 1 ~ "From 15,000€ to 29,999€ per year", 
                              INC7 == 1 | INC8 == 1 | INC9 == 1 ~ "From 30,000€ to 44,999€ per year", 
                              INC10 == 1 | INC11 == 1 | INC12 == 1 ~ "From 45,000€ to 69,999€ per year", 
                              INC13 == 1 | INC14 == 1 ~ "Above 70,000€ per year", 
                              INC15 == 1 | INC16 == 1 ~ "No Answer/DK")) |>
  mutate(category = factor(category, levels = reorder_levels)) |>
  count(category) |>  
  mutate(diesel_euro_5_cars = round(n/sum(n) * 100, 1))


# Petrol Euro 4 ------------------------------------------------------
# education: 
education_pe4 <- 
  replication_dataset |>
  filter(dummy_petrol == 1 & dummy_euro_4 == 1 ) |> 
  count(EDU1, EDU2, EDU3, EDU4) |>  
  pivot_longer(cols = starts_with("EDU"), 
               names_to = "EDU", 
               values_to = "value") |> 
  filter(value == 1) |>
  select(EDU, n) |> 
  mutate(petrol_euro_4_cars = round((n/sum(n))*100, 1)) |>  
  arrange(factor(EDU)) |>  
  rename(category = EDU)

#gender: 
gender_pe4 <- 
  replication_dataset |>
  filter(dummy_petrol == 1 & dummy_euro_4 == 1 ) |> 
  count(female) |>  
  mutate(petrol_euro_4_cars = round(n/sum(n) * 100, 1)) |>  
  rename(category = female)##recode 0 and 1


#age: 
age_pe4 <- 
  replication_dataset |> 
  filter(dummy_petrol == 1 & dummy_euro_4 == 1 ) |> 
  mutate(category = case_when( age >= 18 & age < 25 ~ "18-24", 
                               age >= 25 & age < 35 ~ "25-34", 
                               age >= 35 & age < 45 ~ "35-44", 
                               age >= 45 & age < 55 ~ "45-54", 
                               age >= 55 ~ "55+")) |>  
  count(category) |>  
  mutate(petrol_euro_4_cars = round(n/sum(n) * 100, 1)) 

#income: 
income_pe4 <- 
  replication_dataset |> 
  filter(dummy_petrol == 1 & dummy_euro_4 == 1 ) |> 
  mutate(category = case_when(INC1 == 1 | INC2 == 1 | INC3 == 1 ~ "Less than 15,000€ per year", 
                              INC4 == 1 | INC5 == 1 | INC6 == 1 ~ "From 15,000€ to 29,999€ per year", 
                              INC7 == 1 | INC8 == 1 | INC9 == 1 ~ "From 30,000€ to 44,999€ per year", 
                              INC10 == 1 | INC11 == 1 | INC12 == 1 ~ "From 45,000€ to 69,999€ per year", 
                              INC13 == 1 | INC14 == 1 ~ "Above 70,000€ per year", 
                              INC15 == 1 | INC16 == 1 ~ "No Answer/DK")) |> 
  mutate(category = factor(category, levels = reorder_levels)) |>
  count(category) |>  
  mutate(petrol_euro_4_cars = round(n/sum(n) * 100, 1))


# Petrol Euro 5 -----------------------------------------------------------
# education: 
education_pe5 <- 
  replication_dataset |>
  filter(dummy_petrol == 1 & dummy_euro_5 == 1 ) |> 
  count(EDU1, EDU2, EDU3, EDU4) |>  
  pivot_longer(cols = starts_with("EDU"), 
               names_to = "EDU", 
               values_to = "value") |> 
  filter(value == 1) |>
  select(EDU, n) |> 
  mutate(petrol_euro_5_cars = round((n/sum(n))*100, 1)) |>  
  arrange(factor(EDU)) |>  
  rename(category = EDU)

#gender: 
gender_pe5 <- 
  replication_dataset |>
  filter(dummy_petrol == 1 & dummy_euro_5 == 1 ) |> 
  count(female) |>  
  mutate(petrol_euro_5_cars = round(n/sum(n) * 100, 1)) |>  
  rename(category = female)##recode 0 and 1


#age: 
age_pe5 <- 
  replication_dataset |> 
  filter(dummy_petrol == 1 & dummy_euro_5 == 1 ) |> 
  mutate(category = case_when( age >= 18 & age < 25 ~ "18-24", 
                               age >= 25 & age < 35 ~ "25-34", 
                               age >= 35 & age < 45 ~ "35-44", 
                               age >= 45 & age < 55 ~ "45-54", 
                               age >= 55 ~ "55+")) |>  
  count(category) |>  
  mutate(petrol_euro_5_cars = round(n/sum(n) * 100, 1)) 

#income: 
income_pe5 <- 
  replication_dataset |> 
  filter(dummy_petrol == 1 & dummy_euro_5 == 1 ) |> 
  mutate(category = case_when(INC1 == 1 | INC2 == 1 | INC3 == 1 ~ "Less than 15,000€ per year", 
                              INC4 == 1 | INC5 == 1 | INC6 == 1 ~ "From 15,000€ to 29,999€ per year", 
                              INC7 == 1 | INC8 == 1 | INC9 == 1 ~ "From 30,000€ to 44,999€ per year", 
                              INC10 == 1 | INC11 == 1 | INC12 == 1 ~ "From 45,000€ to 69,999€ per year", 
                              INC13 == 1 | INC14 == 1 ~ "Above 70,000€ per year", 
                              INC15 == 1 | INC16 == 1 ~ "No Answer/DK")) |>
  mutate(category = factor(category, levels = reorder_levels)) |>
  count(category) |>  
  mutate(petrol_euro_5_cars = round(n/sum(n) * 100, 1))

# Combined Tables ----------------------------------------------------------
combined <- 
  bind_rows(
  age |>  mutate(category = c("18-24", "25-34", "35-44", "45-54", "55+"), dataset = "Age"), 
  gender |>  mutate(category = c("Male", "Female"), dataset = "Gender"), 
  education |>  mutate(category = c("High school diploma", "Bachelors",
                                    "MA or higher", "Unknown"), dataset = "Education"), 
  income |> mutate(category= c("Less than 15,000€ per year", "From 15,000€ to 29,999€ per year", 
                               "From 30,000€ to 44,999€ per year", "From 45,000€ to 69,999€ per year", 
                               "Above 70,000€ per year", "No Answer/DK"), dataset = "Income", levels = reorder_levels)) |> 
  select(dataset, category, full_sample) |> 
  rename("Full Sample" = full_sample)

combined_de4 <- bind_rows(
  age_de4 |> mutate(category = c("18-24", "25-34", "35-44", "45-54", "55+"), dataset = "Age"), 
  gender_de4 |> mutate(category = c("Male", "Female"), dataset = "Gender"), 
  education_de4 |> mutate(category = c("High school diploma", "Bachelors",
                                       "MA or higher", "Unknown"), dataset = "Education"), 
  income_de4 |> mutate(category = c("Less than 15,000€ per year", "From 15,000€ to 29,999€ per year", 
                                    "From 30,000€ to 44,999€ per year", "From 45,000€ to 69,999€ per year", 
                                    "Above 70,000€ per year", "No Answer/DK"), dataset = "Income")) |> 
  select(dataset, category, diesel_euro_4_cars) |> 
  rename("Diesel Euro 4" = diesel_euro_4_cars)

combined_de5 <- bind_rows(
  age_de5 |> mutate(category = c("18-24", "25-34", "35-44", "45-54", "55+"), dataset = "Age"), 
  gender_de5 |> mutate(category = c("Male", "Female"), dataset = "Gender"), 
  education_de5 |> mutate(category = c("High school diploma", "Bachelors",
                                       "MA or higher", "Unknown"), dataset = "Education"), 
  income_de5 |> mutate(category = c("Less than 15,000€ per year", "From 15,000€ to 29,999€ per year", 
                                    "From 30,000€ to 44,999€ per year", "From 45,000€ to 69,999€ per year", 
                                    "Above 70,000€ per year", "No Answer/DK"), dataset = "Income")) |> 
  select(dataset, category, diesel_euro_5_cars) |> 
  rename("Diesel Euro 5" = diesel_euro_5_cars)

combined_pe4 <- bind_rows(
  age_pe4 |> mutate(category = c("18-24", "25-34", "35-44", "45-54", "55+"), dataset = "Age"), 
  gender_pe4 |> mutate(category = c("Male", "Female"), dataset = "Gender"), 
  education_pe4 |> mutate(category = c("High school diploma", "Bachelors",
                                       "MA or higher", "Unknown"), dataset = "Education"), 
  income_pe4 |> mutate(category = c("Less than 15,000€ per year", "From 15,000€ to 29,999€ per year", 
                                    "From 30,000€ to 44,999€ per year", "From 45,000€ to 69,999€ per year", 
                                    "Above 70,000€ per year", "No Answer/DK"), dataset = "Income")) |> 
  select(dataset, category, petrol_euro_4_cars) |> 
  rename("Petrol Euro 4" = petrol_euro_4_cars)

combined_pe5 <- bind_rows(
  age_pe5 |> mutate(category = c("18-24", "25-34", "35-44", "45-54", "55+"), dataset = "Age"), 
  gender_pe5 |> mutate(category = c("Male", "Female"), dataset = "Gender"), 
  education_pe5 |> mutate(category = c("High school diploma", "Bachelors",
                                       "MA or higher", "Unknown"), dataset = "Education"), 
  income_pe5 |> mutate(category = c("Less than 15,000€ per year", "From 15,000€ to 29,999€ per year", 
                                    "From 30,000€ to 44,999€ per year", "From 45,000€ to 69,999€ per year", 
                                    "Above 70,000€ per year", "No Answer/DK"), dataset = "Income")) |> 
  select(dataset, category, petrol_euro_5_cars) |> 
  rename("Petrol Euro 5" = petrol_euro_5_cars)

sum_table <- 
  replication_dataset |> 
  summarise(n = n(),
            diesel_euro_4_cars = sum(dummy_diesel == 1 & dummy_euro_4 == 1),
            diesel_euro_5_cars = sum(dummy_diesel == 1 & dummy_euro_5 == 1),
            petrol_euro_4_cars = sum(dummy_petrol == 1 & dummy_euro_4 == 1),
            petrol_euro_5_cars = sum(dummy_petrol == 1 & dummy_euro_5 == 1)) |>
  rename(c("Full Sample" = n, "Diesel Euro 4" = diesel_euro_4_cars, "Diesel Euro 5" = diesel_euro_5_cars,
           "Petrol Euro 4" = petrol_euro_4_cars, "Petrol Euro 5" = petrol_euro_5_cars)) |> 
  mutate(category = "n", dataset = "")
  

# Descriptive Stats Table Replication  ------------------------------------

combined <- 
  combined |> 
  left_join(combined_de4) |> 
  left_join(combined_de5) |> 
  left_join(combined_pe4) |> 
  left_join(combined_pe5) |> 
  bind_rows(sum_table) 


gt_table<- 
  combined |> 
  gt(groupname_col = "dataset", rowname_col = "category") |> 
  rm_stubhead() |> 
  cols_align(align = "center") |> 
  cols_width(everything() ~ px(145)) |>
  tab_footnote("Note: Descriptive statistics on the composition of the sample, overall and by type of car. All figures are shares, summing up to 1 within each column, by section") |> 
  gtsave("tb1.png")

gt_main <- 
  combined |> 
  filter(dataset %in% c("Income", "" )) |> 
  gt(groupname_col = "dataset", rowname_col = "category") |> 
  cols_align(align = "center") |> 
  cols_width(everything() ~ px(145)) |> 
  tab_footnote("Note: Descriptive statistics on the composition of income groups of the sample and by type of car. All figures are shares, summing up to 1 within each column, by income") |> 
  gtsave("tb2.png")

# Table 1 ------------------------------------------------------

#No covariates 
rep_dat_1 <-
  replication_dataset |>  
  filter(target != 3 & no_answer_euro == 0 & target != 4)

fit1 <- lm(vote_lega_euro ~  diesel_euro4 + dummy_diesel + dummy_euro_4 , data = rep_dat_1) 


#controls (age, female, education, income)
edu_names <-
  colnames(replication_dataset) |>
  str_subset(pattern = "EDU") 

inc_names <- 
  colnames(replication_dataset) |>  
  str_subset(pattern = "INC")

variables2 <- c("diesel_euro4","dummy_diesel", "dummy_euro_4", "age", "female")

form_cont2 <- glue("vote_lega_euro ~  {str_c(variables2, collapse = ' + ')}  + {str_c(edu_names, collapse = ' + ')} + {str_c(inc_names, collapse = ' + ')}")

fit2 <- lm(as.formula(form_cont2), data = rep_dat_1)

# with cars unknown
rep_dat_3 <- 
  replication_dataset |>  
  filter(target != 3 & no_answer_euro == 0)
  
variables3 <- c("diesel_euro4_ass","dummy_diesel_ass", "dummy_euro_4_ass", "age", "female", "dummy_car_unknown")

form_cont3 <- glue("vote_lega_euro ~  {str_c(variables3, collapse = ' + ')}  + {str_c(edu_names, collapse = ' + ')} + {str_c(inc_names, collapse = ' + ')}")

fit3 <- lm(as.formula(form_cont3), data = rep_dat_3)

# with Past Lega Vote: L2018
rep_dat_4 <- 
  replication_dataset |>  
  filter(target != 3 & no_answer_euro == 0 & no_answer_2018 == 0 & target != 4)

variables4 <- c("diesel_euro4","dummy_diesel", "dummy_euro_4", "age", "female", "vote_lega_2018")

form_cont4 <- glue("vote_lega_euro ~  {str_c(variables4, collapse = ' + ')} + {str_c(edu_names, collapse = ' + ')} + {str_c(inc_names, collapse = ' + ')}")

fit4 <- lm(as.formula(form_cont4), data = rep_dat_4)

#With Past Lega Vote: R2018
rep_dat_5 <- 
  replication_dataset |>  
  filter(target != 3 & no_answer_euro == 0 & no_answer_regional == 0 & target != 4)

variables5 <- c("diesel_euro4","dummy_diesel", "dummy_euro_4", "age", "female", "vote_lega_regional")

form_cont5 <- glue("vote_lega_euro ~  {str_c(variables5, collapse = ' + ')}  + {str_c(edu_names, collapse = ' + ')} + {str_c(inc_names, collapse = ' + ')}")

fit5 <- lm(as.formula(form_cont5), data = rep_dat_5)

#With Past Lega Vote: M2016
rep_dat_6 <- 
  replication_dataset |>  
  filter(target != 3 & no_answer_euro == 0 & no_answer_municipal == 0 & target != 4)

variables6 <- c("diesel_euro4","dummy_diesel", "dummy_euro_4", "age", "female", "vote_lega_municipal")

form_cont6 <- glue("vote_lega_euro ~  {str_c(variables6, collapse = ' + ')}  + {str_c(edu_names, collapse = ' + ')} + {str_c(inc_names, collapse = ' + ')}")

fit6 <- lm(as.formula(form_cont6), data = rep_dat_6)

models_list <- list(fit1, fit2, fit3, fit4, fit5, fit6)

robust_se_list <- list(  
  sandwich::vcovHC(fit1, type = "HC1"), sandwich::vcovHC(fit2, type = "HC1"),
  sandwich::vcovHC(fit3, type = "HC1"), sandwich::vcovHC(fit4, type = "HC1"),
  sandwich::vcovHC(fit5, type = "HC1"), sandwich::vcovHC(fit6, type = "HC1"))

m1 <- modelsummary(models_list, vcov = robust_se_list,  
             coef_rename = c(diesel_euro4 = "Diesel x Euro 4", dummy_diesel = "Diesel", dummy_euro_4 = "Euro 4", 
                             age = "Age", female = "Female", diesel_euro4_ass = "Diesel x Euro 4", 
                             dummy_diesel_ass = "Diesel", dummy_euro_4_ass = "Euro 4", 
                             vote_lega_2018 = "Past Lega Vote", vote_lega_regional = "Past Lega Vote", 
                             vote_lega_municipal = "Past Lega Vote"), 
             coef_omit = c("dummy_car_unknown|(Intercept)|EDU|INC"), 
             add_rows = data.frame(intercept = c("Education F.E.", "Income F.E.", "Past Lega Vote"), 
                                   fit1 = c("No", "No", "No"), fit2 = c("Yes", "Yes", "No"), 
                                   fit3 = c("Yes", "Yes", "No"), fit4 = c("Yes", "Yes", "L2018"), 
                                   fit5 = c("Yes", "Yes", "R2018"), fit6 = c("Yes", "Yes", "M2016")),
             gof_map = c("Education F.E.", "Income F.E.", "Past Lega Vote", "nobs", "r.squared"), 
             output = "gt", 
             stars = c('*' = .05, '**' = .01))

gt_1 <- m1$`_data`

gt_1 <- 
  gt_1 |>
  gt(rowname_col = "row") |>
  tab_spanner(label = "Dependent Variable: Vote For Lega 2019",
              columns = c( 2, 3, 4, 5, 6, 7)) |> 
  cols_align(align = "center") |> 
  tab_footnote("Note: Columns 2–6 report estimates from regression models that include controls for age and gender, as well as fixed effects for education levels and income brackets. Column 3 includes respondents that did not report their car’s fuel and/or emission category. Columns 4–6 include dummies for past Lega vote in legislative, regional, and municipal elections, respectively. Robust standard errors are in parentheses. *p < 0.05; **p < 0.01.")

  
gtsave(gt_1, file = "dataverse_files/tb3.png")

# Table 3 -----------------------------------------------------------------


# Table 3
# Voting for Democratic Party
# model 1
table_3_spec_form_1 <- glue("vote_pd_euro ~ {str_c(variables2, collapse = ' + ')} + {str_c(edu_names, collapse = ' + ')} + {str_c(inc_names, collapse = ' + ')}") 
table_3_fit_1 <- lm(as.formula(table_3_spec_form_1), data = rep_dat_1)
modelsummary(table_3_fit_1, vcov = sandwich::vcovHC(table_3_fit_1, type = "HC1"))

# model 2
table_3_spec_form_2 <- glue("vote_pd_euro ~ {str_c(variables2, collapse = ' + ')} + {str_c(edu_names, collapse = ' + ')} + {str_c(inc_names, collapse = ' + ')} + vote_pd_2018") 
table_3_fit_2 <- lm(as.formula(table_3_spec_form_2), data = rep_dat_4)
modelsummary(table_3_fit_2, vcov = sandwich::vcovHC(table_3_fit_2, type = "HC1"))

# model 3
table_3_spec_form_3 <- glue("vote_pd_euro ~ {str_c(variables2, collapse = ' + ')} + {str_c(edu_names, collapse = ' + ')} + {str_c(inc_names, collapse = ' + ')} + vote_pd_regional") 
table_3_fit_3 <- lm(as.formula(table_3_spec_form_3), data = rep_dat_5)
modelsummary(table_3_fit_3, vcov = sandwich::vcovHC(table_3_fit_3, type = "HC1"))

# model 4
table_3_spec_form_4 <- glue("vote_pd_euro ~ {str_c(variables2, collapse = ' + ')} + {str_c(edu_names, collapse = ' + ')} + {str_c(inc_names, collapse = ' + ')} + vote_pd_municipal") 
table_3_fit_4 <- lm(as.formula(table_3_spec_form_4), data = rep_dat_6)
modelsummary(table_3_fit_4, vcov = sandwich::vcovHC(table_3_fit_4, type = "HC1"))

# Voting for Forza Italia
# model 5
table_3_spec_form_5 <- glue("vote_forzaitalia_euro ~ {str_c(variables2, collapse = ' + ')} + {str_c(edu_names, collapse = ' + ')} + {str_c(inc_names, collapse = ' + ')}") 
table_3_fit_5 <- lm(as.formula(table_3_spec_form_5), data = rep_dat_1)
modelsummary(table_3_fit_5, vcov = sandwich::vcovHC(table_3_fit_5, type = "HC1"))

# model 6
table_3_spec_form_6 <- glue("vote_forzaitalia_euro ~ {str_c(variables2, collapse = ' + ')} + {str_c(edu_names, collapse = ' + ')} + {str_c(inc_names, collapse = ' + ')} + vote_forzaitalia_2018") 
table_3_fit_6 <- lm(as.formula(table_3_spec_form_6), data = rep_dat_4)
modelsummary(table_3_fit_6, vcov = sandwich::vcovHC(table_3_fit_6, type = "HC1"))

# model 7
table_3_spec_form_7 <- glue("vote_forzaitalia_euro ~ {str_c(variables2, collapse = ' + ')} + {str_c(edu_names, collapse = ' + ')} + {str_c(inc_names, collapse = ' + ')} + vote_forzaitalia_regional") 
table_3_fit_7 <- lm(as.formula(table_3_spec_form_7), data = rep_dat_5)
modelsummary(table_3_fit_7, vcov = sandwich::vcovHC(table_3_fit_7, type = "HC1"))

# model 8
table_3_spec_form_8 <- glue("vote_forzaitalia_euro ~ {str_c(variables2, collapse = ' + ')} + {str_c(edu_names, collapse = ' + ')} + {str_c(inc_names, collapse = ' + ')} + vote_forzaitalia_municipal") 
table_3_fit_8 <- lm(as.formula(table_3_spec_form_8), data = rep_dat_6)
modelsummary(table_3_fit_8, vcov = sandwich::vcovHC(table_3_fit_8, type = "HC1"))

#Voting for Five Star Movement
# model 9
table_3_spec_form_9 <- glue("vote_m5s_euro ~ {str_c(variables2, collapse = ' + ')} + {str_c(edu_names, collapse = ' + ')} + {str_c(inc_names, collapse = ' + ')}") 
table_3_fit_9 <- lm(as.formula(table_3_spec_form_9), data = rep_dat_1)
modelsummary(table_3_fit_9, vcov = sandwich::vcovHC(table_3_fit_9, type = "HC1"))

# model 10
table_3_spec_form_10 <- glue("vote_m5s_euro ~ {str_c(variables2, collapse = ' + ')} + {str_c(edu_names, collapse = ' + ')} + {str_c(inc_names, collapse = ' + ')} + vote_m5s_2018") 
table_3_fit_10 <- lm(as.formula(table_3_spec_form_10), data = rep_dat_4)
modelsummary(table_3_fit_10, vcov = sandwich::vcovHC(table_3_fit_10, type = "HC1"))

# model 11
table_3_spec_form_11 <- glue("vote_m5s_euro ~ {str_c(variables2, collapse = ' + ')} + {str_c(edu_names, collapse = ' + ')} + {str_c(inc_names, collapse = ' + ')} + vote_m5s_regional") 
table_3_fit_11 <- lm(as.formula(table_3_spec_form_11), data = rep_dat_5)
modelsummary(table_3_fit_11, vcov = sandwich::vcovHC(table_3_fit_11, type = "HC1"))

# model 12
table_3_spec_form_12 <- glue("vote_m5s_euro ~ {str_c(variables2, collapse = ' + ')} + {str_c(edu_names, collapse = ' + ')} + {str_c(inc_names, collapse = ' + ')} + vote_m5s_municipal") 
table_3_fit_12 <- lm(as.formula(table_3_spec_form_12), data = rep_dat_6)
modelsummary(table_3_fit_12, vcov = sandwich::vcovHC(table_3_fit_12, type = "HC1"))

table_3_models_list <- lapply(1:12, function(i) {
  get(paste0("table_3_fit_", i))
})

table_3_model_names <- lapply(1:12, function(i) paste0("table_3_fit_", i))

table_3_robust_se_list <- lapply(table_3_model_names, function(model) {
  sandwich::vcovHC(get(model), type = "HC1")
})

m2 <- modelsummary(table_3_models_list, vcov = table_3_robust_se_list,
                                coef_omit = c("(Intercept)|EDU|INC"),
                                coef_rename = c("diesel_euro4" = "Diesel x Euro 4",
                                                "dummy_diesel" = "Diesel",
                                                "dummy_euro_4" = "Euro 4",
                                                "age" = "Age",
                                                "female" = "Female",
                                                "vote_pd_2018" = "Past Vote",
                                                "vote_pd_regional" = "Past Vote",
                                                "vote_pd_municipal"= "Past Vote",
                                                "vote_forzaitalia_2018" = "Past Vote",
                                                "vote_forzaitalia_regional" = "Past Vote",
                                                "vote_forzaitalia_municipal" = "Past Vote",
                                                "vote_m5s_2018" = "Past Vote",
                                                "vote_m5s_regional" = "Past Vote",
                                                "vote_m5s_municipal" = "Past Vote"),
                                add_rows = data.frame(intercept = c("Education F.E.", "Income F.E.", "Lagged Vote"), 
                                                      table_3_fit_1 = c("Yes", "Yes", "No"), table_3_fit_2 = c("Yes", "Yes", "L2018"), 
                                                      table_3_fit_3 = c("Yes", "Yes", "R2018"),  table_3_fit_4 = c("Yes", "Yes", "M2016"), 
                                                      table_3_fit_5 = c("Yes", "Yes", "No"), table_3_fit_6 = c("Yes", "Yes", "L2018"), 
                                                      table_3_fit_7 = c("Yes", "Yes", "R2018"),  table_3_fit_8 = c("Yes", "Yes", "M2016"), 
                                                      table_3_fit_9 = c("Yes", "Yes", "No"), table_3_fit_10 = c("Yes", "Yes", "L2018"), 
                                                      table_3_fit_11 = c("Yes", "Yes", "R2018"),  table_3_fit_12 = c("Yes", "Yes", "M2016")),
                                gof_map = c("Education F.E.", "Income F.E.", "Past Lega Vote", "nobs", "r.squared"), 
                                object = "gt", 
                   stars = c('*' = .05, '**' = .01))


gt_2 <- m2$`_data`

gt_2 <- 
  gt_2 |>
  gt(rowname_col = "row") |>
  tab_spanner(label = "Voting For Democratic Party",
              columns = c( 2, 3, 4, 5 )) |> 
  tab_spanner(label = "Voting For Forza Italia",
              columns = c(6, 7, 8, 9)) |> 
  tab_spanner(label = "Voting For Five Star Movement", 
              columns = c(10, 11, 12, 13)) |> 
  tab_spanner(label = "Dependent Variable: Vote For Other Parties 2019",
              columns = c( 2, 3, 4, 5, 6, 7, 8, 9 , 10, 11, 12, 13)) |>
  cols_align(align = "center") |> 
  tab_footnote("Note: All columns report estimates from regression models that include controls for age and gender, as well as fixed effects for education levels and income brackets. Columns 2–4, 6–8, and 10–12 include dummies for past vote in legislative, regional, and municipal elections, respectively. Robust standard errors are in parentheses. *p < 0.05; **p < 0.01.")

gtsave(gt_2, "dataverse_files/tb4.png")

# Table 5 (with compensation) ---------------------------------------------

#no covariates
var1 <- c( "diesel_euro4", "compensated", "dummy_diesel", "dummy_euro_4", "age", "female")
form1 <- glue("vote_lega_euro ~  {str_c(var1, collapse = ' + ')} + {str_c(edu_names, collapse = ' + ')} + {str_c(inc_names, collapse = ' + ')}")
fit11 <- lm(as.formula(form1), data = rep_dat_1)


#Switching from legislative
rep_data2 <- 
  replication_dataset |>  
  filter(target != 3 & no_answer_euro == 0 & no_answer_2018 == 0 & target != 4)
form2 <- glue("sw_to_lega_18_19 ~  {str_c(var1, collapse = ' + ')} + {str_c(edu_names, collapse = ' + ')} + {str_c(inc_names, collapse = ' + ')}")
fit22 <- lm(as.formula(form2), data = rep_data2)

#Switching from regional
rep_data3 <- 
  replication_dataset |>  
  filter(target != 3 & no_answer_euro == 0 & no_answer_regional == 0 & target != 4)
form3 <- glue("sw_to_lega_reg_19 ~  {str_c(var1, collapse = ' + ')} + {str_c(edu_names, collapse = ' + ')} + {str_c(inc_names, collapse = ' + ')}")
fit33 <- lm(as.formula(form3), data = rep_data3)

#Switching from municipality
rep_data4 <- 
  replication_dataset |>  
  filter(target != 3 & no_answer_euro == 0 & no_answer_municipal == 0 & target != 4)

form4 <- glue("sw_to_lega_16_19 ~  {str_c(var1, collapse = ' + ')} + {str_c(edu_names, collapse = ' + ')} + {str_c(inc_names, collapse = ' + ')}")

fit44 <- lm(as.formula(form4), data = rep_data4)

robust_se_list2 <- list(  
  sandwich::vcovHC(fit11, type = "HC1"), sandwich::vcovHC(fit22, type = "HC1"),
  sandwich::vcovHC(fit33, type = "HC1"), sandwich::vcovHC(fit44, type = "HC1"))

m3 <- modelsummary(list(fit11, fit22, fit33, fit44), vcov = robust_se_list2, 
             coef_rename = c(diesel_euro4 = "Diesel x Euro 4", compensated = "Compensated",
                             dummy_diesel = "Diesel", dummy_euro_4 = "Euro 4", 
                             age = "Age", female = "Female"), 
             coef_omit = "INC|EDU|(Intercept)",
             add_rows = data.frame(intercept = c("Education F.E.", "Income F.E.", "Switch Form:"), 
                                   fit1 = c("Yes", "Yes", ""), fit2 = c("Yes", "Yes", "L2018"), 
                                   fit3 = c("Yes", "Yes", "R2018"), fit4 = c("Yes", "Yes", "M2016")),
             gof_map = c("Switch From:", "Education F.E.", "Income F.E.", "nobs", "r.squared"), 
             output = "gt", 
             stars = c('*' = .05, '**' = .01))

gt_3 <- m3$`_data`

gt_3 <- 
  gt_3 |>
  gt(rowname_col = "row") |>
  tab_spanner(label = "Voting For Lega",
              columns = 2) |> 
  tab_spanner(label = "Switch To Lega",
              columns = c(3, 4, 5)) |> 
  cols_align(align = "center") |>
  tab_footnote("Note: In column 1, the dependent variable is an indicator for voting Lega in the EU elections of 2019. In columns 2–4, the dependent variables are indicators for switching to Lega from the legislative 2018, regional 2018, and municipal elections 2016, respectively. All regressions include individual controls, as in the benchmark specification of column 2 in Table 2. Robust standard errors are in parentheses. *p < 0.05; **p < 0.01.")

gtsave(gt_3, "dataverse_files/tb5.png")
# Extension ---------------------------------------------------------------
# #Income Sub Sample ------------------------------------------------------
#full sample: 
ext_var1 <- c("diesel_euro4", "dummy_diesel", "dummy_euro_4", "age", "female")

ext_data1 <- 
  replication_dataset |>  
  filter(INC15 == 0 & INC16 == 0 & target != 3 & no_answer_euro == 0 & target != 4) |>  
  mutate(income_cat = case_when(profile_gross_personal_eu %in% c(1, 2, 3, 4, 5, 6) ~ "Low Income",
                                profile_gross_personal_eu %in% c(7, 8, 9, 10, 11, 12)  ~ "Middle Income", 
                                profile_gross_personal_eu %in% c(13, 14) ~ "High Income"))

ext_cont1 <- glue("vote_lega_euro ~  {str_c(ext_var1, collapse = ' + ')} + vote_lega_2018 + {str_c(edu_names, collapse = ' + ')}")

ext_fit1 <- lm(as.formula(ext_cont1), data = ext_data1)

#Low Income
ext_data2 <- 
  replication_dataset |>  
  filter(INC15 == 0 & INC16 == 0 & target != 3 & no_answer_euro == 0 & target != 4) |>  
  mutate(income_cat = case_when(profile_gross_personal_eu %in% c(1, 2, 3, 4, 5, 6) ~ "Low Income",
                                profile_gross_personal_eu %in% c(7, 8, 9, 10, 11, 12)  ~ "Middle Income", 
                                profile_gross_personal_eu %in% c(13, 14) ~ "High Income")) |>  
  filter(income_cat == "Low Income")

ext_cont2 <- glue("vote_lega_euro ~  {str_c(ext_var1, collapse = ' + ')} + vote_lega_2018 + {str_c(edu_names, collapse = ' + ')}")

ext_fit2 <- lm(as.formula(ext_cont1), data = ext_data2)

#Middle Income
ext_data3 <- 
  replication_dataset |>  
  filter(INC15 == 0 & INC16 == 0 & target != 3 & no_answer_euro == 0 & target != 4) |>  
  mutate(income_cat = case_when(profile_gross_personal_eu %in% c(1, 2, 3, 4, 5, 6) ~ "Low Income",
                                profile_gross_personal_eu %in% c(7, 8, 9, 10, 11, 12)  ~ "Middle Income", 
                                profile_gross_personal_eu %in% c(13, 14) ~ "High Income")) |>  
  filter(income_cat == "Middle Income")

ext_cont3 <- glue("vote_lega_euro ~  {str_c(ext_var1, collapse = ' + ')} + vote_lega_2018 + {str_c(edu_names, collapse = ' + ')}")

ext_fit3 <- lm(as.formula(ext_cont1), data = ext_data3)

#High Income 
ext_data4 <- 
  replication_dataset |>  
  filter(INC15 == 0 & INC16 == 0 & target != 3 & no_answer_euro == 0 & target != 4) |>  
  mutate(income_cat = case_when(profile_gross_personal_eu %in% c(1, 2, 3, 4, 5, 6) ~ "Low Income",
                                profile_gross_personal_eu %in% c(7, 8, 9, 10, 11, 12)  ~ "Middle Income", 
                                profile_gross_personal_eu %in% c(13, 14) ~ "High Income")) |>  
  filter(income_cat == "High Income")

ext_cont4 <- glue("vote_lega_euro ~  {str_c(ext_var1, collapse = ' + ')} + vote_lega_2018 + {str_c(edu_names, collapse = ' + ')}")

ext_fit4 <- lm(as.formula(ext_cont1), data = ext_data4)

robust_se_list3 <- list(  
  sandwich::vcovHC(ext_fit1, type = "HC1"), sandwich::vcovHC(ext_fit2, type = "HC1"),
  sandwich::vcovHC(ext_fit3, type = "HC1"), sandwich::vcovHC(ext_fit4, type = "HC1"))

m4 <- modelsummary(list(ext_fit1, ext_fit2, ext_fit3, ext_fit4), 
                   vcov = robust_se_list3, 
             coef_rename = c(diesel_euro4 = "Diesel x Euro 4", 
                             dummy_diesel = "Diesel", 
                             dummy_euro_4 = "Euro 4", 
                             age = "Age", female = "Female"),
             coef_omit = c("(Intercept)|EDU"),
             add_rows = data.frame(intercept = c("Education F.E."), 
                                   ext_fit1 = "Yes", ext_fit2 = "Yes", 
                                   ext_fit3 = "Yes", ext_fit4 = "Yes"),
             gof_map = c("Education F.E.", "nobs", "r.squared"), 
             output = "gt", 
             stars = c('*' = .05, '**' = .01))

gt_4 <- m4$`_data`

gt_4 <- 
  gt_4 |>
  gt(rowname_col = "row") |>
  tab_spanner(label = "Full Sample",
              columns = 2) |> 
  tab_spanner(label = "Low Income",
              columns = 3) |>
  tab_spanner(label = "Middle Income", 
              columns = 4) |> 
  tab_spanner(label = "High Income",
              columns = 5) |> 
  tab_spanner(label = "Dependent Variable: Vote For Lega 2019", 
              columns = c(2, 3, 4, 5)) |>  
  cols_align(align = "center") |> 
  tab_footnote("Note: The estimates shown in this table are from regression models that include controls for age and gender as well as education fixed effects. Column 1 studies the entire sample where as columns 2, 3, and 4 are based on sub samples for low, middle and high income. The regression model also contains the dummy for past lega vote in legislative elections. Robust standard errors are in parentheses. *p < 0.05; **p < 0.01.")

gtsave(gt_4, "dataverse_files/tb6.png")
# #Triple Difference -------------------------------------------------------------
# Extension triple dif-in-dif
regression_dat_6 <- 
  replication_dataset |>
  filter(target!=3 & target!=4 & no_answer_euro==0 & INC15 != 1 & INC16 != 1)

triple_dif_in_dif <- 
  regression_dat_6 |> 
  mutate(income_grouped = case_when(INC1 == 1 | INC2 == 1 | INC3 == 1 | INC4 == 1 | INC5 == 1 | INC6 == 1 
                                    | INC7 == 1 | INC8 == 1 | INC9 == 1  ~ "Low Income Group", 
                                    INC10 == 1 | INC11 == 1 | INC12 == 1 | INC13 == 1 | INC14 == 1~ "High Income Group"))

edu_names1 <- str_subset(colnames(rep_dat_1), "EDU") 

spec_form_triple_dif_1 <- glue("vote_lega_euro ~ dummy_diesel:dummy_euro_4:income_grouped + dummy_diesel + dummy_euro_4 + income_grouped + dummy_diesel:dummy_euro_4 + dummy_diesel:income_grouped + dummy_euro_4:income_grouped  + female") 
triple_dif_1 <- lm(as.formula(spec_form_triple_dif_1), data = triple_dif_in_dif)
modelsummary(triple_dif_1 , vcov = sandwich::vcovHC(triple_dif_1 , type = "HC1"))

# here is a comparison bet triple dif-in-dif and subsampling, for triple dif to work, classical design is including another binary outcome, so I simply categorize income into high and low, this is the subsampling result, but they still don't match
regression_dat_7 <- 
  triple_dif_in_dif |>
  filter(income_grouped == "Low Income Group")

spec_form_low_inc_1 <- glue("vote_lega_euro ~ dummy_diesel:dummy_euro_4 + dummy_diesel + dummy_euro_4 + female") 
low_inc_1 <- lm(as.formula(spec_form_low_inc_1 ), data = regression_dat_7)
modelsummary(low_inc_1, vcov = sandwich::vcovHC(low_inc_1 , type = "HC1"))

regression_dat_8 <- 
  triple_dif_in_dif |>
  filter(income_grouped == "High Income Group")

spec_form_high_inc_1 <- glue("vote_lega_euro ~ dummy_diesel:dummy_euro_4 + dummy_diesel + dummy_euro_4 + female") 
high_inc_1 <- lm(as.formula(spec_form_high_inc_1 ), data = regression_dat_8)
modelsummary(list(low_inc_1, high_inc_1), vcov = sandwich::vcovHC(high_inc_1 , type = "HC1"))

spec_form_low_inc_2 <- glue("vote_lega_euro ~ dummy_diesel:dummy_euro_4 + dummy_diesel + dummy_euro_4 + age + female+ {str_c(edu_names1, collapse = ' + ')}") 
low_inc_2 <- lm(as.formula(spec_form_low_inc_2 ), data = regression_dat_7)

spec_form_high_inc_2 <- glue("vote_lega_euro ~ dummy_diesel:dummy_euro_4 + dummy_diesel + dummy_euro_4 + age + female + {str_c(edu_names1, collapse = ' + ')}") 
high_inc_2 <- lm(as.formula(spec_form_high_inc_2 ), data = regression_dat_8)

robust_se_list4 <- list(  
  sandwich::vcovHC(triple_dif_1, type = "HC1"), sandwich::vcovHC(low_inc_1, type = "HC1"),
  sandwich::vcovHC(high_inc_1, type = "HC1"), sandwich::vcovHC(low_inc_2, type = "HC1"), 
  sandwich::vcovHC(high_inc_2, type = "HC1"))
coef_mapping <- list("dummy_diesel" = "Diesel", 
                     "dummy_euro_4" = "Euro 4",
                     "dummy_diesel:dummy_euro_4" = "Diesel Euro 4",
                     "female" = "Female",
                     "dummy_diesel:low_income" = "Diesel x Low Income",
                     "dummy_euro_4:low_income" = "Euro 4 x Low Income",
                     "dummy_diesel:dummy_euro_4:low_income" = "Diesel x Euro 4 x Low Income",
                     "low_income" = "Low Income Group")

m5 <- modelsummary(list(triple_dif_1, low_inc_1, high_inc_1, low_inc_2, high_inc_2), 
                   vcov = robust_se_list4,
                   coef_omit = c("(Intercept)|EDU"),
                   gof_map = c("nobs", "r.squared"),
                   coef_map = coef_mapping,
                   output = "gt", 
                   stars = c('*' = .05, '**' = .01), 
                   add_rows = data.frame(intercept = c("Age Control", "Education F.E."), 
                                         triple_dif_1 = c("No", "No"), low_inc_1 = c("No", "No"), 
                                         high_inc_1 = c("No","No"), low_inc_2 = c("Yes", "Yes"), high_inc_2 = c("Yes", "Yes")))

gt_5 <- m5$`_data`
gt_5 <- 
  gt_5 |> 
  gt(rowname_col = "row") |>
  tab_spanner(label = "DDD",
              columns = 2) |> 
  tab_spanner(label = "Low Income without Controls ",
              columns = 3) |>
  tab_spanner(label = "High Income without Controls", 
              columns = 4) |> 
  tab_spanner(label = "Low Income with Controls",
              columns = 5) |>
  tab_spanner(label = "High Income with Controls",
              columns = 6) |>  
  tab_spanner(label = "Dependent Variable: Vote For Lega 2019", 
              columns = c(2, 3, 4, 5, 6)) |>  
  cols_align(align = "center") |> 
  tab_footnote("Note:Column 1 reports estimates from DDD that controls for income; the coefficient of interest is Diesel x Euro 4 x Low Income. Column 2 and 3 reports estimates of treatment effects for low and high income groups in the estimated coefficients on Diesel Euro 4, the difference of which are compared with that of DDD. Column 4 and 5 add age and education as proxies for income, and the difference in Diesel Euro 4 estimates are again compared with that of DDD.")

gtsave(gt_5, "dataverse_files/tb7.png")

# omitting control group with cost from policy
cleaned_dat <- 
  replication_dataset |>
  filter(diesel_euro4 == 1 & cost_area_b != 8 | diesel_euro4 == 0 & cost_area_b == 1)

cleaned_regression_dat <- 
  cleaned_dat |>
  filter(target!=3 & target!=4 & no_answer_euro==0)

# showing result for column 1 and 2 in Table 2, and column 1 in compensation for this smaller sample

cleaned_spec_form_2 <- glue("vote_lega_euro ~ diesel_euro4 + dummy_diesel + dummy_euro_4 + age + female + {str_c(edu_names1, collapse = ' + ')} + {str_c(inc_names, collapse = ' + ')}") 

cleaned_fit2 <- lm(as.formula(cleaned_spec_form_2), data = cleaned_regression_dat)

robust_se_list5 <- list(sandwich::vcovHC(cleaned_fit2, type = "HC1"), sandwich::vcovHC(fit2, type = "HC1"))

m6 <- modelsummary(list(cleaned_fit2, fit2) , vcov = robust_se_list5, 
                   coef_omit = "INC|(Intercept)|EDU",
                   coef_rename = c(diesel_euro4 = "Diesel x Euro 4", 
                                   dummy_diesel = "Diesel", 
                                   dummy_euro_4 = "Euro 4", 
                                   age = "Age", female = "Female"), 
             output = "gt", 
             stars = c('*' = .05, '**' = .01), 
             gof_map = c("Education F.E.", "nobs", "r.squared"), 
             add_rows = data.frame(intercept = c("Education F.E.", "Income F.E."), 
                                   cleaned_fit2 = c("Yes", "Yes"), fit2 = c("Yes","Yes")))

gt_6 <- m6$'_data'

gt_6 <- 
  gt_6 |> 
  gt(rowname_col = "row") |>
  tab_spanner(label = "Cleaned Sample",
              columns = 2) |> 
  tab_spanner(label = "Full Sample",
              columns = 3) |>
  cols_align(align = "center") |> 
  tab_footnote("The estimates shown in this table are from the regression model that include controls for age and gender as well as income and education fixed effects. Column 1 shows the results for the cleaned sample that does not include observations who reported a cost even though they did not own a diesel euro 4 car. Column 2 replicates the authors results for this regression specification with the full sample. Robus standard errors are in parentheses. *p < 0.05; **p < 0.01")

gtsave(gt_6, "dataverse_files/tb8.png")