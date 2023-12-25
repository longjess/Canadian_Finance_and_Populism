library(tidyverse)
library(haven)
library(fastDummies)
library(nnet)
library(lmtest)
library(DescTools)

setwd("C:/Jessica/Stanford/Fall 2023/STATS 209/Project")

data_raw <- read_dta('2021 Canadian Election Study v1.0.dta')

data_selected <- data_raw %>%
  select(pes21_populism_2, pes21_populism_3, 
         pes21_populism_4, pes21_populism_7, pes21_populism_8,
         pes21_campatt, 
         pes21_follow_pol, 
         pes21_rural_urban,
         cps21_language_1,
         cps21_language_2,
         cps21_genderid,
         cps21_education,
         cps21_religion,
         cps21_rel_imp,
         cps21_own_fin_retro,
         cps21_union,
         cps21_marital, 
         cps21_yob,
         cps21_bornin_canada,
         pes21_parents_born,
         cps21_employment,
         pes21_votechoice2021,
         Region) 
save(data_selected, file="data_selected.RData")
load("data_selected.RData")

data_filtered <- data_selected %>%
  drop_na(pes21_populism_2, pes21_populism_3, 
          pes21_populism_4, pes21_populism_7, pes21_populism_8) %>%
  # Filter out don't know/prefer not answer responses
  dplyr::filter(pes21_populism_2 != 6,
                pes21_populism_3 != 6,
                pes21_populism_4 != 6,
                pes21_populism_7 != 6,
                pes21_populism_8 != 6)

# Full set
data_cleaned_full <- data_filtered %>%
  rename(region = Region) %>%
  dplyr::filter(region != "Territories") %>% 
  mutate(attention = case_when(
    pes21_campatt == 1 ~ 3,
    pes21_campatt == 2 ~ 2,
    pes21_campatt == 3 ~ 1,
    pes21_campatt == 4 ~ 0
  )) %>%
  dplyr::filter(attention != 0) %>% 
  mutate(follow_pol = case_when(
    pes21_follow_pol == 1 ~ 4,
    pes21_follow_pol == 2 ~ 3,
    pes21_follow_pol == 3 ~ 2,
    pes21_follow_pol == 4 ~ 1,
    pes21_follow_pol == 5 ~ 0
  )) %>%
  dplyr::filter(follow_pol != 0) %>% 
  mutate(rural_urban = case_when(
    pes21_rural_urban == 1 ~ "Rural", 
    pes21_rural_urban == 2 ~ "Small_town",
    pes21_rural_urban == 3 ~ "Middle_town",
    pes21_rural_urban == 4 ~ "Suburb",
    pes21_rural_urban == 5 ~ "City",
    !(pes21_rural_urban %in% c(1:5)) ~ "No_answer")) %>%
  dplyr::filter(rural_urban != "No_answer") %>% 
  mutate(native_anglophone = case_when(
    cps21_language_1 == 1 ~ 1,
    cps21_language_1 != 1 ~ 0)) %>% 
  mutate(native_francophone = case_when(
    cps21_language_2 == 1 ~ 1,
    cps21_language_2 != 1 ~ 0)) %>%
  mutate(male = case_when(
    cps21_genderid == 1 ~ 1,
    !(cps21_genderid == 1) ~ 0)) %>%
  mutate(education = case_when(
    cps21_education %in% c(1:4) ~ "Less_than_secondary",
    cps21_education == 5 ~ "Completed_secondary",
    cps21_education == 6 ~ "Some_college",
    cps21_education == 7 ~ "Completed_college",
    cps21_education == 8 ~ "Some_university",
    cps21_education == 9 ~ "Bachelors_degree",
    cps21_education == 10 ~ "Masters_degree",
    cps21_education == 11 ~ "Professional_degree_or_doctorate",
    !(cps21_education %in% c(1:11)) ~ "No_answer")) %>%
  dplyr::filter(education != "No_answer") %>% 
  mutate(religion = case_when(
    cps21_religion %in% c(1:2) ~ "Non_religious", 
    cps21_religion %in% c(8, 13, 16, 18) ~ "Mainline_Protestant",
    cps21_religion %in% c(9, 15, 17, 19, 20, 21) ~ "Other_Protestant",
    cps21_religion == 10 ~ "Catholic",
    cps21_religion %in% c(11, 12, 14) ~ "Other_Christian",
    cps21_religion %in% c(3, 4, 5, 6, 7, 22) ~ "Other_religion",
    !(cps21_religion %in% c(1:22)) ~ "No_answer")) %>%
  mutate(religion_importance = case_when(
    cps21_rel_imp == 1 ~ 4, 
    cps21_rel_imp == 2 ~ 3,
    cps21_rel_imp == 3 ~ 2,
    cps21_rel_imp == 4 ~ 1,
    !(cps21_rel_imp %in% c(1:4)) ~ 0
  )) %>%
  mutate(finance = case_when(
    cps21_own_fin_retro == 1 ~ 3,
    cps21_own_fin_retro == 2 ~ 2,
    cps21_own_fin_retro == 3 ~ 1,
    cps21_own_fin_retro == 4 ~ 0
  )) %>%
  dplyr::filter(finance != 0) %>% 
  mutate(union = case_when(
    cps21_union == 1 ~ 1, 
    cps21_union != 1 ~ 0)) %>%
  mutate(marital = case_when(
    cps21_marital == 1 ~ "Married", 
    cps21_marital == 2 ~ "Living_w_partner",
    cps21_marital == 3 ~ "Divorced",
    cps21_marital == 4 ~ "Seperated",
    cps21_marital == 5 ~ "Widowed",
    cps21_marital == 6 ~ "Never_married",
    !(cps21_marital %in% c(1:6)) ~ "No_answer")) %>%
  dplyr::filter(marital != "No_answer") %>% 
  mutate(age = cps21_yob) %>%
  mutate(bornin_canada = case_when(
    cps21_bornin_canada == 1 ~ 1, 
    cps21_bornin_canada != 1 ~ 0)) %>%
  mutate(parents_born = case_when(
    pes21_parents_born == 1 ~ 1, 
    pes21_parents_born != 1 ~ 0)) %>%
  mutate(employment = case_when(
    cps21_employment == 1 ~ "Full_time",
    cps21_employment == 2 ~ "Part_time",
    cps21_employment == 3 ~ "Self_employed",
    cps21_employment %in% c(4, 11) ~ "Retired",
    cps21_employment == 5 ~ "Unemployed",
    cps21_employment %in% c(6, 9) ~ "Student",
    cps21_employment %in% c(7, 10) ~ "Family_care",
    cps21_employment == 8 ~ "Disabled",
    cps21_employment %in% c(12, 13) ~ "Other"
  )) %>%
  mutate(vote = case_when(
    pes21_votechoice2021 == 1 ~ "Liberal",
    pes21_votechoice2021 == 2 ~ "Conservative",
    pes21_votechoice2021 == 3 ~ "NDP",
    pes21_votechoice2021 == 4 ~ "Bloc_Quebecois",
    pes21_votechoice2021 == 5 ~ "Green",
    pes21_votechoice2021 == 6 ~ "Peoples",
    pes21_votechoice2021 == 7 ~ "Other",
    pes21_votechoice2021 == 8 ~ "Spoiled",
    pes21_votechoice2021 %in% c(9, NA) ~ "NA"
  ))  %>%
  dplyr::filter(!(vote %in% c("Other", "Spoiled")))
save(data_cleaned_full, file="data_cleaned_full.RData")

data_cleaned <- data_cleaned_full %>%
  # 1 if finance == 1, i.e. situation got worse
  mutate(finance = ifelse(finance == 1, 1, 0)) %>%
  mutate(populism = (1/5)*(pes21_populism_2 +
                             pes21_populism_3 + 
                             abs(5 - pes21_populism_4) + 
                             pes21_populism_7 +
                             pes21_populism_8)) %>%
  select(-c(pes21_populism_2, pes21_populism_3, 
            pes21_populism_4, pes21_populism_7, pes21_populism_8,
            pes21_campatt, 
            pes21_follow_pol, 
            pes21_rural_urban,
            cps21_language_1,
            cps21_language_2,
            cps21_genderid,
            cps21_education,
            cps21_religion,
            cps21_rel_imp,
            cps21_own_fin_retro,
            cps21_union,
            cps21_marital, 
            cps21_yob,
            cps21_bornin_canada,
            pes21_parents_born,
            cps21_employment,
            pes21_votechoice2021)) %>%
  drop_na()
save(data_cleaned, file="data_cleaned.RData")

data_cleaned_2 <- data_cleaned_full  %>%
  # 1 if finance == 3, i.e. situation got better
  mutate(finance = ifelse(finance == 3, 1, 0)) %>%
  mutate(populism = (1/5)*(pes21_populism_2 +
                             pes21_populism_3 + 
                             abs(5 - pes21_populism_4) + 
                             pes21_populism_7 +
                             pes21_populism_8)) %>%
  select(-c(pes21_populism_2, pes21_populism_3, 
            pes21_populism_4, pes21_populism_7, pes21_populism_8,
            pes21_campatt, 
            pes21_follow_pol, 
            pes21_rural_urban,
            cps21_language_1,
            cps21_language_2,
            cps21_genderid,
            cps21_education,
            cps21_religion,
            cps21_rel_imp,
            cps21_own_fin_retro,
            cps21_union,
            cps21_marital, 
            cps21_yob,
            cps21_bornin_canada,
            pes21_parents_born,
            cps21_employment,
            pes21_votechoice2021)) %>%
  drop_na()
save(data_cleaned_2, file="data_cleaned_2.RData")

data_cleaned_3 <- data_cleaned_full  %>%
  # 1 if finance == 1, i.e. situation got worse
  mutate(finance = ifelse(finance == 1, 1, 0)) %>%
  select(-c(pes21_campatt, 
            pes21_follow_pol, 
            pes21_rural_urban,
            cps21_language_1,
            cps21_language_2,
            cps21_genderid,
            cps21_education,
            cps21_religion,
            cps21_rel_imp,
            cps21_own_fin_retro,
            cps21_union,
            cps21_marital, 
            cps21_yob,
            cps21_bornin_canada,
            pes21_parents_born,
            cps21_employment,
            pes21_votechoice2021)) %>%
  drop_na()
save(data_cleaned_3, file="data_cleaned_3.RData")
