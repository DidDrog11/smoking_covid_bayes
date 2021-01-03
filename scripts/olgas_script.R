library(tidyverse)
library(here)
library(linelist)
library(boot)
library(googlesheets4)

source(here("scripts", "bootstrap_function.R"))

smoking_status <- c('current_smoker', 'former_smoker', 'current_former_smoker', 'never_smoker', 'never_smoker_unknown', 'not_stated', 'missing')
data_study_general <- read_rds(here("data_clean", "data_study_general.rds")) %>%
  filter(!lead_author %in% exclude_from_analysis)
table_1 <- read_rds(here("data_clean", "table_1.rds")) %>%
  filter(!lead_author %in% exclude_from_analysis)

prevalence_plot <- table_1 %>%
  left_join(., data_study_general %>%
              select(study_id, study_setting), 
            by = "study_id") %>%
  group_by(country, study_setting) %>%
  rename('sample' = total) %>%
  filter(current_smoker != 'NA') %>%
  filter(study_setting != is.na(study_setting)) %>%
  mutate(ever_smoker = former_smoker + current_smoker + current_former_smoker,
         not_stated_missing = missing + not_stated + never_smoker_unknown) %>%
  select(study_id, lead_author, country, sample, current_smoker, former_smoker, never_smoker, ever_smoker, not_stated_missing) %>%
  mutate(total = never_smoker + ever_smoker + not_stated_missing) %>%
  mutate(p_current_smoker = ifelse(current_smoker == NAcurrent_smoker/total,
         p_ever_smoker = ever_smoker/total,
         p_former_smoker = former_smoker/total,
         p_never_smoker = never_smoker/total,
         p_ever_smoker = ever_smoker/total,
         p_not_stated_missing = not_stated_missing/total,
         p_total = p_ever_smoker + p_never_smoker + p_not_stated_missing) %>%
  mutate(study = 1) %>%
  add_count(country) 


prevalence_plot$country <- as.factor(prevalence_plot$country)

google_smoking_prevalence <-  readxl::read_xlsx(here("data_spreadsheet", "data_extraction_current.xlsx"), sheet = 'national_smoking_prevalence') 
national_smoking_prevalence <- google_smoking_prevalence %>%
  select(-Source) %>%
  mutate(study = 0,
         Current = Current/100,
         Former = Former/100) %>%
  rename("country" = Country,
         "current_smoking_p" = Current,
         "former_smoking_p" = Former) %>%
  clean_data()


country_list_ordered <- sort(national_smoking_prevalence$country)

b <- prevalence_plot %>%
  ungroup() %>%
  filter(country != 'multiple') %>%
  mutate(study_id = 1:nrow(.)) %>%
  mutate(current_smoking_p = p_current_smoker,
         former_smoking_p = p_former_smoker,
         true_sample = sample) %>%
  add_row(country = national_smoking_prevalence$country,
          current_smoking_p = national_smoking_prevalence$current_smoking_p, 
          former_smoking_p = national_smoking_prevalence$former_smoking_p, 
          study = national_smoking_prevalence$study) %>%
  select(country, sample, study, current_smoking_p, former_smoking_p, study_id, true_sample, study_setting) %>%
  add_count(country) %>%
  group_by(country, study_id)


write_rds(b, here::here('data_clean', 'country_prevalence_data.rds'))