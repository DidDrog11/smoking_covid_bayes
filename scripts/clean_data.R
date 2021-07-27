search_details <- search_details %>%
  clean_data() %>%
  rename(date_screened = date_screening_in_format_2020_05_22) %>%
  write_rds(here::here('data_clean', 'search_details.rds')) ## The title of the date screened column is to communicate among the review team how we need date formatted

protect_columns_1 <- names(data_study_general) %in% 'doi' ## As the DoI's are links we want to preserve their formatting when running the cleaning script

data_study_general <- data_study_general %>%
  rename(date_published = `date_published_format(2020-05-12)`) %>%
  clean_data(protect = protect_columns_1) %>%
  mutate(study_id = 1:length(lead_author)) %>% ## We create a unique study ID to aid checking of the outputs later
  write_rds(here::here('data_clean', 'data_study_general.rds'))

prisma_previous <- data_study_general

review_details <- data_study_general %>%
  select(lead_author, date_published, country, review_version, study_id) %>% ## The version of the review in which included studies were incorporated is noted
  write_rds(here::here('data_clean', 'review_details.rds'))

table_1 <- table_1 %>%
  clean_data() %>%
  mutate(lower_range = sub('\\_.*', '', .$range),
         upper_range = sub('.*_', '',.$range,)) %>% ## We break apart the age range into an upper and lower level
  select(lead_author, sample_size, female_sex_percent, data_source, median_age, mean_age, iqr_lower, iqr_upper, standard_deviation, lower_range,
         upper_range, current_smoker, former_smoker, current_former_smoker, never_smoker, never_smoker_unknown, current_vaper, former_vaper,
         current_former_vaper, never_vaper, current_smokeless_tobacco, current_smoker_vaper, former_smoker_vaper, never_smoker_vaper,
         current_smoker_former_vaper, former_smoker_former_vaper, never_smoker_former_vaper, not_stated, missing, total) %>%
  mutate(study_id = row_number()) %>%
  filter(!is.na(lead_author)) %>%
  left_join(., review_details %>%
              select(-"lead_author"), by = 'study_id') %>%
  write_rds(here::here('data_clean', 'table_1.rds'))


##For the following tables we're only interested in obtaining data for trials which report on the specific outcome of interest

table_2 <-  table_2 %>%
  clean_data() %>%
  mutate(study_id = row_number()) %>%
  filter(data_on_testing == TRUE) %>%
  left_join(., review_details %>%
              select(-"lead_author"), by = 'study_id') %>%
  write_rds(here::here('data_clean', 'table_2.rds'))

table_3 <-  table_3 %>%
  clean_data() %>%
  mutate(study_id = row_number()) %>%
  filter(data_on_hospitalisation == TRUE) %>%
  left_join(., review_details %>%
              select(-"lead_author"), by = 'study_id') %>%
  write_rds(here::here('data_clean', 'table_3.rds'))

table_4 <-  table_4 %>%
  clean_data() %>%
  mutate(study_id = row_number()) %>%
  filter(data_disease_severity == TRUE) %>%
  left_join(., review_details %>%
              select(-"lead_author"), by = 'study_id') %>%
  write_rds(here::here('data_clean', 'table_4.rds'))

table_5 <- table_5 %>%
  clean_data() %>%
  mutate(study_id = row_number()) %>%
  filter(data_on_deaths == TRUE) %>%
  left_join(., review_details %>%
              select(-"lead_author"), by = 'study_id') %>%
  write_rds(here::here('data_clean', 'table_5.rds'))


protect_columns_2 <- !names(table_6) %in% 'lead_author'

table_6 <-  table_6 %>%
  clean_data(protect = protect_columns_2) %>%
  mutate(study_id = 1:length(lead_author)) %>%
  left_join(., table_1 %>%
              select(not_stated,
                     missing,
                     total,
                     study_id),
            by = 'study_id') %>%
  mutate(missingness = rowSums(.[8:9], na.rm = T),
         missingness_percentage = (missingness/total)*100) %>% ## Later we want to assess the amount of missingness for each study as it would impact the inclusion of a study into the relevant meta-analyses
  select(-c(not_stated, missing, total, missingness)) %>%
  left_join(., review_details %>%
              select(-lead_author), by = 'study_id') %>%
  write_rds(here::here('data_clean', 'table_6.rds'))

a <- data_study_general %>%
  select(lead_author, date_published, source, study_id) %>%
  rename('Lead Author' = lead_author,
         'Date Published' = date_published,
         'Publication Source' = source,
         'Study ID' = study_id)

a$`Lead Author` <- to_upper_camel_case(a$`Lead Author`, sep_out = ", ")
a$`Publication Source` <- to_title_case(a$`Publication Source`)
a$`Publication Source` <- if_else(str_length(a$`Publication Source`) < 5,
                                  toupper(a$`Publication Source`),
                                  to_title_case(a$`Publication Source`))

write_rds(data_study_general, here("data_clean", "data_study_general.rds"))
