data_study_general <- data_study_general %>%
  filter(review_version %in% analysed_versions) %>%
  filter(!(lead_author %in% exclude_from_analysis)) %>%
  write_rds(., here::here('data_clean', 'data_study_general.rds'))

review_details <- data_study_general %>%
  select(lead_author, date_published, country, review_version, study_id) %>% 
  filter(!(lead_author %in% exclude_from_analysis))

table_1 <- table_1 %>%
  filter(review_version %in% analysed_versions) %>%
  select(-review_version) %>%
  filter(!(lead_author %in% exclude_from_analysis))

table_2 <- table_2 %>%
  filter(review_version %in% analysed_versions) %>%
  select(-review_version) %>%
  filter(!(lead_author %in% exclude_from_analysis))

table_3 <- table_3 %>%
  filter(review_version %in% analysed_versions) %>%
  select(-review_version) %>%
  filter(!(lead_author %in% exclude_from_analysis))

table_4 <- table_4 %>%
  filter(review_version %in% analysed_versions) %>%
  select(-review_version) %>%
  filter(!(lead_author %in% exclude_from_analysis))

table_5 <- table_5 %>%
  filter(review_version %in% analysed_versions) %>%
  select(-review_version) %>%
  filter(!(lead_author %in% exclude_from_analysis))

table_6 <- table_6 %>%
  filter(review_version %in% analysed_versions) %>%
  select(-review_version) %>%
  filter(!lead_author %in% exclude_from_analysis)

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