source(here("scripts", "libraries.R"))

ovid_dec <- read_csv(here("data_spreadsheet", "2020-12-17_ovid.csv")) %>%
  select(DO) %>%
  drop_na()

ovid_feb <- read_bibliography(here("data_spreadsheet", "ovid_2021-02-16.ris"))

ovid_jul <- read_bibliography(here("data_spreadsheet", "ovid_2021-07-18.ris"))

ovid_new <- ovid_jul %>%
  filter(!doi %in% ovid_feb$doi) %>%
  write_bibliography(., file = here("data_spreadsheet", "deduplicated_ovid.bib"))

ovid_include <- read_bibliography(here("data_spreadsheet", "include_v12_ovid.bib")) %>%
  mutate(doi = paste0("https://doi.org/", doi))