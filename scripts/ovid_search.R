library("tidyverse")
library("here")

ovid_dec <- read_csv(here("data_spreadsheet", "2020-12-17_ovid.csv")) %>%
  select(DO) %>%
  drop_na()
