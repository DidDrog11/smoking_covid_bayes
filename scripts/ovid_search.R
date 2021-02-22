library("tidyverse")
library("here")
library(magrittr)
library(bib2df)
library(revtools)

ovid_dec <- read_csv(here("data_spreadsheet", "2020-12-17_ovid.csv")) %>%
  select(DO) %>%
  drop_na()

read_bibliography(here("data_spreadsheet", "ovid_2021-02-16.ris")) %>%
  filter(!doi %in% ovid_dec$DO) %>%
  write_bibliography(., file = here("data_spreadsheet", "deduplicated_ovid.bib"))
