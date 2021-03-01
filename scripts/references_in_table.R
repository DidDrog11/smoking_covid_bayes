library(tidyverse)
library(magrittr)
library(flextable)
library(ftExtra)
library(bib2df)

tbl_1 <- readRDS(here::here("data_clean", "table_1_refs.rds")) %>%
  mutate(ref = ifelse(startsWith(ref, "h"), gsub(".*/10", "10", ref), ref))
bib <- bib2df(here::here("reports", "manuscript.bib")) %>%
  select(BIBTEXKEY, DOI)

tbl_1 <- left_join(tbl_1, bib %>%
              rename("ref" = DOI),
            by = "ref") %>%
  mutate(ref = paste("@", BIBTEXKEY, sep = "")) %>%
  select(-BIBTEXKEY)

write_rds(tbl_1, here::here("reports", "tables", "table_1_refs.rds"))
