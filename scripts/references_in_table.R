library(tidyverse)
library(magrittr)
library(flextable)
library(ftExtra)
library(bib2df)

tbl_1 <- readRDS(here::here("data_clean", "table_1_refs.rds")) %>%
  mutate(ref = ifelse(startsWith(ref, "h"), gsub(".*/10", "10", ref), ref),
         ref = str_remove(ref, "v\\d.full.pdf$"),
         ref = str_remove(ref, "v10.full.pdf"),
         ref = case_when(`Lead author` != "Tehrani" ~ str_remove(ref, "v\\d$"),
                         TRUE ~ ref)) %>%
  distinct(`Lead author`, `Sample size`, .keep_all = T)
bib <- bib2df(here::here("reports", "manuscript.bib")) %>%
  select(BIBTEXKEY, DOI)

missing_doi <- tbl_1 %>%
  filter(!ref %in% bib$DOI) %>%
  mutate(ref = str_remove(ref, "v\\d.full.pdf$"))

#This is the vector to lookup missing doi's in zoter
cat(paste(missing_doi$ref, collapse = ", "))

tbl_1 <- left_join(tbl_1, bib %>%
              rename("ref" = DOI),
            by = "ref") %>%
  mutate(ref = paste("@", BIBTEXKEY, sep = "")) %>%
  select(-BIBTEXKEY)

write_rds(tbl_1, here::here("reports", "tables", "table_1_refs.rds"))
