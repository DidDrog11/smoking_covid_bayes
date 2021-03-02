library(tidyverse)

dt <- readxl::read_xlsx("traa172_supplemental_file.xlsx") %>%
  select(
    2, 10, 13, 22, 54,
    56, 67, 68
  ) %>%
  mutate_all(., ~ as_factor(.)) %>%
  rename(
    "hospitalised" = "Facility (home, hospital, institutional isolation)",
    "eversmok" = "Ever smoked (1/0)",
    "cursmok" = "Current smoker (1/0)",
    "smokeless" = "Smokeless tobacco use",
    "anytobacco" = "Any current TOBACCO USE"
  )

summary(dt)
xtabs(~ hospitalised + eversmok + cursmok, data = dt)
xtabs(~ severecovid + eversmok + cursmok, data = dt)
xtabs(~ death + eversmok + cursmok, data = dt)
