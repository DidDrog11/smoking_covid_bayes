if (!require("linelist")) devtools::install_github("reconhub/linelist")

if (!require("pacman")) install.packages("pacman")
pkgs =
  c("brms",
    "ggplot2",
    "tidybayes",
    "readr",
    "lubridate",
    "tidyverse",
    "ggridges",
    "glue",
    "stringr",
    "forcats",
    "here",
    "meta",
    "snakecase",
    "flextable",
    "googlesheets4",
    "googledrive",
    "linelist",
    "DT",
    "epitools",
    "ggmap",
    "gtsummary",
    "bib2df",
    "ftExtra",
    "meta",
    "snakecase",
    "revtools",
    "rnaturalearth",
    "sf"
  )
pacman::p_load(pkgs, character.only = T)
