source(here("scripts", "libraries.R"))
source(here("scripts", "bayesian_data_summary.R"))

rmarkdown::render(here("reports","manuscript.Rmd"), output_format = "html_document")
rmarkdown::render(here("reports","manuscript.Rmd"), output_format = "word_document")