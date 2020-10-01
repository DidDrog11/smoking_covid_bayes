library(tidybayes)
library(brms)
library(here)
source(here("scripts", "bayes_scripts.R"))
source(here("scripts", "author_dictionary.R"))

m1_a <- readRDS(here("data_clean", "bayesian_models", "testing_current_m1_a.rds"))
post_samples_m1_a <- post_samples(m1_a)
median_hdci(post_samples_m1_a$tau)

m2_a <- readRDS(here("data_clean", "bayesian_models", "testing_former_m2_a.rds"))
post_samples_m2_a <- post_samples(m2_a)
median_hdci(post_samples_m2_a$tau)

m3_a <- readRDS(here("data_clean", "bayesian_models", "hospitalisation_current_m3_a.rds"))
post_samples_m3_a <- post_samples(m3_a)
median_hdci(post_samples_m3_a$tau)

m4_a <- readRDS(here("data_clean", "bayesian_models", "hospitalisation_former_m4_a.rds"))
post_samples_m4_a <- post_samples(m4_a)
median_hdci(post_samples_m4_a$tau)

m5_a <- readRDS(here("data_clean", "bayesian_models", "severity_current_m5_a.rds"))
post_samples_m5_a <- post_samples(m5_a)
median_hdci(post_samples_m5_a$tau)

m6_a <- readRDS(here("data_clean", "bayesian_models", "severity_former_m6_a.rds"))
post_samples_m6_a <- post_samples(m6_a)
median_hdci(post_samples_m6_a$tau)

m7_a <- readRDS(here("data_clean", "bayesian_models", "mortality_current_m7_a.rds"))
post_samples_m7_a <- post_samples(m7_a)
median_hdci(post_samples_m7_a$tau)

m8_a <- readRDS(here("data_clean", "bayesian_models", "mortality_former_m8_a.rds"))
post_samples_m8_a <- post_samples(m8_a)
median_hdci(post_samples_m8_a$tau)