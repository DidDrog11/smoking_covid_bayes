library(brms)
library(ggplot2)
library(tidybayes)
library(readr)
library(dplyr)
library(ggridges)
library(glue)
library(stringr)
library(forcats)
library(here)
library(meta)
library(snakecase)

source(here("scripts", "bayes_scripts.R"))
source(here("scripts", "author_dictionary.R"))
versions <- read_rds(here("data_clean", "versioning.rds"))

previous_review_versions <- unique(versions[,1])
current_review_version <- unique(versions[,2])

data_study_general <- read_rds(here("data_clean", "data_study_general.rds"))

study_review_version <- data_study_general %>%
  mutate(lead_author = to_upper_camel_case(lead_author, sep_out = ", "),
         lead_author = plyr::mapvalues(lead_author,
                                 from = cleaned_names,
                                 to = correct_names),
         lead_author = recode(lead_author, "de Lusignan" = "de.Lusignan")) %>%
  select(lead_author, review_version) %>%
  rename(Author = lead_author) %>%
  mutate(review_version = factor(fct_collapse(review_version, previous = previous_review_versions, current = current_review_version)))

levels(study_review_version$review_version) <- c(levels(study_review_version$review_version), "combined_pooled") %>%
  fct_relevel(c("previous", "current", "combined_pooled"))

m1 <- read_rds(here("data_clean", "bayesian_models", "testing_current_m1.rds"))
m1_a <- read_rds(here("data_clean", "bayesian_models", "testing_current_m1_a.rds"))
#m1_a_hh <- read_rds(here("data_clean", "bayesian_models", "testing_current_m1_a_hh.rds"))
#m2 <- read_rds(here("data_clean", "bayesian_models", "testing_former_m2.rds"))
m2_a <- read_rds(here("data_clean", "bayesian_models", "testing_former_m2_a.rds"))
#m2_a_hh <- read_rds(here("data_clean", "bayesian_models", "testing_former_m2_a_hh.rds"))
#m3 <- read_rds(here("data_clean", "bayesian_models", "hospitalisation_current_m3.rds"))
m3_a <- read_rds(here("data_clean", "bayesian_models", "hospitalisation_current_m3_a.rds"))
#m3_a_hh <- read_rds(here("data_clean", "bayesian_models", "hospitalisation_current_m3_a_hh.rds"))
#m4 <- read_rds(here("data_clean", "bayesian_models", "hospitalisation_former_m4.rds"))
m4_a <- read_rds(here("data_clean", "bayesian_models", "hospitalisation_former_m4_a.rds"))
#m4_a_hh <- read_rds(here("data_clean", "bayesian_models", "hospitalisation_former_m4_a_hh.rds"))
#m5 <- read_rds(here("data_clean", "bayesian_models", "severity_current_m5.rds"))
m5_a <- read_rds(here("data_clean", "bayesian_models", "severity_current_m5_a.rds"))
#m5_a_hh <- read_rds(here("data_clean", "bayesian_models", "severity_current_m5_a_hh.rds"))
#m6 <- read_rds(here("data_clean", "bayesian_models", "severity_former_m6.rds"))
m6_a <- read_rds(here("data_clean", "bayesian_models", "severity_former_m6_a.rds"))
#m6_a_hh <- read_rds(here("data_clean", "bayesian_models", "severity_former_m6_a_hh.rds"))
#m7 <- read_rds(here("data_clean", "bayesian_models", "mortality_current_m7.rds"))
m7_a <- read_rds(here("data_clean", "bayesian_models", "mortality_current_m7_a.rds"))
#m7_a_hh <- read_rds(here("data_clean", "bayesian_models", "mortality_current_m7_a_hh.rds"))
#m8 <- read_rds(here("data_clean", "bayesian_models", "mortality_former_m8.rds"))
m8_a <- read_rds(here("data_clean", "bayesian_models", "mortality_former_m8_a.rds"))
#m8_a_hh <- read_rds(here("data_clean", "bayesian_models", "mortality_former_m8_a_hh.rds"))

# Current smokers testing -------------------------------------------------

# Bayesian analysis for current smokers and testing for SARS-CoV-2
#post_samples_m1 <- post_samples(m1)
post_samples_m1_a <- post_samples(m1_a)
#post_samples_m1_a_hh <- post_samples(m1_a_hh)

#m1_ecdf <- ecdf(exp(post_samples_m1$TE))
m1_a_ecdf <- ecdf(exp(post_samples_m1_a$TE))
#m1_a_hh_ecdf <- ecdf(exp(post_samples_m1_a_hh$TE))

#m1_ecdf(0.9) #minimally informative
m1_a_ecdf(0.9) #informative
#m1_a_hh_ecdf(0.9) #high heterogeneity

# study_draws_m1 <- study_draw(m1) %>%
#  mutate(Author = ifelse(Author == "Merkely", "Merkely*",
#                         ifelse(Author == "Carrat", "Carrat*",
#                                ifelse(Author == "Richard", "Richard*", Author))))
# pooled_draw_m1 <- pooled_effect_draw(m1)

study_draws_m1_a <- study_draw(m1_a) %>%
  mutate(Author = ifelse(Author == "Merkely", "Merkely*",
                         ifelse(Author == "Carrat", "Carrat*",
                                ifelse(Author == "Richard", "Richard*", Author))))
pooled_draw_m1_a <- pooled_effect_draw(m1_a)

# study_draws_m1_a_hh <- study_draw(m1_a_hh) %>%
#   mutate(Author = ifelse(Author == "Merkely", "Merkely*",
#                          ifelse(Author == "Carrat", "Carrat*", Author)))
# pooled_draw_m1_a_hh <- pooled_effect_draw(m1_a_hh)

# current_testing_minimal_prior <- forest_plot(
#  m1,
#  study_draws_m1,
#  pooled_draw_m1,
#  cut = 4,
#  "Forest plot of current smokers and risk of testing positive",
#  "Minimally informative prior",
#  "m1.png"
# )

current_testing_alternative_prior <- forest_plot(
  m1_a,
  study_draws_m1_a,
  pooled_draw_m1_a,
  cut = 4,
  "Forest plot of current smokers and risk of testing positive",
  paste(tail(previous_review_versions, 1), "testing prior"),
  "m1_a.png"
)

png(file = (here("reports", "figure","ECDF_m1_a.png")))
plot(m1_a_ecdf, main = "Empirical cumulative distribution, current smokers, testing",
     xlab = "Relative Risk")
abline(v = 0.9, col = "red")
dev.off()

# current_testing_alternative_prior_hh <- forest_plot(
#   m1_a_hh,
#   study_draws_m1_a_hh,
#   pooled_draw_m1_a_hh,
#   cut = 4,
#   "Forest plot of current smokers and risk of testing positive",
#   "high heterogeneity prior",
#   "m1_a_hh.png"
# )

median_hdci(post_samples_m1_a$tau)

# Former smokers testing --------------------------------------------------
# Bayesian analysis for former smokers and testing for SARS-CoV-2


#post_samples_m2 <- post_samples(m2)
post_samples_m2_a <- post_samples(m2_a)
#post_samples_m2_a_hh <- post_samples(m2_a_hh)

#m2_ecdf <- ecdf(exp(post_samples_m2$TE))
m2_a_ecdf <- ecdf(exp(post_samples_m2_a$TE))
#m2_a_hh_ecdf <- ecdf(exp(post_samples_m2_a_hh$TE))

#1-m2_ecdf(1.1) #minimally informative
1-m2_a_ecdf(1.1) #informative
#1-m2_a_hh_ecdf(1.1) #high heterogeneity

# study_draws_m2 <- study_draw(m2) %>%
#   mutate(Author = ifelse(Author == "Merkely", "Merkely*",
#                          ifelse(Author == "Carrat", "Carrat*", Author)))
# pooled_draw_m2 <- pooled_effect_draw(m2)

study_draws_m2_a <- study_draw(m2_a) %>%
  mutate(Author = ifelse(Author == "Merkely", "Merkely*",
                         ifelse(Author == "Carrat", "Carrat*",
                                ifelse(Author == "Richard", "Richard*", Author))))
pooled_draw_m2_a <- pooled_effect_draw(m2_a)

# study_draws_m2_a_hh <- study_draw(m2_a_hh) %>%
#   mutate(Author = ifelse(Author == "Merkely", "Merkely*",
#                          ifelse(Author == "Carrat", "Carrat*", Author)))
# pooled_draw_m2_a_hh <- pooled_effect_draw(m2_a_hh)

# former_testing_minimal_prior <- forest_plot(
#   m2,
#   study_draws_m2,
#   pooled_draw_m2,
#   cut = 3,
#   "Forest plot of former smokers and risk of testing positive",
#   "Minimally informative prior",
#   "m2.png"
# )

former_testing_alternative_prior <- forest_plot(
  m2_a,
  study_draws_m2_a,
  pooled_draw_m2_a,
  cut = 3,
  "Forest plot of former smokers and risk of testing positive",
  paste(tail(previous_review_versions, 1), "testing prior"),
  "m2_a.png"
)

png(file = (here("reports", "figure","ECDF_m2_a.png")))
plot(m2_a_ecdf, main = "Empirical cumulative distribution, former smokers, testing",
     xlab = "Relative Risk")
abline(v = 1.1, col = "red")
dev.off()

# former_testing_alternative_prior_hh <- forest_plot(
#   m2_a_hh,
#   study_draws_m2_a_hh,
#   pooled_draw_m2_a_hh,
#   cut = 3,
#   "Forest plot of former smokers and risk of testing positive",
#   "high heterogeneity prior",
#   "m2_a_hh.png"
# )

median_hdci(post_samples_m2_a$tau)

# Current smokers hospitalisation --------------------------------------------------
# Bayesian analysis for current smokers and hospitalisation for SARS-CoV-2

#post_samples_m3 <- post_samples(m3)
post_samples_m3_a <- post_samples(m3_a)
#post_samples_m3_a_hh <- post_samples(m3_a_hh)

#m3_ecdf <- ecdf(exp(post_samples_m3$TE))
m3_a_ecdf <- ecdf(exp(post_samples_m3_a$TE))
#m3_a_hh_ecdf <- ecdf(exp(post_samples_m3_a_hh$TE))

#1-m3_ecdf(1.1) #minimally informative
1-m3_a_ecdf(1.1) #informative
#1-m3_a_hh_ecdf(1.1) #high heterogeneity


# study_draws_m3 <- study_draw(m3) %>%
# mutate(review_version = as.character(review_version),
#        review_version = ifelse(is.na(review_version), "current", review_version),
#        review_version = as_factor(review_version))
# pooled_draw_m3 <- pooled_effect_draw(m3)

study_draws_m3_a <- study_draw(m3_a)  %>%
  mutate(review_version = as.character(review_version),
         review_version = ifelse(is.na(review_version), "current", review_version),
         review_version = as_factor(review_version))
pooled_draw_m3_a <- pooled_effect_draw(m3_a)

# study_draws_m3_a_hh <- study_draw(m3_a_hh)  %>%
#   mutate(review_version = as.character(review_version),
#          review_version = ifelse(is.na(review_version), "current", review_version),
#          review_version = as_factor(review_version))
# pooled_draw_m3_a_hh <- pooled_effect_draw(m3_a_hh)

# current_hospitalisation_minimal_prior <- forest_plot(
#   m3,
#   study_draws_m3,
#   pooled_draw_m3,
#   cut = 3,
#   "Forest plot of current smokers and risk of hospital admission",
#   "Minimally informative prior",
#   "m3.png"
# )

current_hospitalisation_alternative_prior <- forest_plot(
  m3_a,
  study_draws_m3_a,
  pooled_draw_m3_a,
  cut = 3,
  "Forest plot of current smokers and risk of hospital admission",
  paste(tail(previous_review_versions, 1), "hospitalisation prior"),
  "m3_a.png"
)

png(file = (here("reports", "figure","ECDF_m3_a.png")))
plot(m3_a_ecdf, main = "Empirical cumulative distribution, current smokers, hospitalisation",
     xlab = "Relative Risk")
abline(v = 1.1, col = "red")
dev.off()

# current_hospitalisation_alternative_prior_hh <- forest_plot(
#   m3_a_hh,
#   study_draws_m3_a_hh,
#   pooled_draw_m3_a_hh,
#   cut = 3,
#   "Forest plot of current smokers and risk of hospital admission",
#   "high heterogeneity prior",
#   "m3_a_hh.png"
# )

median_hdci(post_samples_m3_a$tau)

# Former smokers hospitalisation --------------------------------------------------
# Bayesian analysis for former smokers and hospitalisation for SARS-CoV-2

#post_samples_m4 <- post_samples(m4)
post_samples_m4_a <- post_samples(m4_a)
#post_samples_m4_a_hh <- post_samples(m4_a_hh)

#m4_ecdf <- ecdf(exp(post_samples_m4$TE))
m4_a_ecdf <- ecdf(exp(post_samples_m4_a$TE))
#m4_a_hh_ecdf <- ecdf(exp(post_samples_m4_a_hh$TE))

#1-m4_ecdf(1.1) #minimally informative
1-m4_a_ecdf(1.1) #informative
#1-m4_a_hh_ecdf(1.1) #high heterogeneity

# study_draws_m4 <- study_draw(m4)  %>%
#   mutate(review_version = as.character(review_version),
#          review_version = ifelse(is.na(review_version), "current", review_version),
#          review_version = as_factor(review_version))
# pooled_draw_m4 <- pooled_effect_draw(m4)

study_draws_m4_a <- study_draw(m4_a)  %>%
  mutate(review_version = as.character(review_version),
         review_version = ifelse(is.na(review_version), "current", review_version),
         review_version = as_factor(review_version))
pooled_draw_m4_a <- pooled_effect_draw(m4_a)

# study_draws_m4_a_hh <- study_draw(m4_a_hh)  %>%
#   mutate(review_version = as.character(review_version),
#          review_version = ifelse(is.na(review_version), "current", review_version),
#          review_version = as_factor(review_version))
# pooled_draw_m4_a_hh <- pooled_effect_draw(m4_a_hh)

# former_hospitalisation_minimal_prior <- forest_plot(
#   m4,
#   study_draws_m4,
#   pooled_draw_m4,
#   cut = 3,
#   "Forest plot of former smokers and risk of hospital admission",
#   "Minimally informative prior",
#   "m4.png"
# )

former_hospitalisation_alternative_prior <- forest_plot(
  m4_a,
  study_draws_m4_a,
  pooled_draw_m4_a,
  cut = 3,
  "Forest plot of former smokers and risk of hospital admission",
  paste(tail(previous_review_versions, 1), "hospitalisation prior"),
  "m4_a.png"
)

png(file = (here("reports", "figure","ECDF_m4_a.png")))
plot(m4_a_ecdf, main = "Empirical cumulative distribution, former smokers, hospitalisation",
     xlab = "Relative Risk")
abline(v = 1.1, col = "red")
dev.off()


# former_hospitalisation_alternative_prior_hh <- forest_plot(
#   m4_a_hh,
#   study_draws_m4_a_hh,
#   pooled_draw_m4_a_hh,
#   cut = 3,
#   "Forest plot of former smokers and risk of hospital admission",
#   "high heterogeneity prior",
#   "m4_a_hh.png"
# )

median_hdci(post_samples_m4_a$tau)

# Current smokers disease severity --------------------------------------------------
# Bayesian analysis for current smokers and severity for SARS-CoV-2

#post_samples_m5 <- post_samples(m5)
post_samples_m5_a <- post_samples(m5_a)
#post_samples_m5_a_hh <- post_samples(m5_a_hh)

#m5_ecdf <- ecdf(exp(post_samples_m5$TE))
m5_a_ecdf <- ecdf(exp(post_samples_m5_a$TE))
#m5_a_hh_ecdf <- ecdf(exp(post_samples_m5_a_hh$TE))

#1-m5_ecdf(1.1) #minimally informative
1-m5_a_ecdf(1.1) #informative
#1-m5_a_hh_ecdf(1.1) #high heterogeneity

# study_draws_m5 <- study_draw(m5)
# study_draws_m5$review_version[study_draws_m5$Author == "Guan"] <- "previous"
# pooled_draw_m5 <- pooled_effect_draw(m5)

study_draws_m5_a <- study_draw(m5_a)
study_draws_m5_a$review_version[study_draws_m5_a$Author == "Guan"] <- "previous"
pooled_draw_m5_a <- pooled_effect_draw(m5_a)

# study_draws_m5_a_hh <- study_draw(m5_a_hh)
# study_draws_m5_a_hh$review_version[study_draws_m5_a_hh$Author == "Guan"] <- "previous"
# pooled_draw_m5_a_hh <- pooled_effect_draw(m5_a_hh)
 
# current_severity_minimal_prior <- forest_plot(
#   m5,
#   study_draws_m5,
#   pooled_draw_m5,
#   cut = 3,
#   "Forest plot of current smokers and risk of severe disease",
#   "Minimally informative prior",
#   "m5.png"
# )

current_severity_alternative_prior <- forest_plot(
  m5_a,
  study_draws_m5_a,
  pooled_draw_m5_a,
  cut = 3,
  "Forest plot of current smokers and risk of severe disease",
  paste(tail(previous_review_versions, 1), "severity prior"),
  "m5_a.png"
)

png(file = (here("reports", "figure","ECDF_m5_a.png")))
plot(m5_a_ecdf, main = "Empirical cumulative distribution, current smokers, disease severity",
     xlab = "Relative Risk")
abline(v = 1.1, col = "red")
dev.off()

# current_severity_alternative_prior_hh <- forest_plot(
#   m5_a_hh,
#   study_draws_m5_a_hh,
#   pooled_draw_m5_a_hh,
#   cut = 3,
#   "Forest plot of current smokers and risk of severe disease",
#   "high heterogeneity prior",
#   "m5_a_hh.png"
# )

median_hdci(post_samples_m5_a$tau)

# Former smokers disease severity --------------------------------------------------
# Bayesian analysis for current smokers and severity for SARS-CoV-2

#post_samples_m6 <- post_samples(m6)
post_samples_m6_a <- post_samples(m6_a)
#post_samples_m6_a_hh <- post_samples(m6_a_hh)

#m6_ecdf <- ecdf(exp(post_samples_m6$TE))
m6_a_ecdf <- ecdf(exp(post_samples_m6_a$TE))
#m6_a_hh_ecdf <- ecdf(exp(post_samples_m6_a_hh$TE))

#1-m6_ecdf(1.1) #minimally informative
1-m6_a_ecdf(1.1) #informative
#1-m6_a_hh_ecdf(1.1) #high heterogeneity

# study_draws_m6 <- study_draw(m6)
# study_draws_m6$review_version[study_draws_m6$Author == "Guan"] <- "previous"
# pooled_draw_m6 <- pooled_effect_draw(m6)

study_draws_m6_a <- study_draw(m6_a)
study_draws_m6_a$review_version[study_draws_m6_a$Author == "Guan"] <- "previous"
pooled_draw_m6_a <- pooled_effect_draw(m6_a)

# study_draws_m6_a_hh <- study_draw(m6_a_hh)
# study_draws_m6_a_hh$review_version[study_draws_m6_a_hh$Author == "Guan"] <- "previous"
# pooled_draw_m6_a_hh <- pooled_effect_draw(m6_a_hh)

# former_severity_minimal_prior <- forest_plot(
#   m6,
#   study_draws_m6,
#   pooled_draw_m6,
#   cut = 4,
#   "Forest plot of former smokers and risk of severe disease, former smokers, disease severity",
#   "Minimally informative prior",
#   "m6.png"
# )

former_severity_alternative_prior <- forest_plot(
  m6_a,
  study_draws_m6_a,
  pooled_draw_m6_a,
  cut = 4,
  "Forest plot of former smokers and risk of severe disease",
  paste(tail(previous_review_versions, 1), "severity prior"),
  "m6_a.png"
)

png(file = (here("reports", "figure","ECDF_m6_a.png")))
plot(m6_a_ecdf, main = "Empirical cumulative distribution",
     xlab = "Relative Risk")
abline(v = 1.1, col = "red")
dev.off()

# former_severity_alternative_prior <- forest_plot(
#   m6_a_hh,
#   study_draws_m6_a_hh,
#   pooled_draw_m6_a_hh,
#   cut = 4,
#   "Forest plot of former smokers and risk of severe disease",
#   "high heterogeneity prior",
#   "m6_a_hh.png"
# )

median_hdci(post_samples_m6_a$tau)
# Current smokers mortality --------------------------------------------------
# Bayesian analysis for current smokers and mortality from SARS-CoV-2

#post_samples_m7 <- post_samples(m7)
post_samples_m7_a <- post_samples(m7_a)
#post_samples_m7_a_hh <- post_samples(m7_a_hh)

#m7_ecdf <- ecdf(exp(post_samples_m7$TE))
m7_a_ecdf <- ecdf(exp(post_samples_m7_a$TE))
#m7_a_hh_ecdf <- ecdf(exp(post_samples_m7_a_hh$TE))

#1-m7_ecdf(1.1) #minimally informative
1-m7_a_ecdf(1.1) #informative
#1-m7_a_hh_ecdf(1.1) #high heterogeneity


#study_draws_m7 <- study_draw(m7)
#pooled_draw_m7 <- pooled_effect_draw(m7)

study_draws_m7_a <- study_draw(m7_a)
pooled_draw_m7_a <- pooled_effect_draw(m7_a)

# study_draws_m7_a_hh <- study_draw(m7_a_hh)
# pooled_draw_m7_a_hh <- pooled_effect_draw(m7_a_hh)

###Temp for Rubio
# study_draws_m7 <- study_draws_m7 %>%
#   mutate(review_version = as.character(review_version),
#          review_version = ifelse(is.na(review_version), "current", review_version),
#          review_version = as_factor(review_version))
study_draws_m7_a <- study_draws_m7_a %>%
  mutate(review_version = as.character(review_version),
         review_version = ifelse(is.na(review_version), "current", review_version),
         review_version = as_factor(review_version))
# study_draws_m7_a_hh <- study_draws_m7_a_hh %>%
#   mutate(review_version = as.character(review_version),
#          review_version = ifelse(is.na(review_version), "current", review_version),
#          review_version = as_factor(review_version))

# current_mortality_minimal_prior <- forest_plot(
#   m7,
#   study_draws_m7,
#   pooled_draw_m7,
#   cut = 5,
#   "Forest plot of current smokers and mortality",
#   "Minimally informative prior",
#   "m7.png"
# )

current_mortality_alternative_prior <- forest_plot(
  m7_a,
  study_draws_m7_a,
  pooled_draw_m7_a,
  cut = 5,
  "Forest plot of current smokers and mortality",
  paste(tail(previous_review_versions, 1), "mortality prior"),
  "m7_a.png"
)

png(file = (here("reports", "figure","ECDF_m7_a.png")))
plot(m7_a_ecdf, main = "Empirical cumulative distribution, current smokers, mortality",
     xlab = "Relative Risk")
abline(v = 1.1, col = "red")
dev.off()
# 
# current_mortality_alternative_prior_hh <- forest_plot(
#   m7_a_hh,
#   study_draws_m7_a_hh,
#   pooled_draw_m7_a_hh,
#   cut = 5,
#   "Forest plot of current smokers and mortality",
#   "high heterogeneity prior",
#   "m7_a_hh.png"
# )

median_hdci(post_samples_m7_a$tau)
# Former smokers mortality --------------------------------------------------
# Bayesian analysis for former smokers and mortality from SARS-CoV-2

# post_samples_m8 <- post_samples(m8)
post_samples_m8_a <- post_samples(m8_a)
# post_samples_m8_a_hh <- post_samples(m8_a_hh)

# m8_ecdf <- ecdf(exp(post_samples_m8$TE))
m8_a_ecdf <- ecdf(exp(post_samples_m8_a$TE))
# m8_a_hh_ecdf <- ecdf(exp(post_samples_m8_a_hh$TE))

# 1-m8_ecdf(1.1) #minimally informative
1-m8_a_ecdf(1.1) #informative
# 1-m8_a_hh_ecdf(1.1) #high heterogeneity


# study_draws_m8 <- study_draw(m8)
# pooled_draw_m8 <- pooled_effect_draw(m8)

study_draws_m8_a <- study_draw(m8_a)
pooled_draw_m8_a <- pooled_effect_draw(m8_a)

# study_draws_m8_a_hh <- study_draw(m8_a_hh)
# pooled_draw_m8_a_hh <- pooled_effect_draw(m8_a_hh)

##Temp for Rubio
# study_draws_m8 <- study_draws_m8 %>%
#   mutate(review_version = as.character(review_version),
#          review_version = ifelse(is.na(review_version), "current", review_version),
#          review_version = as_factor(review_version))
study_draws_m8_a <- study_draws_m8_a %>%
  mutate(review_version = as.character(review_version),
         review_version = ifelse(is.na(review_version), "current", review_version),
         review_version = as_factor(review_version))
# study_draws_m8_a_hh <- study_draws_m8_a_hh %>%
#   mutate(review_version = as.character(review_version),
#          review_version = ifelse(is.na(review_version), "current", review_version),
#          review_version = as_factor(review_version))

# former_mortality_minimal_prior <- forest_plot(
#   m8,
#   study_draws_m8,
#   pooled_draw_m8,
#   cut = 5,
#   "Forest plot of former smokers and mortality",
#   "Minimally informative prior",
#   "m8.png"
# )

former_mortality_alternative_prior <- forest_plot(
  m8_a,
  study_draws_m8_a,
  pooled_draw_m8_a,
  cut = 5,
  "Forest plot of former smokers and mortality",
  paste(tail(previous_review_versions, 1), "mortality prior"),
  "m8_a.png"
)

png(file = (here("reports", "figure","ECDF_m8_a.png")))
plot(m8_a_ecdf, main = "Empirical cumulative distribution, former smokers, mortality",
     xlab = "Relative Risk")
abline(v = 1.1, col = "red")
dev.off()

# former_mortality_alternative_prior_a_hh <- forest_plot(
#   m8_a_hh,
#   study_draws_m8_a_hh,
#   pooled_draw_m8_a_hh,
#   cut = 5,
#   "Forest plot of former smokers and mortality",
#   "high heterogeneity prior",
#   "m8_a_hh.png"
# )

median_hdci(post_samples_m8_a$tau)
# ECDF --------------------------------------------------------------------

ecdf_list <- list(m1_a_ecdf, m2_a_ecdf, m3_a_ecdf, m4_a_ecdf, m5_a_ecdf, m6_a_ecdf, m7_a_ecdf, m8_a_ecdf)
write_rds(ecdf_list, here("data_clean", "bayesian_models", "ecdf_list.RDS"))

#  Tau heterogeneity ------------------------------------------------------
tau <- rbind(
  median_hdci(post_samples_m1_a$tau),
  median_hdci(post_samples_m2_a$tau),
  median_hdci(post_samples_m3_a$tau),
  median_hdci(post_samples_m4_a$tau),
  median_hdci(post_samples_m5_a$tau),
  median_hdci(post_samples_m6_a$tau),
  median_hdci(post_samples_m7_a$tau),
  median_hdci(post_samples_m8_a$tau))

write_rds(tau, here("output_data", "tau.rds"))
# Export summary ----------------------------------------------------------

write_rds(current_testing_alternative_prior, here("output_data", "current_testing_alternative.rds"))
write_rds(former_testing_alternative_prior, here("output_data", "former_testing_alternative.rds"))
write_rds(current_hospitalisation_alternative_prior, here("output_data", "current_hospitalisation_alternative.rds"))
write_rds(former_hospitalisation_alternative_prior, here("output_data", "former_hospitalisation_alternative.rds"))
write_rds(current_severity_alternative_prior, here("output_data", "current_severity_alternative.rds"))
write_rds(former_severity_alternative_prior, here("output_data", "former_severity_alternative.rds"))
write_rds(current_mortality_alternative_prior, here("output_data", "current_mortality_alternative.rds"))
write_rds(former_mortality_alternative_prior, here("output_data", "former_mortality_alternative.rds"))

