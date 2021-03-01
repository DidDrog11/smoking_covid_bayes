source(here::here("scripts", "libraries.R"))

tau <- read_rds(here("output_data", "tau.rds"))
current_testing_alternative_prior <- read_rds(here("output_data", "current_testing_alternative.rds"))
former_testing_alternative_prior <- read_rds(here("output_data", "former_testing_alternative.rds"))
current_hospitalisation_alternative_prior <- read_rds(here("output_data", "current_hospitalisation_alternative.rds"))
former_hospitalisation_alternative_prior <- read_rds(here("output_data", "former_hospitalisation_alternative.rds"))
current_severity_alternative_prior <- read_rds(here("output_data", "current_severity_alternative.rds"))
former_severity_alternative_prior <- read_rds(here("output_data", "former_severity_alternative.rds"))
current_mortality_alternative_prior <- read_rds(here("output_data", "current_mortality_alternative.rds"))
former_mortality_alternative_prior <- read_rds(here("output_data", "former_mortality_alternative.rds"))

# Results summaries
# Current
current_testing_RR_value <- current_testing_alternative_prior$summary %>%
  ungroup() %>%
  filter(Author == "Pooled Effect") %>%
  select(b_Intercept)
current_testing_lci <- current_testing_alternative_prior$summary %>%
  ungroup() %>%
  filter(Author == "Pooled Effect") %>%
  select(.lower)
current_testing_uci <- current_testing_alternative_prior$summary %>%
  ungroup() %>%
  filter(Author == "Pooled Effect") %>%
  select(.upper)
current_testing_tau <- tau[1,1]
former_testing_RR_value <- former_testing_alternative_prior$summary %>%
  ungroup() %>%
  filter(Author == "Pooled Effect") %>%
  select(b_Intercept)
former_testing_lci <- former_testing_alternative_prior$summary %>%
  ungroup() %>%
  filter(Author == "Pooled Effect") %>%
  select(.lower)
former_testing_uci <- former_testing_alternative_prior$summary %>%
  ungroup() %>%
  filter(Author == "Pooled Effect") %>%
  select(.upper)
former_testing_tau <- tau[2,1]
#Hospitalisation
current_hospitalisation_RR_value <- current_hospitalisation_alternative_prior$summary %>%
  ungroup() %>%
  filter(Author == "Pooled Effect") %>%
  select(b_Intercept)
current_hospitalisation_lci <- current_hospitalisation_alternative_prior$summary %>%
  ungroup() %>%
  filter(Author == "Pooled Effect") %>%
  select(.lower)
current_hospitalisation_uci <- current_hospitalisation_alternative_prior$summary %>%
  ungroup() %>%
  filter(Author == "Pooled Effect") %>%
  select(.upper)
current_hospitalisation_tau <- tau[3,1]
former_hospitalisation_RR_value <- former_hospitalisation_alternative_prior$summary %>%
  ungroup() %>%
  filter(Author == "Pooled Effect") %>%
  select(b_Intercept)
former_hospitalisation_lci <- former_hospitalisation_alternative_prior$summary %>%
  ungroup() %>%
  filter(Author == "Pooled Effect") %>%
  select(.lower)
former_hospitalisation_uci <- former_hospitalisation_alternative_prior$summary %>%
  ungroup() %>%
  filter(Author == "Pooled Effect") %>%
  select(.upper)
former_hospitalisation_tau <- tau[4,1]
#Severity
current_severity_RR_value <- current_severity_alternative_prior$summary %>%
  ungroup() %>%
  filter(Author == "Pooled Effect") %>%
  select(b_Intercept)
current_severity_lci <- current_severity_alternative_prior$summary %>%
  ungroup() %>%
  filter(Author == "Pooled Effect") %>%
  select(.lower)
current_severity_uci <- current_severity_alternative_prior$summary %>%
  ungroup() %>%
  filter(Author == "Pooled Effect") %>%
  select(.upper)
current_severity_tau <- tau[5,1]
former_severity_RR_value <- former_severity_alternative_prior$summary %>%
  ungroup() %>%
  filter(Author == "Pooled Effect") %>%
  select(b_Intercept)
former_severity_lci <- former_severity_alternative_prior$summary %>%
  ungroup() %>%
  filter(Author == "Pooled Effect") %>%
  select(.lower)
former_severity_uci <- former_severity_alternative_prior$summary %>%
  ungroup() %>%
  filter(Author == "Pooled Effect") %>%
  select(.upper)
former_severity_tau <- tau[6,1]
#Mortality
current_mortality_RR_value <- current_mortality_alternative_prior$summary %>%
  ungroup() %>%
  filter(Author == "Pooled Effect") %>%
  select(b_Intercept)
current_mortality_lci <- current_mortality_alternative_prior$summary %>%
  ungroup() %>%
  filter(Author == "Pooled Effect") %>%
  select(.lower)
current_mortality_uci <- current_mortality_alternative_prior$summary %>%
  ungroup() %>%
  filter(Author == "Pooled Effect") %>%
  select(.upper)
current_mortality_tau <- tau[7,1]
former_mortality_RR_value <- former_mortality_alternative_prior$summary %>%
  ungroup() %>%
  filter(Author == "Pooled Effect") %>%
  select(b_Intercept)
former_mortality_lci <- former_mortality_alternative_prior$summary %>%
  ungroup() %>%
  filter(Author == "Pooled Effect") %>%
  select(.lower)
former_mortality_uci <- former_mortality_alternative_prior$summary %>%
  ungroup() %>%
  filter(Author == "Pooled Effect") %>%
  select(.upper)
former_mortality_tau <- tau[8,1]
