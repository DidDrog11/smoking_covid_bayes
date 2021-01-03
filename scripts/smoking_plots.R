## packages
source(here::here("scripts", "libraries.R"))
library(ggsci)
library(matrixStats)
library(gapminder)

smoking_status <- c('current_smoker', 'former_smoker', 'current_former_smoker', 'never_smoker', 'never_smoker_unknown', 'not_stated', 'missing')
exclude_from_analysis <- read_rds(here("output_data", "excluded_studies.rds"))
data_study_general <- read_rds(here("data_clean", "data_study_general.rds")) %>%
  filter(!lead_author %in% exclude_from_analysis)
table_1 <- read_rds(here("data_clean", "table_1.rds")) %>%
  filter(!lead_author %in% exclude_from_analysis)

prevalence_plot <- table_1 %>%
  left_join(., data_study_general %>%
              select(study_id, study_setting), 
            by = "study_id") %>%
  group_by(country, study_setting) %>%
  rename('sample' = total) %>%
  select(study_id, country, sample, current_smoker, former_smoker) %>%
  mutate(country = snakecase::to_any_case(country, case = "title", abbreviations = c("USA", "UK")),
         p_current_smoker = ifelse(!is.na(current_smoker), current_smoker/sample, NA),
         p_former_smoker = ifelse(!is.na(former_smoker), former_smoker/sample, NA),
         study_setting = recode(study_setting, "quarantine_centre" = "community",
                                "homeless_shelters" = "community"),
         study_id  = factor(study_id),
         study_setting = factor(study_setting)) %>%
  ungroup() %>%
  filter(!is.na(p_current_smoker) | !is.na(p_former_smoker)) %>%
  group_by(country) %>%
  mutate(ranked_n = factor(row_number()),
         included_study = T) 

ordered_country <- prevalence_plot %>%
  filter(study_id != is.na(study_id)) %>%
  group_by(country, study_id) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(country) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count)) %>%
  select(country)
ordered_country <- as.vector(t(ordered_country))
prevalence_plot$country <- factor(prevalence_plot$country, levels = ordered_country)

smoking_data <- readxl::read_xlsx(here("data_spreadsheet", "data_extraction_current.xlsx"), sheet = 'national_smoking_prevalence') %>%
  select(-c(Source, "Source 2")) %>%
  mutate(p_current_smoking = Current/100,
         p_former_smoking = Former/100) %>%
  select(-c(Current, Former)) %>%
  rename(country = "Country")

current_smoking <- prevalence_plot %>%
  select(country, study_setting, sample, p_current_smoker, ranked_n) %>%
  drop_na(p_current_smoker)

include_countries <- current_smoking %>%
  group_by(country) %>%
  summarise(n_studies = n()) %>%
  filter(n_studies >= 3 & country != "Multiple")

current_smoking_plot <- current_smoking %>%
  filter(country %in% include_countries$country) %>%
  group_by(country, study_setting) %>%
  mutate(weighted_mean = ifelse(is.na(weightedMean(p_current_smoker, w = sample)), mean(p_current_smoker),
                weightedMean(p_current_smoker, w = sample, na.rm = T)))

current_smoking_data <- smoking_data %>%
  filter(country %in% current_smoking_plot$country) %>%
  mutate(country = factor(country, levels = ordered_country))

## new plots current
ggplot() +
  geom_point(data = current_smoking_plot,
             aes(x = study_setting, y = p_current_smoker, colour = study_setting, size = sample),
             position = position_dodge2(0.3),
             alpha = 0.3)  +
  scale_size_continuous(trans = "log10", guide = F) +
  geom_hline(data = current_smoking_data,
             aes(yintercept = p_current_smoking),
             linetype = "dashed", 
             colour = "#A50026",
             size = 0.8) +
  geom_hline(data = current_smoking_plot ,
             aes(yintercept = weighted_mean, colour = study_setting)) +
  coord_flip() +
  facet_grid(country ~ .,  scales = "free_y", space = "free", switch = 'y') +
  scale_color_brewer(palette = "Dark2", labels = c("Hospital", "Community & Hospital", "Community")) +
  scale_y_continuous(name = "Prevalence", limits = c(0, 0.7), breaks = scales::pretty_breaks(n = 9), expand = c(0, 0)) +
  theme(panel.spacing = unit(0.1, "lines"),
        strip.text = element_text(angle = 0, size = 12),
        axis.title = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 10),
        panel.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        strip.text.y.left = element_text(angle = 0),
        legend.position = "right") +
  labs(title ="Prevalence of current smoking in included studies",
       colour = "Weighted mean prevalence", shape = "Study setting")
ggsave("current_smoking_plots_updated.png", plot = last_plot(), dpi = 360, path = here("reports", "figure"))

## data former
former_smoking <- prevalence_plot %>%
  select(country, study_setting, sample, p_former_smoker, ranked_n) %>%
  drop_na(p_former_smoker)

include_countries <- former_smoking %>%
  group_by(country) %>%
  summarise(n_studies = n()) %>%
  filter(n_studies >= 3 & country != "Multiple")

ordered_country <- former_smoking %>%
  group_by(country) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(country) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count)) %>%
  select(country)
ordered_country <- as.vector(t(ordered_country))
former_smoking$country <- factor(former_smoking$country, levels = ordered_country)

former_smoking_plot <- former_smoking %>%
  filter(country %in% include_countries$country) %>%
  group_by(country, study_setting) %>%
  mutate(weighted_mean = ifelse(is.na(weightedMean(p_former_smoker, w = sample)), mean(p_former_smoker),
                                weightedMean(p_former_smoker, w = sample, na.rm = T)))

former_smoking_data <- smoking_data %>%
  filter(country %in% former_smoking_plot$country) %>%
  mutate(country = factor(country, levels = ordered_country))

## new plots former
ggplot() +
  geom_point(data = former_smoking_plot,
             aes(x = study_setting, y = p_former_smoker, colour = study_setting, size = sample),
             position = position_dodge2(0.3),
             alpha = 0.3)  +
  scale_size_continuous(trans = "log10", guide = F) +
  geom_hline(data = former_smoking_data,
             aes(yintercept = p_former_smoking),
             linetype = "dashed", 
             colour = "#A50026",
             size = 0.8) +
  geom_hline(data = former_smoking_plot ,
             aes(yintercept = weighted_mean, colour = study_setting)) +
  coord_flip() +
  facet_grid(country ~ .,  scales = "free_y", space = "free", switch = 'y') +
  scale_color_brewer(palette = "Dark2", labels = c("Hospital", "Community & Hospital", "Community")) +
  scale_y_continuous(name = "Prevalence", limits = c(0, 0.7), breaks = scales::pretty_breaks(n = 9), expand = c(0, 0)) +
  theme(panel.spacing = unit(0.1, "lines"),
        strip.text = element_text(angle = 0, size = 12),
        axis.title = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 10),
        panel.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        strip.text.y.left = element_text(angle = 0),
        legend.position = "right") +
  labs(title ="Prevalence of former smoking in included studies",
       colour = "Weighted mean prevalence", shape = "Study setting")
ggsave("former_smoking_plots_updated.png", plot = last_plot(), dpi = 360, path = here("reports", "figure"))