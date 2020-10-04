## packages
library(tidyverse)
library(ggsci)
library(matrixStats)
library(gapminder)

## set seed
set.seed(123)

## load fonts
extrafont::loadfonts(device = "win")

## get data
data <-  read_rds(here::here('data_clean', 'country_prevalence_data.rds')) %>%
  mutate(country = recode(country, "SouthKorea" = "South Korea"),
         study_setting = recode(study_setting, "quarantine_centre" = "community"),
         study_id  = factor(study_id),
         study_setting = factor(study_setting),
         smoking = factor(smoking)) %>%
  group_by(country, study_setting, smoking, study) %>%
  mutate(mean = ifelse(is.na(weightedMean(prevalence, w = true_sample)), mean(prevalence),
                       weightedMean(prevalence, w = true_sample, na.rm = T)),
         mean_uci = ifelse(is.na(weightedMean(upper_ci, w = true_sample)), mean(upper_ci),
                       weightedMean(upper_ci, w = true_sample, na.rm = T)),
         mean_lci = ifelse(is.na(weightedMean(lower_ci, w = true_sample)), mean(lower_ci),
                       weightedMean(lower_ci, w = true_sample, na.rm = T))) %>%
  ungroup() %>%
  group_by(country, smoking, study) %>%
  mutate(ranked_n = factor(row_number()))

ordered_country <- data %>%
  filter(study_id != is.na(study_id)) %>%
  group_by(country, study_id) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(country) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count)) %>%
  select(country)
ordered_country <- as.vector(t(ordered_country))
data$country <- factor(data$country, levels = ordered_country)

country_colour <- data.frame(gapminder, cc = I(country_colors[match(gapminder$country, names(country_colors))]))
country_colour <- country_colour %>%
  select(country, cc) %>%
  mutate(country = recode(country,
         "United Kingdom" = "UK",
         "United States" = "USA",
         "Korea, Dem. Rep." = "South Korea")) %>%
  rename("country_colour" = cc) %>%
  distinct() %>%
  filter(country %in% data$country) %>%
  deframe()

current_smoking_plot <- data %>%
  filter(smoking == "current_smoking_p") %>%
  drop_na(country)

former_smoking_plot <- data %>%
  filter(smoking == "former_smoking_p") %>%
  drop_na(country) %>%
  filter(!country %in% c("Mexico", "Iran", "Turkey", "India", "Kuwait", "Poland", "Qatar", "Switzerland", "Netherlands"))

## sort data
sorted_data <- data %>%
  ungroup() %>%
  rename("number_studies" = n) %>%
  mutate(country = fct_reorder(country, -number_studies), ## sorting countries by number of studies from each country
         smoking = fct_relevel(smoking, "former_smoking_p", after = Inf),
         study_setting = recode(study_setting, "quarantine_centre" = "community"),
         study_setting = forcats::fct_explicit_na(study_setting)) %>%
  group_by(smoking, country) %>%
  mutate(running_count = row_number()) %>%
  group_by(smoking, country, study, study_setting) %>%
  mutate(mean = ifelse(is.na(weightedMean(prevalence, w = true_sample)), mean(prevalence),
                         weightedMean(prevalence, w = true_sample, na.rm = T))) %>%
  ungroup() %>%
  add_count(smoking, country) %>%
  group_by(study, study_setting)

sorted_data$running_count <- as_factor(sorted_data$running_count)  

current_sorted_data <- sorted_data %>%
  filter(., smoking == "current_smoking_p") %>%
  arrange(-prevalence, study_setting, .by_group = T) %>%
  ungroup() %>%
  group_by(country) %>%
  filter(!n <= 1)

a <- ggplot(data = filter(current_sorted_data, study == 1), aes(x = running_count, y = prevalence)) +
  geom_point(aes(size = true_sample, shape = factor(study_setting))) + 
  scale_size_continuous(trans = "log10", guide = F) +
  scale_shape_manual(values = c(15, 16, 17), labels = c("Community", "Community & Hospital", "Hospital")) +
  geom_linerange(aes(ymin = lower_ci, ymax =upper_ci), linetype = "solid") +
  geom_segment(data = filter(current_sorted_data, study == 1), aes(x = 0-0.5, 
                                                                   xend = n+0.5,
                                                                   y = mean,
                                                                   yend = mean,
                                                                   colour = study_setting), size = 0.8) +
  scale_color_brewer(palette = "Dark2", labels = c("Community", "Community & Hospital", "Hospital")) +
  geom_segment(data = filter(current_sorted_data, study == 0), aes(x = 0-0.5, 
                                                                   xend = n+0.5,
                                                                   y = prevalence,
                                                                   yend = prevalence), linetype = "solid", 
               colour = "#A50026",
               size = 0.8) +
    coord_flip() +
    facet_grid(country ~  ., scales = "free_y", space = "free", switch = 'y') +
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
a

png(here::here('reports', 'figure', 'figure_2a.png'), width=912, height=970, res = 120)
a
null <- dev.off()


former_sorted_data <- sorted_data %>%
  filter(., smoking == "former_smoking_p") %>%
  filter(., !n <= 1 ) %>%
  arrange(-prevalence, .by_group = T)

a <- ggplot(data = filter(former_sorted_data, study == 1), aes(x = running_count, y = prevalence)) +
  geom_point(aes(size = true_sample, shape = factor(study_setting))) + 
  scale_size_continuous(trans = "log10", guide = F) +
  scale_shape_manual(values = c(15, 16, 17), labels = c("Community", "Community & Hospital", "Hospital")) +
  geom_linerange(aes(ymin = lower_ci, ymax =upper_ci), linetype = "solid") +
  geom_segment(data = filter(former_sorted_data, study == 1), aes(x = 0, 
                                                                   xend = n + 0.5,
                                                                   y = mean,
                                                                   yend = mean,
                                                                   colour = study_setting), size = 0.8) +
  scale_color_brewer(palette = "Dark2", labels = c("Community", "Community & Hospital", "Hospital")) +
  geom_segment(data = filter(former_sorted_data, study == 0), aes(x = 0, 
                                                                   xend = n + 0.5,
                                                                   y = prevalence,
                                                                   yend = prevalence), linetype = "solid", 
               colour = "#A50026",
               size = 0.8) +
  scale_y_continuous(name = "Prevalence", limits = c(0, 0.9), breaks = scales::pretty_breaks(n = 9), expand = c(0, 0)) +
  coord_flip() +
  facet_grid(country ~  ., scales = "free_y", space = "free", switch = 'y') +
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
a

png(here::here('reports', 'figure', 'figure_2b.png'), width=912, height=967, res = 120)
a
null <- dev.off()

## Limiting plots to countries with more than 3 studies worth of data.
current_country_list <- current_smoking_plot %>%
  group_by(country, study) %>%
  summarise(number_studies = n()) %>%
  filter(study != 0) %>%
  filter(!number_studies < 3) %>%
  select(country)

current_smoking_plot <- current_smoking_plot %>%
  filter(country %in% current_country_list$country)

former_country_list <- former_smoking_plot %>%
  group_by(country, study) %>%
  summarise(number_studies = n()) %>%
  filter(study != 0) %>%
  filter(!number_studies < 3) %>%
  select(country)

former_smoking_plot <- former_smoking_plot %>%
  filter(country %in% former_country_list$country)

## new plots current
png(here("reports", "figure", "current_smoking_plots_updated.png"), width = 833, height = 719)
ggplot() +
  geom_point(data = current_smoking_plot %>%
               filter(study != 0), 
             aes(x = study_setting, y = prevalence, colour = study_setting, size = true_sample),
             position = position_dodge2(0.3),
             alpha = 0.3)  +
  scale_size_continuous(trans = "log10", guide = F) +
  geom_hline(data = current_smoking_plot %>%
               filter(study == 0),
             aes(yintercept = mean),
             linetype = "dashed", 
             colour = "#A50026",
             size = 0.8) +
  geom_hline(data = current_smoking_plot %>%
               filter(study == 1),
             aes(yintercept = mean, colour = study_setting)) +
  coord_flip() +
  facet_grid(country ~ .,  scales = "free_y", space = "free", switch = 'y') +
  scale_color_brewer(palette = "Dark2", labels = c("Community", "Community & Hospital", "Hospital")) +
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
dev.off

## new plots former
png(here("reports", "figure", "former_smoking_plots_updated.png"), width = 833, height = 719)
ggplot() +
  geom_point(data = former_smoking_plot %>%
               filter(study != 0), 
             aes(x = study_setting, y = prevalence, colour = study_setting, size = true_sample),
             position = position_dodge2(0.3),
             alpha = 0.3)  +
  scale_size_continuous(trans = "log10", guide = F) +
  geom_hline(data = former_smoking_plot %>%
               filter(study == 0),
             aes(yintercept = mean),
             linetype = "dashed", 
             colour = "#A50026",
             size = 0.8) +
  geom_hline(data = former_smoking_plot %>%
               filter(study == 1),
             aes(yintercept = mean, colour = study_setting)) +
  coord_flip() +
  facet_grid(country ~ .,  scales = "free_y", space = "free", switch = 'y') +
  scale_color_brewer(palette = "Dark2", labels = c("Community", "Community & Hospital", "Hospital")) +
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
dev.off()