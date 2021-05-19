library(tidyverse)
library(ggrepel)
library(scico)

# get all keys from folder into a named list
source(here::here("scripts", "get_keys.R"))

theme_set(theme_bw())

options(scipen = 999)
set.seed(69420)

structure_dir <- here::here("data", "structure")

structure_csvs <- list.files(structure_dir, 
                             pattern = "*2015.csv", 
                             full.names = TRUE)

# structure ----
same_sample <- function(df) {
  
  if (is.character(df)) {
    df <- read_csv(df)
  }

  # filter out those df's with few observations
  if (nrow(df) == 0) {
    return()
  }
  
  # get number of cells to run t test on
  cells <- df %>% group_by(protected) %>% tally() %>% pull(n) %>% min()

  if (cells == 0) {
    return()
  }
  
  # get 20% of cells
  cells = floor(cells * .20)

  # may be worth sampling the same cells for each variable
  # this would involve a different method?
  # running as a function of subzone rather than df/string
  out_df <- df %>% group_by(protected) %>% slice_sample(n = cells)
  
  return(out_df)
}

# load all in, takes some time due to size
structure <- map_df(structure_csvs, same_sample)

t_tests <- structure %>% 
  group_by(subzone, protected, variable) %>%
  summarise(value = list(value)) %>%
  pivot_wider(names_from = protected, values_from = value) %>%
  group_by(subzone, variable) %>%
  rename(protected = `TRUE`, unprotected = `FALSE`) %>%
  filter(length(unlist(protected)) > 1) %>% 
  # there is a bec zone w/ 1 pixel, so this prevents it from breaking the t-test
  mutate(p_value = t.test(unlist(protected), unlist(unprotected))$p.value,
         t_value = t.test(unlist(protected), unlist(unprotected))$statistic,
         n = length(unlist(protected)),
         sig0.05 = ifelse(p_value < 0.05, T, F),
         sig0.01 = ifelse(p_value < 0.01, T, F)) %>%

  select(subzone, variable, n, sig0.05, sig0.01, p_value, t_value) %>%
  separate(subzone, into = c("zone", "subzone"))


t_tests %>% 
  group_by(variable, sig0.05) %>% 
  count()

t_tests %>% 
  group_by(variable, sig0.01) %>% 
  count()

t_tests %>%
  
  group_by(zone, variable, sig0.01) %>%
  summarize(n = n()) %>%
  ungroup() %>% 
  group_by(zone, variable) %>%
  summarize(count = sum(n),
            per_sig = n / sum(n)) %>%
  ggplot(aes(x = per_sig, y = variable, colour = zone, size = count), alpha = 0.5) +
  geom_point() +
  labs(x = "Percent Significant within the Zone",
       y = NULL,
       colour = "Zone",
       size = "Number of Subzones")

  

t_tests %>% 
  left_join(keys$structure) %>%
  group_by(zone, var_long) %>%
  summarize(count = n(),
            per_sig = sum(sig0.01) / n()) %>%
  ggplot(aes(x = per_sig, y = var_long), alpha = 0.5) +
  geom_text_repel(aes(label = zone), max.overlaps = 25) +
  geom_point() +
  labs(x = "Proportion of Significant Subzones",
       y = NULL) +
  xlim(0, 1)

ggsave(here::here("outputs", "struct_sig_proportions.png"), device = "png", height = 5, width = 12.5)

t_tests %>%
  ggplot(aes(x = p_value, y = variable)) + 
  geom_text_repel(aes(label = paste(zone, subzone, sep = "-")), max.overlaps = 314) +
  geom_point() +
  geom_vline(xintercept = 0.01, lty = "dashed") +
  geom_vline(xintercept = 0.05, lty = "dotted") +
  labs(x = "p-value",
       y = NULL,
       colour = NULL) +
  theme(legend.position = "bottom") +
  xlim(0, .05)



egg <- t_tests %>% 
  select(zone, subzone, variable, sig0.01) %>% 
  pivot_wider(names_from = variable, values_from = sig0.01) %>% 
  mutate(loreys_height = ifelse(loreys_height, "l", "-"),
         percentage_first_returns_above_2m  = ifelse(percentage_first_returns_above_2m , "p", "-"),
         total_biomass = ifelse(total_biomass, "b", "-"))

egg %>%
  mutate(key = paste(loreys_height, percentage_first_returns_above_2m, total_biomass)) %>% 
  group_by(key) %>% 
  summarize(n = n())

egg <- t_tests %>% 
  select(zone, subzone, variable, sig0.05) %>% 
  pivot_wider(names_from = variable, values_from = sig0.05) %>% 
  mutate(loreys_height = ifelse(loreys_height, "l", "-"),
         percentage_first_returns_above_2m  = ifelse(percentage_first_returns_above_2m , "p", "-"),
         total_biomass = ifelse(total_biomass, "b", "-"))

egg %>% mutate(key = paste(loreys_height, 
                           percentage_first_returns_above_2m, 
                           total_biomass)) %>% 
  group_by(key) %>% 
  summarize(n = n())

# vlce ----
vlce_dir <- here::here("data", "vlce")

vlce_csvs <- list.files(vlce_dir, pattern = "*2015.csv", full.names = TRUE)

vlce = map_df(vlce_csvs, read_csv, 
              col_types = cols("d", "d", "l", "c", "c", "d"))


# disturbance ----
dist_dir <- here::here("data", "disturbance")

dist_csvs <- list.files(dist_dir, full.names = TRUE)

dist <- map_df(dist_csvs, read_csv, 
               col_types = cols("d", "d", "d", "l", "c", "c"))

overall_dist <- dist %>% 
  group_by(subzone, protected, class_val) %>% 
  summarize(n = sum(n))

overall_dist %>% 
  mutate(fire = ifelse(class_val == 1, T, F)) %>%
  group_by(subzone, protected, fire) %>% 
  summarize(n = sum(n)) %>%
  summarize(per_fire = n / sum(n)) %>%
  select(subzone, variable, n, sig0.05, sig0.01, p_value, t_value)

