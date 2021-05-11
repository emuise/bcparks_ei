library(tidyverse)
library(scico)

options(scipen = 999)
set.seed(69420)

structure_dir <- here::here("data", "structure")

structure_csvs <- list.files(structure_dir, full.names = TRUE)

same_sample <- function(df) {
  print(df)
  
  if (is.character(df)) {
    df = read_csv(df)
  } 
  
  if (nrow(df) == 0) {
    return()
  }
  
  cells <- df %>% group_by(protected) %>% tally() %>% pull(n) %>% min()
  
  if (cells == 0) {
    return()
  }
  
  out_df <- df %>% group_by(protected) %>% slice_sample(n = cells)
  
  return(out_df)
}

df <- map_df(structure_csvs, same_sample)

df %>% 
  group_by(subzone, protected, variable) %>%
  summarise(value = list(value)) %>%
  pivot_wider(names_from = protected, values_from = value) %>%
  group_by(subzone) %>%
  rename(protected = `TRUE`, unprotected = `FALSE`) %>%
  mutate(p_value = t.test(unlist(protected), unlist(unprotected))$p.value,
         t_value = t.test(unlist(protected), unlist(unprotected))$statistic,
         n = length(unlist(protected)),
         sig0.05 = ifelse(p_value < 0.05, T, F)) %>%
  select(subzone, variable, n, sig0.05, p_value, t_value) %>%
  arrange(n) %>%
  view()
