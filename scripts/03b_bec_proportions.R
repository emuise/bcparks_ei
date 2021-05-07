library(tidyverse)
library(sf)
library(scico)
options(scipen = 9999)

ppa_bec <- read_sf(here::here("data", "shapefiles", "bc_ppa_bec_hres.shp"))

ppa_bec <- ppa_bec %>% 
  select(NAME_E, zone, subzone, BIOME, PROTDAT, OWNER_E, geometry) %>%
  mutate(area = st_area(ppa_bec),
         area_sqm = as.numeric(area))

ppa_bec_df <- ppa_bec %>%
  st_drop_geometry() %>%
  janitor::clean_names() %>%
  filter(!is.na(zone)) %>% #remove sliver polygons from analysis
  mutate(protected = ifelse(is.na(name_e), "Unprotected", "Protected"))

per_zones <- ppa_bec_df %>% 
  group_by(protected, zone) %>%
  summarize(area = sum(area_sqm),
            count = n()) %>%
  mutate(per_area = area / sum(area))

per_zones_wide <- per_zones %>%
  pivot_wider(names_from = protected, values_from = c(count, per_area, area))

per_subzones <- ppa_bec_df %>% 
  group_by(protected, zone, subzone) %>%
  summarize(area = sum(area_sqm),
            count = n()) %>%
  mutate(per_area = area / sum(area))

per_subzones_wide <- per_subzones %>%
  pivot_wider(names_from = protected, values_from = c(count, per_area, area))

per_zones %>% ggplot(aes(x = zone, y = per_area, fill = protected)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  theme_test() +
  labs(fill = "",
       x = "BEC Zone",
       y = "Proportion of British Columbia")

per_zones %>% ggplot(aes(x = protected, y = per_area, fill = zone)) +
  geom_col(position = "stack") +
  coord_polar(theta = "y") +
  scale_fill_scico_d() +
  theme_void() +
  labs(fill = "")

per_subzones %>%
  #filter(zone == "BWBS") %>% 
  ggplot(aes(x = subzone, y = per_area, fill = protected)) +
  geom_col(position = "dodge") +
  facet_wrap(~ zone, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  #scale_fill_scico_d(palette = "roma") +
  theme_test() +
  labs(fill = "",
       x = "Subzone",
       y = "Proportion of BEC Zone")
