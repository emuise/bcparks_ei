library(tidyverse)
library(terra)

scratch <- here::here("G:/", "scratch")
f_masks_locs <- here::here("G:/", "Merged", "forest_masks")
masked_structure_locs <- here::here("G:/", "Merged", "masked_structure")

struct_vars <- c("percentage_first_returns_above_2m", "loreys_height", "total_biomass", "elev_cv")
years <- 2015

mask_structure <- function(variable, year) {
  print(paste(variable, year))
  save_loc <- here::here(masked_structure_locs, variable, paste0("masked-", variable, "-", year, ".tif"))
  
  structure_loc <- here::here("G:/", "Merged", variable, paste0("BC-",  variable, "-", year, ".tif"))
  mask_loc <- here::here(f_masks_locs, paste0("forest_mask-", year, ".tif"))
  
  structure <- rast(structure_loc)
  mask <- rast(mask_loc)
  
  masked_structure <- terra::mask(structure, mask, maskvalues = 0)
  
  terra::writeRaster(masked_structure, save_loc, overwrite = T, NAflag = 0)
}

map(struct_vars, mask_structure, year = 2015)
