library(tidyverse)
library(terra)

scratch <- here::here("G:/", "scratch")
f_masks_locs <- here::here("G:/", "Merged", "forest_masks")
vlce_locs <- here::here("G:/", "Merged", "vlce")

vlces <- list.files(vlce_locs)

vlces <- here::here(vlce_locs, vlces)

rcl <- c(20, 0,
         31, 0,
         32, 0,
         33, 0,
         40, 0,
         50, 0,
         80, 0,
         81, 1,
         100, 0,
         210, 1,
         220, 1,
         230, 1)

rclmat <- matrix(rcl, ncol = 2, byrow = T)

forest_masks <- function(vlce) {
  year <- str_extract(vlce, "(\\d)+")
  save_name <- paste0("forest_mask-", year, ".tif")
  save_loc <- here::here(f_masks_locs, save_name)
  print(save_loc)
  
  vlce_rast <- rast(vlce)
  
  f_mask <- classify(vlce_rast, rclmat)
  
  terra::writeRaster(f_mask, save_loc, overwrite = T)
  
  print(paste(year, "done"))
}

forest_masks(vlces[32])

map(vlces, forest_masks)

plot(rast(save_loc))
