### this script downloads and cleans input data (ppas, becs, and boundaries)
#Evan Muise
#April 7th, 2020

library(tidyverse)
library(rgdal)
library(sf)

##cpcad data

cpcad_url <- "https://cws-scf.ca/CPCAD-BDCAPC_Dec2020.gdb.zip"
cpcad_name <- str_split(cpcad_url, "/")[[1]][4]

cpcad_dest <- here::here("scratch", cpcad_name)
gdb_location <- here::here("scratch", str_sub(cpcad_name, 1, -5))

if (!file.exists(gdb_location)) {
  download.file(cpcad_url, cpcad_dest)
  
  unzip(cpcad_dest, exdir = here::here("scratch"))
}

fc_list <- ogrListLayers(gdb_location)

cpcad_sf <- sf::st_read(dsn = gdb_location,
                        layer = fc_list[1])

#filter to bc and transform to BC albers
cpcad_bc <- cpcad_sf %>%
  filter(LOC_E == "British Columbia") %>%
  st_transform(3005)

#categories used in Bolton et al. (2018)
valid_categories <- c("Ia", "Ib", "II", "IV")

#size filter is based on 100ha
#hence 1e6 square metres
#also removes any biome marine parks

###currently does not filter for AGE!
cpcad_filtered <- cpcad_bc %>%
  mutate(Shape_Area = st_area(cpcad_bc)) %>%
  filter(IUCN_CAT %in% valid_categories &
         as.numeric(Shape_Area) > 1e6 &
         BIOME != "M") %>%
  select(!ends_with("_F"))


st_write(cpcad_filtered, 
         dsn = here::here("data", "shapefiles"), 
         layer = "bc_ppa.shp",
         driver = "ESRI Shapefile")

##data from the bcmaps package
library(bcmaps)

bounds_name <- here::here("data", "shapefiles", "bc_boundary_simplified.shp")

if (!file.exists(bounds_name)) {
  
  bcb <- bc_bound_hres()
  
  st_write(bcb,
           dsn = here::here("data", "shapefiles"), 
           layer = "bc_boundary.shp",
           driver = "ESRI Shapefile")
  
  bcb_simple <- bcb %>%
    st_simplify(dTolerance = 30)
  
  st_write(bcb_simple,
           dsn = here::here("data", "shapefiles"), 
           layer = "bc_boundary_simplified.shp",
           driver = "ESRI Shapefile")

}

bec_name <- here::here("data", "shapefiles", "bec_zones.shp")

if (!file.exists(bec_name)) {
  bec <- bec()
  
  bec <- bec %>% select(!SE_ANNO_CAD_DATA)
  
  bec_colnames <- c("id",
                    "skey",
                    "zone",
                    "subzone",
                    "variant",
                    "phase",
                    "disturbance",
                    "map_label",
                    "bgc_label",
                    "zone_name",
                    "subzone_name",
                    "variant_name",
                    "phase_name",
                    "disturbance_name",
                    "area_sqm",
                    "length_m",
                    "area",
                    "length",
                    "oid",
                    "geometry")
  
  colnames(bec) <- bec_colnames
  
  st_write(bec,
           dsn = here::here("data", "shapefiles"), 
           layer = "bec_zones.shp",
           driver = "ESRI Shapefile")
}
