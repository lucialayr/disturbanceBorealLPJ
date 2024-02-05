library(tidyverse)
library(terra)
library(sf)

source("code/utils_general.R")


study_domain = st_read("data/final/shp/vegetation_ssp585_d0.04_fpc_30years2100.shp") %>%
  select(geometry) %>%
  st_make_valid() %>%
  st_union() %>%
  terra::vect()

study_grid = read_csv(paste0("data/processed/", create_name_timeslice("picontrol", 0.01, "met", 2100), "_processed.csv"),
                      show_col_types = FALSE) %>%
  select(lon, lat, aet) %>%
  group_by(lon, lat) %>%
  summarise(aet = as.numeric(mean(aet))) %>%
  terra::rast(crs = "EPSG:4326") %>%
  terra::crop(evapotranspiration)

study_domain = as.polygons(study_grid, dissolve = F, aggregate = T)

evapotranspiration = terra::rast("data/ext/evapotranspiration_monthly_AboVE.nc4") %>%
  terra::crop(study_domain) 
