library(tidyverse)
library(furrr)
library(sf)
library(terra)

source("code/utils_general.R")

## 1A

final_1A_configuration = function(climate_scenario, disturbance_regime, variable, year) {
  
  variable = as.character(variable) #arrives as a factor which gives use trouble
  disturbance_regime = as.character(disturbance_regime) #arrives as a factor which gives use trouble
  
  df = read_csv(paste0("data/processed/", create_name_timeslice(climate_scenario, round(as.numeric(disturbance_regime), 3), variable, year), "_processed.csv"),
                show_col_types = FALSE) %>%
    summarize(across(c("bne_fraction", "ibs_fraction", "tebs_fraction", "bns_fraction", "tundra_fraction", "soil_fraction"), list(mean = ~round(mean(.x, na.rm = TRUE), digits = 4)),
                      .names = "{.col}_{.fn}")) %>%
    mutate(s = climate_scenario,
           d = round(as.numeric(disturbance_regime), 3))
  
    return(df)
}

final_1A = function(Climate_Scenarios, Disturbance_Regimes, variable, year, ncore) {
  combinations = expand.grid(climate_scenario = Climate_Scenarios, disturbance_regime = Disturbance_Regimes)
  
  plan(multisession, workers = ncore)
  
  results = future_pmap_dfr(combinations, ~final_1A_configuration(..1, ..2, variable, year))
  
  results %>%
    write_csv(paste0("data/final/final_vegetationA_", variable, ".csv"))
}

final_1A(Climate_Scenarios = c("ssp370" , "ssp585" , "ssp126" , "picontrol"),
         Disturbance_Regimes = c("0.04", "0.003333333", "0.1", "0.01"),
         variable = "fpc",
         year = 2100,
         ncore = 4)

## 1B

final_1B_configuration = function(climate_scenario, disturbance_regime, variable, year) {
  
  variable = as.character(variable) #arrives as a factor which gives use trouble
  disturbance_regime = as.character(disturbance_regime) #arrives as a factor which gives use trouble
  
  df = read_csv(paste0("data/processed/", create_name_timeslice(climate_scenario, round(as.numeric(disturbance_regime), 3), variable, year), "_processed.csv"),
                show_col_types = FALSE) %>%
    group_by(year) %>%
    summarize(across(everything(), list(mean = ~mean(.x, na.rm = TRUE)),
                      .names = "{.col}")) %>%
    mutate(total = total + soil) %>% #add soil to total
    summarize(across(c("bne", "ibs", "tebs", "bns", "tundra", "soil", "total"), list(mean = ~round(mean(.x, na.rm = TRUE), digits = 4),
                                                                    sd = ~round(sd(.x, na.rm = TRUE), digits = 4)),
                      .names = "{.col}_{.fn}")) %>%
    mutate(s = climate_scenario,
           d = round(as.numeric(disturbance_regime), 3)) %>% 
  pivot_longer(cols = -c("d", "s", "total_mean", "total_sd"), names_to = "names", values_to = "values") %>%
  mutate("pft" = sapply(strsplit(names, "_"), `[`, 1),
         "stats" = sapply(strsplit(names, "_"), `[`, 2)) %>%
  select(-names) %>%
  pivot_wider(names_from = "stats", values_from = "values") 
  
return(df)
}

final_1B = function(Climate_Scenarios, Disturbance_Regimes, variable, year, ncore) {
  combinations = expand.grid(climate_scenario = Climate_Scenarios, 
                             disturbance_regime = Disturbance_Regimes)
  
  plan(multisession, workers = ncore)
  
  results = future_pmap_dfr(combinations, ~final_1B_configuration(..1, ..2, variable, year))
  
  results %>%
    write_csv(paste0("data/final/final_vegetationB_", variable, ".csv"))
}

final_1B(Climate_Scenarios = c("ssp370" , "ssp585" , "ssp126" , "picontrol"),
         Disturbance_Regimes = c("0.04", "0.003333333", "0.1", "0.01"),
         variable = "lai",
         year = 2100,
         ncore = 4)

final_1B(Climate_Scenarios = c("ssp370" , "ssp585" , "ssp126" , "picontrol"),
         Disturbance_Regimes = c("0.04", "0.003333333", "0.1", "0.01"),
         variable = "fpc",
         year = 2100,
         ncore = 4)

## 1C

final_1C_configuration = function(climate_scenario, disturbance_regime, variable, year) {
  
  variable = as.character(variable) #arrives as a factor which gives use trouble
  disturbance_regime = as.character(disturbance_regime) #arrives as a factor which gives use trouble
  
  df = read_csv(paste0("data/processed/", create_name_timeslice(climate_scenario, round(as.numeric(disturbance_regime), 3), variable, year), "_processed.csv"),
                show_col_types = FALSE) %>%
    group_by(lon, lat) %>%
    summarize(across(c("bne_fraction", "ibs_fraction", "tebs_fraction", "bns_fraction", "tundra_fraction"), list(mean = ~round(mean(.x, na.rm = TRUE), digits = 4)),
                      .names = "{.col}_{.fn}")) %>%
    pivot_longer(cols = c("bne_fraction_mean", "ibs_fraction_mean", "tebs_fraction_mean", "bns_fraction_mean", "tundra_fraction_mean"),
                 names_to = "pft",
                 values_to = "fraction_mean") %>%
    mutate(pft = gsub("_fraction_mean", "", pft)) %>%
    group_by(lon, lat) %>%
    mutate(dominant_pft = pft_to_number(pft[which.max(fraction_mean)])) #convert to numeric code for raster
  
  shp = df %>%
    select(-fraction_mean, -pft) %>%
    select(lon, lat, dominant_pft) %>%
    terra::rast(crs = "EPSG:4326") %>% # convert to  raster
    as.polygons(aggregate = T) %>% # convert to shapefile 
    st_as_sf() %>%
    mutate(dominant_pft = number_to_pft(dominant_pft), # convert back to names
           s = climate_scenario,
           d = round(as.numeric(disturbance_regime), 3))
  
  st_write(shp, paste0("data/final/shp/vegetation_", create_name_timeslice(climate_scenario, round(as.numeric(disturbance_regime), 3), variable, year), ".shp"), append = F)
  
  return(df)
}

final_1C = function(Configurations, variable, year, ncore) {
  
  plan(multisession, workers = ncore)
  
  results = future_pmap_dfr(Configurations, ~final_1C_configuration(..1, ..2, variable, year))
  
  results %>%
    write_csv(paste0("data/final/final_vegetationC_", variable, ".csv"))
}

final_1C(Configurations = data.frame(climate_scenario = c("picontrol",  "ssp585" ,  "ssp585"),
                                     disturbance_regime = c("0.04", "0.003333333", "0.04")),
         variable = "fpc",
         year = 2100,
         ncore = 4)
