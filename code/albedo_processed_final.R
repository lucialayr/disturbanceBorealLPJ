library(tidyverse)
library(stars)
library(sf)
library(furrr)

source("code/utils_general.R")

## 3A

final_3A_configuration = function(climate_scenario, disturbance_regime, year) {
  
  disturbance_regime = as.character(disturbance_regime) #arrives as a factor which gives use trouble
  
  df_individual_years = read_csv(paste0("data/processed/", create_name_timeslice(climate_scenario, round(as.numeric(disturbance_regime), 3), "albedo", year), "_processed.csv"),
                                 show_col_types = FALSE) %>%
    group_by(year, month) %>%
    summarize(across(albedo, list(mean = ~round(mean(.x, na.rm = TRUE), digits = 4)),
                     .names = "{.col}_{.fn}")) %>%
    ungroup() 
  
  df_mean = df_individual_years %>%
    group_by(month) %>%
    summarize(across(albedo_mean, list(mean = ~round(mean(.x, na.rm = TRUE), digits = 4),
                                    sd = ~round(sd(.x, na.rm = TRUE), digits = 4)))) %>%
    rename(albedo_mean_30years = albedo_mean_mean,
           albedo_sd_30years = albedo_mean_sd)
  
  df = full_join(df_mean, df_individual_years) %>%
    mutate(s = climate_scenario,
           d = round(as.numeric(disturbance_regime), 3))
  
  rm(df_individual_years, df_mean)
  gc()
  
  return(df)
}

final_3A = function(Climate_Scenarios, Disturbance_Regimes, year, ncore) {
  combinations = expand.grid(climate_scenario = Climate_Scenarios, disturbance_regime = Disturbance_Regimes)
  
  plan(multisession, workers = ncore)
  
  results = future_pmap_dfr(combinations, ~final_3A_configuration(..1, ..2, year))
  
  results %>%
    write_csv(paste0("data/final/final_albedoA.csv"))
  
  rm(results)
  gc()
}

final_3A(Climate_Scenarios = c("ssp585" , "ssp126" , "picontrol"),
         Disturbance_Regimes = c("0.04", "0.003"),
         year = 2100,
         ncore = 4)

## 3B

final_3B_configuration = function(climate_scenario, disturbance_regime, year, season) {
  
  disturbance_regime = as.character(disturbance_regime) #arrives as a factor which gives use trouble
  
  df = read_csv(paste0("data/processed/", create_name_timeslice(climate_scenario, round(as.numeric(disturbance_regime), 3), "albedo", year), "_processed.csv"),
                                 show_col_types = FALSE) %>%
    select(lon, lat, year, month, albedo) %>%
    filter(month %in% season) %>%
    group_by(year) %>%
    summarize(across(albedo, list(mean = ~round(mean(.x, na.rm = TRUE), digits = 4)),
                     .names = "{.col}")) %>%
    ungroup() %>%
    summarize(across(albedo, list(mean = ~round(mean(.x, na.rm = TRUE), digits = 4),
                                  sd = ~round(sd(.x, na.rm = TRUE), digits = 6)),
                     .names = "{.col}_{.fn}_30years")) %>%
    mutate(s = climate_scenario,
           d = round(as.numeric(disturbance_regime), 3))
    
  return(df)
}

final_3B = function(Climate_Scenarios, Disturbance_Regimes, year, season, ncore) {
  combinations = expand.grid(climate_scenario = Climate_Scenarios, disturbance_regime = Disturbance_Regimes)
  
  plan(multisession, workers = ncore)
  
  results = future_pmap_dfr(combinations, ~final_3B_configuration(..1, ..2, year, season))
  
  results %>%
    write_csv(paste0("data/final/final_albedoB.csv"))
  
  rm(results)
  gc()
}

final_3B(Climate_Scenarios = c("ssp585" , "ssp126" , "picontrol"),
         Disturbance_Regimes = c("0.04", "0.003", "0.01", "0.1"),
         year = 2100,
         season = c("jan", "feb", "dec"),
         ncore = 4)

## 3C

final_3C_configuration = function(climate_scenario, disturbance_regime, year, season) {
  
  disturbance_regime = as.character(disturbance_regime) #arrives as a factor which gives use trouble
  
  df_configuration = read_csv(paste0("data/processed/", create_name_timeslice(climate_scenario, round(as.numeric(disturbance_regime), 3), "albedo", year), "_processed.csv"),
                show_col_types = FALSE) %>%
    select(lon, lat, year, month, albedo) %>%
    filter(month %in% season) %>%
    group_by(lon, lat) %>%
    summarize(across(albedo, list(mean = ~round(mean(.x, na.rm = TRUE), digits = 4),
                                  sd = ~round(sd(.x, na.rm = TRUE), digits = 4)),
                     .names = "{.col}_{.fn}_30years"))
  
  df_anomaly = read_csv(paste0("data/processed/", create_name_timeslice("picontrol", 0.003, "albedo", year), "_processed.csv"),
                              show_col_types = FALSE) %>%
    select(lon, lat, year, month, albedo) %>%
    filter(month %in% season) %>%
    group_by(lon, lat) %>%
    summarize(across(albedo, list(mean = ~round(mean(.x, na.rm = TRUE), digits = 4),
                                  sd = ~round(sd(.x, na.rm = TRUE), digits = 4)),
                     .names = "reference_{.fn}_30years")) %>%
    ungroup() %>%
   left_join(df_configuration) %>%
    mutate(albedo_anomaly_mean_30years = round(albedo_mean_30years - reference_mean_30years, 4),
           albedo_anomaly_sd_30years = round(albedo_sd_30years + reference_sd_30years, 6),
           s = climate_scenario,
           d = round(as.numeric(disturbance_regime), 3)) %>%
    select(-reference_mean_30years, -reference_sd_30years, -albedo_mean_30years, -albedo_sd_30years)
  
  shp = df_anomaly %>%
    select(lon, lat, albedo_anomaly_mean_30years) %>%
    mutate(albedo_anomaly_mean_30years = round(albedo_anomaly_mean_30years, 4)) %>%
    terra::rast(crs = "EPSG:4326") %>% # convert to  raster
    as.polygons(dissolve = F, aggregate = T) %>% # convert to shapefile 
    st_as_sf() %>%
    mutate(s = climate_scenario,
           d = round(as.numeric(disturbance_regime), 3))
  
  st_write(shp, paste0("data/final/shp/albedo_anomaly_", create_name_timeslice(climate_scenario, round(as.numeric(disturbance_regime), 3), "albedo", year), ".shp"), append = F)
  
  
  rm(df_configuration)
  gc()
  
  return(df_anomaly)
}

final_3C = function(Configurations, year, season, ncore) {
  
  plan(multisession, workers = ncore)
  
  results = future_pmap_dfr(Configurations, ~final_3C_configuration(..1, ..2, year, season))
  
  results %>%
    write_csv("data/final/final_albedoC.csv")
}

final_3C(Configurations = data.frame(climate_scenario = c("picontrol",  "ssp585" ,  "ssp585"),
                                     disturbance_regime = c("0.04", "0.003333333", "0.04")),
         year = 2100,
         season = c("jan", "feb", "dec"),
         ncore = 4)
