library(tidyverse)
library(furrr)
library(sf)
library(terra)

source("code/utils_general.R")

climate_scenario = "ssp585"
disturbance_regime = 0.04
year = 2100
## 4A

final_4A_configuration = function(climate_scenario, disturbance_regime, year) {
  
  disturbance_regime = as.character(disturbance_regime) #arrives as a factor which gives use trouble
  
  df_individual_years = read_csv(paste0("data/processed/", create_name_timeslice(climate_scenario, round(as.numeric(disturbance_regime), 3), "met", year), "_processed.csv"),
                                 show_col_types = FALSE) %>%
    group_by(year, month) %>%
    summarize(across(aet, list(mean = ~round(mean(.x, na.rm = TRUE), digits = 4)),
                     .names = "{.col}_{.fn}")) %>%
    ungroup() 
  
  df_mean = df_individual_years %>%
    group_by(month) %>%
    summarize(across(aet_mean, list(mean = ~round(mean(.x, na.rm = TRUE), digits = 4),
                                    sd = ~round(sd(.x, na.rm = TRUE), digits = 4)))) %>%
    rename(aet_mean_30years = aet_mean_mean,
           aet_sd_30years = aet_mean_sd)
  
  df = full_join(df_mean, df_individual_years) %>%
    mutate(s = climate_scenario,
           d = round(as.numeric(disturbance_regime), 3))
  
  rm(df_individual_years, df_mean)
  gc()
  
  return(df)
}

final_4A = function(Climate_Scenarios, Disturbance_Regimes, year, ncore) {
  combinations = expand.grid(climate_scenario = Climate_Scenarios, disturbance_regime = Disturbance_Regimes)
  
  plan(multisession, workers = ncore)
  
  results = future_pmap_dfr(combinations, ~final_4A_configuration(..1, ..2, year))
  
  results %>%
    write_csv(paste0("data/final/final_evapotransA.csv"))
  
  rm(results)
  gc()
}

final_4A(Climate_Scenarios = c("ssp585" , "ssp126" , "picontrol"),
         Disturbance_Regimes = c("0.04", "0.003"),
         year = 2100,
         ncore = 4)

final_4A_configuration_appendix = function(climate_scenario, disturbance_regime, year) {
  
  disturbance_regime = as.character(disturbance_regime) #arrives as a factor which gives use trouble
  
  df_individual_years = read_csv(paste0("data/processed/", create_name_timeslice(climate_scenario, round(as.numeric(disturbance_regime), 3), "met", year), "_processed.csv"),
                                 show_col_types = FALSE) %>%
    group_by(year, month) %>%
    summarize(across(c(transpiration, soil_evaporation, interception, runoff), list(mean = ~round(mean(.x, na.rm = TRUE), digits = 4)),
                     .names = "{.col}_{.fn}")) %>%
    ungroup() 
  
  
  
  df = df_individual_years %>%
    mutate(s = climate_scenario,
           d = round(as.numeric(disturbance_regime), 3))
  
  rm(df_individual_years)
  gc()
  
  return(df)
}

final_4A_appendix = function(Climate_Scenarios, Disturbance_Regimes, year, ncore) {
  combinations = expand.grid(climate_scenario = Climate_Scenarios, disturbance_regime = Disturbance_Regimes)
  
  plan(multisession, workers = ncore)
  
  results = future_pmap_dfr(combinations, ~final_4A_configuration_appendix(..1, ..2, year))
  
  results %>%
    write_csv(paste0("data/final/final_evapotrans_appendix.csv"))
  
  rm(results)
  gc()
}

final_4A_appendix(Climate_Scenarios = c("ssp585" , "ssp126" , "picontrol"),
         Disturbance_Regimes = c("0.04", "0.003"),
         year = 2100,
         ncore = 4)

## 4B

final_4B_configuration = function(climate_scenario, disturbance_regime, year, season) {
  
  disturbance_regime = as.character(disturbance_regime) #arrives as a factor which gives use trouble
  
  df = read_csv(paste0("data/processed/", create_name_timeslice(climate_scenario, round(as.numeric(disturbance_regime), 3), "met", year), "_processed.csv"),
                show_col_types = FALSE) %>%
    select(lon, lat, year, month, aet) %>%
    filter(month %in% season) %>%
    group_by(year) %>%
    summarize(across(aet, list(mean = ~round(mean(.x, na.rm = TRUE), digits = 4)), #spatial mean
                     .names = "{.col}")) %>%
    ungroup() %>%
    group_by() %>%
    summarize(across(aet, list(mean = ~round(mean(.x, na.rm = TRUE), digits = 4), #climatology
                               sd = ~round(sd(.x, na.rm = TRUE), digits = 4)), #interannual variability
                     .names = "{.col}_{.fn}_30years")) %>%
    mutate(s = climate_scenario,
           d = round(as.numeric(disturbance_regime), 3))
  
  return(df)
}

final_4B = function(Climate_Scenarios, Disturbance_Regimes, year, season, ncore) {
  combinations = expand.grid(climate_scenario = Climate_Scenarios, disturbance_regime = Disturbance_Regimes)
  
  plan(multisession, workers = ncore)
  
  results = future_pmap_dfr(combinations, ~final_4B_configuration(..1, ..2, year, season))
  
  results %>%
    write_csv(paste0("data/final/final_evapotransB.csv"))
  
  rm(results)
  gc()
}

final_4B(Climate_Scenarios = c("ssp585" , "ssp126" , "picontrol"),
         Disturbance_Regimes = c("0.04", "0.003", "0.01", "0.1"),
         year = 2100,
         season = c("mar", "apr", "may"),
         ncore = 4)

## 3C

final_4C_configuration = function(climate_scenario, disturbance_regime, year, season) {
  
  disturbance_regime = as.character(disturbance_regime) #arrives as a factor which gives use trouble
  
  df_configuration = read_csv(paste0("data/processed/", create_name_timeslice(climate_scenario, round(as.numeric(disturbance_regime), 3), "met", year), "_processed.csv"),
                              show_col_types = FALSE) %>%
    select(lon, lat, year, month, aet) %>%
    filter(month %in% season) %>%
    group_by(lon, lat) %>%
    summarize(across(aet, list(mean = ~round(mean(.x, na.rm = TRUE), digits = 4),
                               sd = ~round(sd(.x, na.rm = TRUE), digits = 4)), 
                     .names = "{.col}_{.fn}_30years")) %>%
    ungroup()
    
    
  
  df_anomaly = read_csv(paste0("data/processed/", create_name_timeslice("picontrol", 0.003, "met", year), "_processed.csv"),
                        show_col_types = FALSE) %>%
    select(lon, lat, year, month, aet) %>%
    filter(month %in% season) %>%
    group_by(lon, lat) %>%
    summarize(across(aet, list(mean = ~round(mean(.x, na.rm = TRUE), digits = 4), #climatology
                                  sd = ~round(sd(.x, na.rm = TRUE), digits = 4)), #interannual variability
                     .names = "reference_{.fn}_30years")) %>%
    ungroup() %>%
    left_join(df_configuration) %>%
    mutate(aet_anomaly_mean_30years = round(aet_mean_30years - reference_mean_30years, 4),
           aet_anomaly_sd_30years = round(aet_sd_30years + reference_sd_30years, 6),
           s = climate_scenario,
           d = round(as.numeric(disturbance_regime), 3)) %>%
    select(-reference_mean_30years, -reference_sd_30years, -aet_mean_30years, -aet_sd_30years)
  
  shp = df_anomaly %>%
    select(lon, lat, aet_anomaly_mean_30years) %>%
    terra::rast(crs = "EPSG:4326") %>% # convert to  raster
    as.polygons(dissolve = F) %>% # convert to shapefile 
    st_as_sf() %>%
    mutate(s = climate_scenario,
           d = round(as.numeric(disturbance_regime), 3))
  
  st_write(shp, paste0("data/final/shp/evapotrans_anomaly_", create_name_timeslice(climate_scenario, round(as.numeric(disturbance_regime), 3), "met", year), ".shp"), append = F)
  
  rm(df_configuration)
  gc()
  
  return(df_anomaly)
}

final_4C = function(Configurations, year, season, ncore) {
  
  plan(multisession, workers = ncore)
  
  results = future_pmap_dfr(Configurations, ~final_4C_configuration(..1, ..2, year, season))
  
  results %>%
    write_csv("data/final/final_evapotransC.csv")
}

final_4C(Configurations = data.frame(climate_scenario = c("picontrol",  "ssp585" ,  "ssp585"),
                                     disturbance_regime = c("0.04", "0.003333333", "0.04")),
         year = 2100,
         season = c("mar", "apr", "may"),
         ncore = 4)


final_3C_configuration_significance = function(climate_scenario, disturbance_regime, year, season) {
  df_anomaly = read_csv(paste0("data/processed/", create_name_timeslice("picontrol", 0.003, "met", year), "_processed.csv"),
                        show_col_types = FALSE) %>%
    select(lon, lat, year, month, aet) %>%
    filter(month %in% season) %>%
    group_by(lon, lat, year) %>%
    summarize(across(aet, list(mean = ~round(mean(.x, na.rm = TRUE), digits = 4)),
                     .names = "reference_{.fn}"))
  
  
  df_configuration = read_csv(paste0("data/processed/", create_name_timeslice(climate_scenario, round(as.numeric(disturbance_regime), 3), "met", year), "_processed.csv"),
                              show_col_types = FALSE) %>%
    select(lon, lat, year, month, aet) %>%
    filter(month %in% season) %>%
    group_by(lon, lat, year) %>%
    summarize(across(aet, list(mean = ~round(mean(.x, na.rm = TRUE), digits = 4)),
                     .names = "{.col}_{.fn}")) %>%
    full_join(df_anomaly) %>%
    group_by(lon, lat) %>%
    mutate(p_value = wilcox.test(aet_mean, reference_mean, alternative = "two.sided")$p.value) %>%
    mutate(significant = case_when(p_value < 0.01 ~ "1",
                                   TRUE ~ "0")) %>%
    select(lon, lat, p_value, significant) 
  
  shp = df_configuration %>%
    select(lon, lat, significant) %>%
    terra::rast(crs = "EPSG:4326") %>% # convert to  raster
    as.polygons(dissolve = F, aggregate = T) %>% # convert to shapefile 
    st_as_sf() %>%
    mutate(s = climate_scenario,
           d = round(as.numeric(disturbance_regime), 3))
  
  st_write(shp, paste0("data/final/shp/evapotrans_significance_", create_name_timeslice(climate_scenario, round(as.numeric(disturbance_regime), 3), "met", year), ".shp"), 
           append = F)
  
  rm(df_anomaly)
  gc()
  
  return(df_configuration)
}

final_3C_significance = function(Configurations, year, season, ncore) {
  
  plan(multisession, workers = ncore)
  
  results = future_pmap_dfr(Configurations, ~final_3C_configuration_significance(..1, ..2, year, season))
  
  results %>%
    write_csv("data/final/final_evapotransC_significance.csv")
}

final_3C_significance(Configurations = data.frame(climate_scenario = c("picontrol",  "ssp585" ,  "ssp585"),
                                                  disturbance_regime = c("0.04", "0.003333333", "0.04")),
                      year = 2100,
                      season = c("mar", "apr", "may"),
                      ncore = 4)

