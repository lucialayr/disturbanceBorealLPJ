library(tidyverse)
library(furrr)

source("code/utils_general.R")
source("code/utils_biophysics.R")

## time slice
albedo_timeslice_subset_processed_single = function(climate_scenario, disturbance_regime, year) {
  
  disturbance_regime = as.character(disturbance_regime) #arrives as a factor which gives use trouble
  
  snow = read_timeslice(climate_scenario, disturbance_regime, "msnowdepth", year) %>%
    mutate(across(c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"), 
                  calculate_snow_cover)) %>%
    pivot_longer(cols = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"),
                 names_to = "month", values_to = "snow_cover") %>%
    mutate(season =  case_when(month %in% c("jan", "feb", "mar", "dec", "nov", "oct") ~ "winter",
                               month %in% c("apr", "may","jun", "jul", "aug", "sep") ~ "summer",
                               TRUE ~ NA_character_))
  
  if (file.exists(paste0("data/processed/", create_name_timeslice(climate_scenario, round(as.numeric(disturbance_regime), 3), "fpc", year), "_processed.csv"))) {
    fpc = read_csv(paste0("data/processed/", create_name_timeslice(climate_scenario, round(as.numeric(disturbance_regime), 3), "fpc", year), "_processed.csv"),
                   show_col_types = FALSE) %>%
      reclassify_vegetation_albedo() %>%
      mutate(soil = calculate_bare_soil_fraction(total)) %>%
      select(lon, lat, year, total, evergreen, summergreen, tundra, soil) %>%
      full_join(snow) %>%
      calculate_albedo()
    
    write_csv(fpc, paste0("data/processed/", create_name_timeslice(climate_scenario, round(as.numeric(disturbance_regime), 3), "albedo", year), "_processed.csv"))
  } else {
    print(paste0("data/processed/", create_name_timeslice(climate_scenario, round(as.numeric(disturbance_regime), 3), "fpc", year), "_processed.csv does not exist on disk."))
  }
  
  rm(fpc)
  rm(snow)
  gc()
  
}

albedo_timeslice_subset_processed_all = function(Climate_Scenarios, Disturbance_Regimes, year, ncore) {
  combinations = expand.grid(climate_scenario = Climate_Scenarios, disturbance_regime = Disturbance_Regimes)
  
  plan(multisession, workers = ncore)
  
  results = future_pmap(combinations, ~albedo_timeslice_subset_processed_single(..1, ..2, year))
}

albedo_timeslice_subset_processed_all(Climate_Scenarios = c("ssp370" , "ssp585" , "ssp126" , "picontrol"),
                                      Disturbance_Regimes = c("0.04", "0.003333333", "0.1", "0.01"),
                                      year = 2100,
                                      ncore = 4)

## time series

albedo_timeseries_subset_processed_single = function(climate_scenario, disturbance_regime) {

  disturbance_regime = as.character(disturbance_regime) #arrives as a factor which gives use trouble
  
  snow_global = read_timeseries(climate_scenario, disturbance_regime, "msnowdepth", "global") %>%
    mutate(across(c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"), 
                  calculate_snow_cover)) %>%
    pivot_longer(cols = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"),
                 names_to = "month", values_to = "snow_cover") %>%
    mutate(season =  case_when(month %in% c("jan", "feb", "mar", "dec", "nov", "oct") ~ "winter",
                               month %in% c("apr", "may","jun", "jul", "aug", "sep") ~ "summer",
                               TRUE ~ NA_character_))
  
  snow_smoothed = read_timeseries(climate_scenario, disturbance_regime, "msnowdepth", "smoothed") %>%
    mutate(across(c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"), 
                  calculate_snow_cover)) %>%
    pivot_longer(cols = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"),
                 names_to = "month", values_to = "snow_cover") %>%
    mutate(season =  case_when(month %in% c("jan", "feb", "mar", "dec", "nov", "oct") ~ "winter",
                               month %in% c("apr", "may","jun", "jul", "aug", "sep") ~ "summer",
                               TRUE ~ NA_character_))
  
  albedo_global = read_timeseries(climate_scenario, disturbance_regime, "fpc", "global") %>%
    check_and_fill_pfts() %>%
    summarize_pfts() %>%
    reclassify_vegetation_albedo() %>%
    select(year, total, evergreen, summergreen, tundra, soil) %>%
    full_join(snow_global) %>%
    calculate_albedo()
  
  albedo_smoothed = read_timeseries(climate_scenario, disturbance_regime, "fpc", "smoothed") %>%
    check_and_fill_pfts() %>%
    summarize_pfts() %>%
    reclassify_vegetation_albedo() %>%
    select(year, total, evergreen, summergreen, tundra, soil) %>%
    full_join(snow_smoothed) %>%
    calculate_albedo()
  
  write_csv(albedo_global, 
            paste0("data/processed/", create_name_timeseries(climate_scenario, round(as.numeric(disturbance_regime), 3), "albedo", "global"), "_processed.csv"))
  
  write_csv(albedo_smoothed, 
            paste0("data/processed/", create_name_timeseries(climate_scenario, round(as.numeric(disturbance_regime), 3), "albedo", "smoothed"), "_processed.csv"))
  
  rm(snow_global, snow_smoothed, albedo_global, albedo_smoothed)
  gc()
}

albedo_timeseries_subset_process_configuration = function(climate_scenario, disturbance_regime, ncore) {
  combinations = expand.grid(climate_scenario = c("picontrol", climate_scenario), 
                             disturbance_regime = c("0.003333333", disturbance_regime))
  
  plan(multisession, workers = ncore)
  
  results = future_pmap(combinations, ~albedo_timeseries_subset_processed_single(..1, ..2))
}

albedo_timeseries_subset_process_configuration("ssp585", "0.04", ncore = 4)












