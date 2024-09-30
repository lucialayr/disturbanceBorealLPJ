library(tidyverse)
library(furrr)


source("code/utils_general.R")
source("code/utils_biophysics.R")


## time slice 
evapotranspiration_timeslice_subset_processed_single = function(climate_scenario, disturbance_regime, year) {
  
  disturbance_regime = as.character(disturbance_regime) #arrives as a factor which gives use trouble
  

  transpiration = read_timeslice(climate_scenario, disturbance_regime, "maet", year) %>%
    pivot_longer(cols = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"), 
                 names_to = "month", values_to = "transpiration") 
  
  soil_evaporation = read_timeslice(climate_scenario, disturbance_regime, "mevap", year) %>%
    pivot_longer(cols = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"), 
                 names_to = "month", values_to = "soil_evaporation")
  
  interception = read_timeslice(climate_scenario, disturbance_regime, "mintercep", year) %>%
    pivot_longer(cols = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"), 
                 names_to = "month", values_to = "interception")
  
  runoff = read_timeslice(climate_scenario, disturbance_regime, "mrunoff", year) %>%
    pivot_longer(cols = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"), 
                 names_to = "month", values_to = "runoff")
  
  
  components = list(transpiration, soil_evaporation, interception, runoff)
  
  df = purrr::reduce(components, full_join) %>%
    mutate(aet = transpiration + soil_evaporation + interception) 
  
  write_csv(df, paste0("data/processed/", create_name_timeslice(climate_scenario, round(as.numeric(disturbance_regime), 3), "met", year), "_processed.csv"))
  
  rm(df, transpiration, soil_evaporation, interception, components)
  gc()
}

evapotranspiration_timeslice_subset_processed_all = function(Climate_Scenarios, Disturbance_Regimes, year, ncore) {
  combinations = expand.grid(climate_scenario = Climate_Scenarios, disturbance_regime = Disturbance_Regimes)
  
  plan(multisession, workers = ncore)
  
  results = future_pmap(combinations, ~evapotranspiration_timeslice_subset_processed_single(..1, ..2, year))
}

evapotranspiration_timeslice_subset_processed_all(Climate_Scenarios = c("ssp370" , "ssp585" , "ssp126" , "picontrol"),
                                                  Disturbance_Regimes = c("0.04", "0.003333333", "0.1", "0.01"),
                                                  year = 2100,
                                                  ncore = 4)

## time series

evapotranspiration_timeseries_subset_process_single = function(climate_scenario, disturbance_regime) {
  
  disturbance_regime = as.character(disturbance_regime) #arrives as a factor which gives use trouble
  
  transpiration = read_timeseries(climate_scenario, disturbance_regime, "maet", "global") %>%
    pivot_longer(cols = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"), 
                 names_to = "month", values_to = "transpiration") 
  
  interception = read_timeseries(climate_scenario, disturbance_regime, "mintercep", "global") %>%
    pivot_longer(cols = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"), 
                 names_to = "month", values_to = "interception")
  
  soil_evaporation = read_timeseries(climate_scenario, disturbance_regime, "mevap", "global") %>%
    pivot_longer(cols = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"), 
                 names_to = "month", values_to = "soil_evaporation")
  

  components = list(transpiration, soil_evaporation, interception)
  
  evapotranspiration_global = purrr::reduce(components, full_join) %>%
    mutate(aet = transpiration + soil_evaporation + interception )
  
  write_csv(evapotranspiration_global, 
            paste0("data/processed/", create_name_timeseries(climate_scenario, round(as.numeric(disturbance_regime), 3), "met", "global"), "_processed.csv"))
  
  transpiration = read_timeseries(climate_scenario, disturbance_regime, "maet", "smoothed") %>%
    pivot_longer(cols = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"), 
                 names_to = "month", values_to = "transpiration") 
  
  interception = read_timeseries(climate_scenario, disturbance_regime, "mintercep", "smoothed") %>%
    pivot_longer(cols = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"), 
                 names_to = "month", values_to = "interception")
  soil_evaporation = read_timeseries(climate_scenario, disturbance_regime, "mevap", "smoothed") %>%
    pivot_longer(cols = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"), 
                 names_to = "month", values_to = "soil_evaporation")
  
  components = list(transpiration, soil_evaporation, interception)
  
  evapotranspiration_smoothed = purrr::reduce(components, full_join) %>%
    mutate(aet = transpiration + soil_evaporation + interception)
  
  write_csv(evapotranspiration_smoothed, 
            paste0("data/processed/", create_name_timeseries(climate_scenario, round(as.numeric(disturbance_regime), 3), "met", "smoothed"), "_processed.csv"))
  
  rm(transpiration, soil_evaporation, interception, components, evapotranspiration_global, evapotranspiration_smoothed)
  gc()
  
}

evapotranspiration_timeseries_subset_process_configuration = function(climate_scenario, disturbance_regime, ncore) {
  combinations = expand.grid(climate_scenario = c("picontrol", climate_scenario), 
                             disturbance_regime = c("0.003333333", disturbance_regime))
  
  plan(multisession, workers = ncore)
  
  results = future_pmap(combinations, ~evapotranspiration_timeseries_subset_process_single(..1, ..2))
}

evapotranspiration_timeseries_subset_process_configuration("ssp585", "0.04", ncore = 4)
