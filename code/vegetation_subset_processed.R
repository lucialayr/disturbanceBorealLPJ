library(tidyverse)
library(furrr)

source("code/utils_general.R")

## time slices
vegetation_timeslice_subset_processed_single = function(climate_scenario, disturbance_regime, variable, year) {
  
  variable = as.character(variable) #arrives as a factor which gives use trouble
  disturbance_regime = as.character(disturbance_regime) #arrives as a factor which gives use trouble
  
  df = read_timeslice(climate_scenario, disturbance_regime, variable, year) %>%
    check_and_fill_pfts() %>% #check if all pfts are present in data and if not, add them with value 0
    summarize_pfts() %>% # summarize pfts to aggregated classes as described in methods
    mutate(soil = if (as.character(variable) == "fpc") calculate_bare_soil_fraction(total) else 0, #calculate bare soil fraction
            across(c("bne", "ibs", "tebs", "tene", "bns", "tundra", "soil"), list(fraction = ~./(total + soil)), #calculate fractions of vegetation (not bare soil)
                   .names = "{.col}_{.fn}"), #convert to fraction
            control = bne_fraction + ibs_fraction + tebs_fraction + tene_fraction + bns_fraction + tundra_fraction + soil_fraction) #calculate control, percentages should add up to 1
  
  print(mean(df$total))

  if (!between(mean(df$control), 0.999, 1.001)) {
    print(paste("Warning: sum of fractions is not 1 for", create_name_timeslice(climate_scenario, round(as.numeric(disturbance_regime), 3), variable, year)))
  }

  df %>%
    write_csv(paste0("data/processed/", create_name_timeslice(climate_scenario, round(as.numeric(disturbance_regime), 3), variable, year), "_processed.csv"))
  
  print(paste("Processed", create_name_timeslice(climate_scenario, round(as.numeric(disturbance_regime), 3), variable, year)))
}

vegetation_timeslice_subset_process_all = function(Climate_Scenarios, Disturbance_Regimes, Variables, year, ncore) {
  combinations = expand.grid(climate_scenario = Climate_Scenarios, disturbance_regime = Disturbance_Regimes, variable = Variables)
  
  plan(multisession, workers = ncore)
  
  results = future_pmap(combinations, ~vegetation_timeslice_subset_processed_single(..1, ..2, ..3, year))
}

vegetation_timeslice_subset_process_all(Climate_Scenarios = c("ssp370" , "ssp585" , "ssp126" , "picontrol"),
                                        Disturbance_Regimes = c("0.04", "0.003333333", "0.1", "0.01"),
                                        Variables = c("fpc"),
                                        year = 2100,
                                        ncore = 4)

vegetation_timeslice_subset_process_all(Climate_Scenarios = c("ssp370" , "ssp585" , "ssp126" , "picontrol"),
                                        Disturbance_Regimes = c("0.04", "0.003333333", "0.1", "0.01"),
                                        Variables = c("cmass"),
                                        year = 2100,
                                        ncore = 4)

## time series
vegetation_timeseries_subset_processed_single = function(climate_scenario, disturbance_regime, variable) {
  
  variable = as.character(variable) #arrives as a factor which gives use trouble
  disturbance_regime = as.character(disturbance_regime) #arrives as a factor which gives use trouble
  
  df_global = read_timeseries(climate_scenario, disturbance_regime, variable, "global") %>% #read original timeseries
    check_and_fill_pfts() %>% #check if all pfts are present in data and if not, add them with value 0
    summarize_pfts() %>% # summarize pfts to aggregated classes as described in methods
    summarize_pfts_broadleaves() 
  
  df_smoothed = read_timeseries(climate_scenario, disturbance_regime, variable, "smoothed") %>% #read smoothed data
    check_and_fill_pfts() %>% #check if all pfts are present in data and if not, add them with value 0
    summarize_pfts() %>% # summarize pfts to aggregated classes as described in methods
    summarize_pfts_broadleaves() 
  
  write_csv(df_global, 
            paste0("data/processed/", create_name_timeseries(climate_scenario, round(as.numeric(disturbance_regime), 3), variable, "global"), "_processed.csv"))
  
  write_csv(df_smoothed, 
            paste0("data/processed/", create_name_timeseries(climate_scenario, round(as.numeric(disturbance_regime), 3), variable, "smoothed"), "_processed.csv"))
  
  rm(df_smoothed, df_global)
  gc()
}

vegetation_timeseries_subset_process_configuration = function(climate_scenario, disturbance_regime, variable, ncore) {
  combinations = expand.grid(climate_scenario = c("picontrol", climate_scenario), 
                             disturbance_regime = c("0.003333333", disturbance_regime), 
                             variable = variable)
  
  plan(multisession, workers = ncore)
  
  results = future_pmap(combinations, ~vegetation_timeseries_subset_processed_single(..1, ..2, ..3))
}

vegetation_timeseries_subset_process_configuration(climate_scenario = "ssp585",
                                                   disturbance_regime = "0.04",
                                                   variable = c("lai", "fpc",  "cmass"),
                                                   ncore = 4)

vegetation_timeseries_subset_process_configuration(climate_scenario = "ssp585",
                                                   disturbance_regime = "0.1",
                                                   variable = c("lai", "fpc",  "cmass"),
                                                   ncore = 4)

vegetation_timeseries_subset_process_configuration(climate_scenario = "ssp126",
                                                   disturbance_regime = "0.1",
                                                   variable = c("lai", "fpc",  "cmass"),
                                                   ncore = 4)




