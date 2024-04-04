library(tidyverse)
library(furrr)

source("code/utils_general.R")

# all variables except et paralelized
calculate_effect_single = function(climate_scenario, disturbance_regime, type, variable, column_of_interest, months) {
  
  print(months)
  months = season_to_months(months)
  print(months)
  df_0 = read_csv(paste0("data/processed/", create_name_timeseries("picontrol", 0.003, variable, type), "_processed.csv"),
                  show_col_types = FALSE) %>%
    filter(if (any("month" %in% names(.))) month %in% months else TRUE) %>% #filter for correct season if data is monthly
    group_by(year) %>%
    summarise(x_0 = mean(!!rlang::sym(column_of_interest), na.rm = TRUE)) 
  
  df_s = read_csv(paste0("data/processed/", create_name_timeseries(climate_scenario, 0.003, variable, type), "_processed.csv"),
                  show_col_types = FALSE) %>%
    filter(if (any("month" %in% names(.))) month %in% months else TRUE) %>% #filter for correct season if data is monthly
    group_by(year) %>%
    summarise(s = mean(!!rlang::sym(column_of_interest), na.rm = TRUE))
  
  df_d = read_csv(paste0("data/processed/", create_name_timeseries("picontrol", round(as.numeric(disturbance_regime), 3), variable, type), "_processed.csv"),
                  show_col_types = FALSE) %>%
    filter(if (any("month" %in% names(.))) month %in% months else TRUE) %>% #filter for correct season if data is monthly
    group_by(year) %>%
    summarise(d = mean(!!rlang::sym(column_of_interest), na.rm = TRUE))
  
  df_sd = read_csv(paste0("data/processed/", create_name_timeseries(climate_scenario, round(as.numeric(disturbance_regime), 3), variable, type), "_processed.csv"),
                   show_col_types = FALSE) %>%
    filter(if (any("month" %in% names(.))) month %in% months else TRUE) %>% #filter for correct season if data is monthly
    group_by(year) %>%
    summarise(sd = mean(!!rlang::sym(column_of_interest), na.rm = TRUE))
  
  components = list(df_0, df_s, df_d, df_sd)
  
  df = purrr::reduce(components, full_join) %>%
    mutate(across(c("s", "d", "sd"), list(D = ~ round(. - x_0, 3)),  .names = "D_{col}"),
           D_x = D_sd - D_d - D_s) %>%
    select(-x_0, -s, -d, -sd) %>%
    pivot_longer(cols = c("D_s", "D_d", "D_sd", "D_x"),
                 names_to = "factor", values_to = paste0("effect_", type)) 
  
  return(df)
}

calculate_effect_variable = function(climate_scenario, disturbance_regime, variable, column_of_interest, months) {
  
  df = full_join(calculate_effect_single(climate_scenario, disturbance_regime, "global", variable, column_of_interest, months),
                 calculate_effect_single(climate_scenario, disturbance_regime, "smoothed", variable, column_of_interest, months)) %>%
    pivot_longer(cols = c("effect_global", "effect_smoothed"),
                 names_to = "type", values_to = "effect") %>%
    mutate(variable = column_of_interest)
 
  return(df)
    
}

attribution_processed_final = function(variable_list, climate_scenario, disturbance_regime, ncore) {
  
  plan(multisession, workers = ncore)
  
  results = future_pmap_dfr(variable_list, ~calculate_effect_variable(climate_scenario, disturbance_regime,
                                                                                  ..1, ..2, ..3))
  
  write_csv(results, "data/final/final_factors.csv")
}

attribution_processed_final(variable_list = data.frame(variable = c("albedo", "fpc", "fpc", "fpc", "fpc", "met"),
                                                       column_of_interest = c("albedo", "total", "bne", "tundra", "bl", "aet"),
                                                       months = c("DJF", 0, 0, 0, 0, "MAM")),
                            climate_scenario = "ssp585",
                            disturbance_regime = "0.04",
                            ncore = 4)




