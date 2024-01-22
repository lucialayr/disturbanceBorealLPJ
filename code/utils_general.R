# general helper function to read in and process data

## check if all pfts are present in data and if not, add them with value 0
check_and_fill_pfts = function(df) { 
  df = df %>%
    mutate(bine = {if("bine" %in% names(.)) bine else 0},
           bne = {if("bne" %in% names(.)) bne else 0},
           ibs = {if("ibs" %in% names(.)) ibs else 0},
           tebs = {if("tebs" %in% names(.)) tebs else 0},
           tene = {if("tene" %in% names(.)) tene else 0},
           bns = {if("bns" %in% names(.)) bns else 0},
           c3g = {if("c3g" %in% names(.)) c3g else 0},
           hse = {if("hse" %in% names(.)) hse else 0},
           hss = {if("hss" %in% names(.)) hss else 0},
           lse = {if("lse" %in% names(.)) lse else 0},
           lss = {if("lss" %in% names(.)) lss else 0},
           grt = {if("grt" %in% names(.)) grt else 0},
           epds = {if("epds" %in% names(.)) epds else 0},
           spds = {if("spds" %in% names(.)) spds else 0},
           clm = {if("clm" %in% names(.)) clm else 0})
  return(df)
} 
                                      
create_name_timeslice = function(s,d, var, year) {
  name = paste0(s, "_d", d, "_", var, "_30years", year)
  return(name)
}

create_name_timeseries = function(s,d, var, type) {
  name = paste0(s, "_d", d, "_", var, "_", type, "_mean")
  return(name)
}

calculate_bare_soil_fraction = function(x) {
  soil = ifelse(1 - x > 0, 1 - x, 0)
}

# read time slice data for any variable
read_timeslice = function(s, d, v, y) {
  name = create_name_timeslice(s, d, v, y)

  coords_boreal = read.table("data/ext/coords_per_biome.txt") %>% #move this to aux data or something else
    filter(BIOME == 6) %>%
    mutate(id = paste0(Lat, "_", Lon)) 
  
  if (file.exists(paste0("data/subset/", name, ".out")))  {
    df = read.table(paste0("data/subset/", name, ".out"), header = TRUE)  %>%
      rename_with(tolower) %>%
      mutate(id = paste0(lat, "_", lon)) %>%
      filter(id %in% coords_boreal$id) %>%
      select(-id)
    
    return(df)
  } else {print(paste(name, "does not exsist on disk."))}
 
}

read_timeseries = function(s, d, v, t) {
  name = create_name_timeseries(s, d, v, t)
  
  if (file.exists(paste0("data/subset/", create_name_timeseries(s, d, v, t), ".csv")))  {
    df = read_csv(paste0("data/subset/", create_name_timeseries(s, d, v, t), ".csv"), 
             show_col_types = FALSE) %>%
      pivot_wider(names_from = "variable", values_from = "value_mean") %>%
      rename_with(tolower) 
    
    return(df)
  } else {print(paste(name, "does not exsist on disk."))}
  
}

# summarize pfts to aggregated classes as described in methods
summarize_pfts = function(df) {
  df = df %>%
    mutate(bne = bne + bine,
           tundra = (c3g + hse + hss + lse + lss + grt + epds + spds + clm) ) %>% 
    select(-bine,  -c3g, -hse, -hss, -lse, -lss,  -grt, -epds, -spds, -clm)
  
  return(df)
}

summarize_pfts_broadleaves = function(df) {
  df = df %>%
    mutate(bl = ibs + tebs) %>%
    select(-ibs, -tebs)
  return(df)
}

pft_to_number = function(x) {
  x = gsub("bne", 1, x)
  x = gsub("ibs", 2, x)
  x = gsub("tebs", 3, x)
  x = gsub("bns", 4, x)
  x = gsub("tundra", 6, x)
  x = gsub("soil", 7, x)
  return(x)
}

season_to_months = function(x) {
  x = str_replace_all(x, c("DJF" = "dec,jan,feb", 
                           "MAM" = "mar,apr,may", 
                           "JJA" = "jun,jul,aug", 
                           "SON" = "sep,oct,nov"))
  x = unlist(strsplit(x, ","))
return(x)
}

number_to_pft = function(x) {
  x = gsub("1", "bne", x)
  x = gsub("2", "ibs", x)
  x = gsub("3", "tebs", x)
  x = gsub("4", "bns", x)
  x = gsub("6", "tundra", x)
  x = gsub("7", "soil", x)
  return(x)
}
