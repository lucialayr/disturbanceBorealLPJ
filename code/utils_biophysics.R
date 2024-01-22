# helper functions specific for calculating albedo and evapotranspiration

calculate_snow_cover = function(x) {
  snowcover = x/(0.1+x)
  return(snowcover)
}

calculate_bare_soil_fraction = function(x) {
  soil = ifelse(1 - x > 0, 1 - x, 0)
}

reclassify_vegetation_albedo = function(df) {
  df = df %>%
    mutate(evergreen = bne + tene,
           summergreen = ibs + tebs + bns)
  
  return(df)
}


calculate_albedo = function(df) {
  albedo_factors = data.frame(row.names  = c("evergreen", "summergreen", "grass", "soil"),
                              summer = c(0.104, 0.153, 0.176, 0.246),
                              winter = c(0.094, 0.117, 0.161, 0.205),
                              winter_snow =c(0.205, 0.244, 0.568, 0.535)) # specific albedos from Boisier
  
  df = df %>%
    mutate(albedo = case_when(season == "winter" ~ evergreen*(albedo_factors["evergreen", "winter_snow"]*snow_cover +  albedo_factors["evergreen", "winter"]*(1-snow_cover)) +
                                tundra*(albedo_factors["grass", "winter_snow"]*snow_cover + albedo_factors["grass", "winter"]*(1-snow_cover)) +
                                summergreen*(albedo_factors["summergreen", "winter_snow"]*snow_cover + albedo_factors["summergreen", "winter"]*(1-snow_cover)) +
                                soil*(albedo_factors["soil", "winter_snow"]*snow_cover + albedo_factors["soil", "winter"]*(1-snow_cover)),
                              season == "summer" ~ evergreen*(albedo_factors["evergreen", "winter_snow"]*snow_cover +  albedo_factors["evergreen", "summer"]*(1-snow_cover)) +
                                tundra*(albedo_factors["grass", "winter_snow"]*snow_cover +  albedo_factors["grass", "summer"]*(1-snow_cover)) +
                                summergreen*(albedo_factors["summergreen", "winter_snow"]*snow_cover +  albedo_factors["summergreen", "summer"]*(1-snow_cover)) +
                                soil*(albedo_factors["soil", "winter_snow"]*snow_cover +  albedo_factors["soil", "summer"]*(1-snow_cover)))) 
  return(df)
}

calculate_evapotranspiration = function(type) {
  
  
}