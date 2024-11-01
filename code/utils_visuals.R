# helper functions for plotting

## labels - convert code-friendly names into reader-friendly names

long_names_months = function(x) {
  x = gsub("jan", "January", x)
  x = gsub("feb", "February", x)
  x = gsub("mar", "March", x)
  x = gsub("apr", "April", x)
  x = gsub("may", "May", x)
  x = gsub("jun", "June", x)
  x = gsub("jul", "July", x)
  x = gsub("aug", "August", x)
  x = gsub("sep", "September", x)
  x = gsub("oct", "October", x)
  x = gsub("nov", "November", x)
  x = gsub("dec", "December", x)
  
  return(x)
}

long_names_pfts = function(x) {
  x = gsub("ibs", "Pioneering broadleaf", x)
  x = gsub("tebs", "Temperate broadleaf", x)
  x = gsub("bne", "Needleleaf evergreen", x)
  x = gsub("bns", "Needleleaf summergreen", x)
  x = gsub("tene", "Temperate needleleaf", x)
  x = gsub("tundra", "Non-tree V.", x)
  x = gsub("soil", "Bare soil", x)
  return(x)
}

long_names_pfts_twolines = function(x) {
  x = gsub("ibs", "Pioneering\nbroadleaf", x)
  x = gsub("tebs", "Temperate\nbroadleaf", x)
  x = gsub("bne", "Needleleaf\nevergreen", x)
  x = gsub("bns", "Needleleaf\nsummergreen", x)
  x = gsub("tene", "Temperate\nneedleleaf", x)
  x = gsub("tundra", "Non-tree V.", x)
  x = gsub("soil", "Bare soil", x)
  return(x)
}

long_names_scenarios = function(x) {
  x = gsub(" control", " Historic", x)
  x = gsub("picontrol", "Control", x)
  x = gsub("ssp126", "SSP1-RCP2.6", x)
  x = gsub("ssp370", "SSP3-RCP7.0", x)
  x = gsub("ssp585", "SSP5-RCP8.5", x)
  
  return(x)
}

long_names_scenarios_twolines = function(x) {
  x = gsub(" control", " Historic", x)
  x = gsub("picontrol", "Control", x)
  x = gsub("ssp126", "SSP1-\nRCP2.6", x)
  x = gsub("ssp370", "SSP3-\nRCP7.0", x)
  x = gsub("ssp585", "SSP5-\nRCP8.5", x)
  
  return(x)
}

long_names_variables = function(x) {
  x = gsub("tas", "Temperature (K)", x)
  x = gsub("rsds", "Radiation (W/m²)", x)
  x = gsub("pr", "Precipitation (kg/m²/year)", x)
  x = gsub("co2", "Atmospheric C02 (ppm)", x)
  x = gsub("fpc", "Fractional plant cover in %", x)
  x = gsub("lai", "Leaf area index in m²/m²", x)
  x = gsub("cmass", "Abovegr. carbon (kg/m²)", x)
  
  return(x)
}

short_names_variables = function(x) {
  x = gsub("fpc", "FPC", x)
  x = gsub("lai", "LAI", x)
  x = gsub("cmass", "AGC", x)
  
  return(x)
}

long_names_attribution = function(x) {
  x = gsub("albedo", "Albedo \n(DJF)", x)
  x = gsub("aet", "ET (JJA) in \nmm/month", x)
  x = gsub("total", "Total plant \ncover in %", x)
  x = gsub("bne", "Needleleaf \ncover in %", x)
  x = gsub("tundra", "Non-tree \ncover in %", x)
  x = gsub("bl", "Broadleaf \ncover in %", x)
  return(x)
}  

## Layout - to help consistent formating

add_common_layout = function(fontsize) {
  theme_classic() %+replace%
    theme(axis.title = element_text(size = fontsize), # fontsize
          strip.text = element_text(size = fontsize),
          text = element_text(size = fontsize),
          legend.background = element_rect(fill='transparent', color = NA), # make plot background transparent, especially helpful for presentations
          legend.box.background = element_rect(fill='transparent', color = NA),
          panel.background = element_rect(fill = "transparent", colour = NA),  
          plot.background = element_rect(fill = "transparent", colour = NA),
          strip.background = element_rect(fill = "transparent", color = NA),
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) #zero margins to make paneling sub panels more contolled
}

##geospatial

load_basemap = function(epsg = "EPSG:3408") {
  
  shp_coastline <<- ne_coastline(scale = "medium", returnclass = "sf")  %>%
    st_transform(crs = epsg) %>%
    st_crop(xmin = -4555364, xmax = 4366954, ymin = -3429227, ymax = 3981392 )
  shp_countries <<- ne_countries(scale = "medium", returnclass = "sf")  %>%
    st_transform(crs = epsg) %>%
    st_make_valid() %>%
    st_crop(xmin = -4555364, xmax = 4366954, ymin = -3429227, ymax = 3981392 )
}

add_basemap = function() {
  list(geom_sf(data=shp_countries, fill = "grey90", color="grey90", linewidth = .0),
       geom_sf(data=shp_coastline, colour = "grey40", linewidth = .15))
}

## color scales

# color scales for anomaly plots

add_fill_anomaly = function(name, endpoint = .3) {
  scico::scale_fill_scico(palette = "vik", name = name, midpoint = 0,
                          breaks = c(-endpoint, 0, endpoint), labels = c(-endpoint, 0, endpoint), limits = c(-endpoint, endpoint)) 
}

# color scale climate scenario

add_color_scenarios = function() {
  scale_color_manual(values = c("Control" = "#DFB793", "SSP5-RCP8.5" = "#190C65", "SSP1-RCP2.6" = "#4483A6", "SSP3-RCP7.0" = "#274E92", "historical" = "#729DA0"),
                     name = "Climate scenario")
}

add_fill_scenarios = function() {
  scale_fill_manual(values = c("Control" = "#DFB793", "SSP5-RCP8.5" = "#190C65", "SSP1-RCP2.6" = "#4483A6", "SSP3-RCP7.0" = "#274E92", "historical" = "#729DA0"),
                     name = "Climate scenario")
}

# color scale attribution

add_scale_factors = function() {
  scale_color_manual(values = c("D_sd" = "grey20", "D_d" = "#95A494", "D_s" = "#4483A6",  "D_x" = "#f3e4d3ff"),
                     labels = c("D_sd" = expression(Total~effect~Delta[~~SD]), "D_d" = expression(Disturbance~Delta[~~D]), "D_s" = expression(Climate~Delta[~~S]),  "D_x" = expression(Interaction~Delta[~~X])),
                     name = "Driver") 
}
  
# color scale pfts

add_pft_scale = function(){
  scale_fill_manual(values = c("Temperate broadleaf" = "#D55E00", "Pioneering broadleaf" = "#E69F00",  "Needleleaf evergreen"  = "#0072B2",   
                    "Needleleaf summergreen" = "#56B4E9",  "Non-tree V." = "#009E73", "Bare soil" = "mistyrose3"),
                    name = "Vegetation type")
}

add_pft_scale_twolines = function(){
  scale_fill_manual(values = c("Temperate\nbroadleaf" = "#D55E00", "Pioneering\nbroadleaf" = "#E69F00",  "Needleleaf\nevergreen"  = "#0072B2",   
                               "Needleleaf\nsummergreen" = "#56B4E9",  "Non-tree V." = "#009E73", "Bare soil" = "mistyrose3"),
                    name = "Vegetation type")
}

# color scale disturbance regime