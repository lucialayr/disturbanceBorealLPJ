library(tidyverse)
library(purrr)
library(rgee)


rgee::ee_Authenticate()
# Define the shapefile path
shapefile_path = 'data/ext/boreal_outline.shp'

# Load shapefile
boreal_shape = ee$FeatureCollection(shapefile_path)

# Define the MODIS white sky albedo dataset
albedo_dataset = ee$ImageCollection("MODIS/061/MCD43A3")

# Define the time range
start_date = "1985-01-01"
end_date = "2015-12-31"