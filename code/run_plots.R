setwd("~/Desktop/PhD/disturbanceBorealLPJ") 

install.packages(c('scico', 'cowplot', 'rnaturalearth', 'rnaturalearthdata',  'stars', 'abind', 'terra', 
                   'sf', 'furrr', 'future', 'forcats', 'stringr', 'dplyr', 'purrr', 
                   'readr', 'tidyr', 'tibble', 'ggplot2', 'tidyverse'), 
                 version = c('1.3.1', '1.1.1', '0.3.2', '1.0.0', '0.6-4', '1.4-5', '1.7-65', 
                             '1.0-15', '0.3.1', '1.33.1', '0.5.2', '1.5.0', '1.0.10', 
                             '1.0.1', '2.1.3', '1.2.1', '3.1.8', '3.4.0', '1.3.2'), 
                 repos='https://cran.rstudio.com/')

source("code/vegetation_plot_diss.R")
source("code/factors_plot_diss.R")
source("code/albedo_plot_diss.R")
source("code/evapotranspiration_plot_diss.R")
