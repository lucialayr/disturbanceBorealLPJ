# Use an R base image with a specific R version
FROM rocker/r-ver:4.2.2


# Install system dependencies for required R packages
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    libnetcdf-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    gcc \
    && rm -rf /var/lib/apt/lists/*

# Copy the Dockerfile and set the working directory
COPY . /app
WORKDIR /app

# Install R packages versions that were used for the analysis
RUN R -e "install.packages(c('scico', 'cowplot', 'rnaturalearth', 'stars', 'abind', 'terra', 'sf', 'furrr', 'future', 'forcats', 'stringr', 'dplyr', 'purrr', 'readr', 'tidyr', 'tibble', 'ggplot2', 'tidyverse'), version = c('1.3.1', '1.1.1', '0.3.2', '0.6-4', '1.4-5', '1.7-65', '1.0-15', '0.3.1', '1.33.1', '0.5.2', '1.5.0', '1.0.10', '1.0.1', '2.1.3', '1.2.1', '3.1.8', '3.4.0', '1.3.2'), repos='https://cran.rstudio.com/')"

# Start an interactive shell by default #this should allow to be able to manually execute makefile
CMD ["R"]
