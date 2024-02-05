library(tidyverse)
library(purrr)
library(sf)
library(rnaturalearth)
library(cowplot)

source("code/utils_visuals.R")


plot_1A = function(variable, fontsize) {
  df = read_csv(paste0("data/final/final_vegetationA_", variable, ".csv"), show_col_types = FALSE) %>%
    pivot_longer(cols = -c("d", "s"), names_to = "pft", values_to = "fraction") %>%
    mutate(s = long_names_scenarios(s),
           pft = long_names_pfts(gsub("_fraction_mean", "", pft))) 
  
  df$d = factor(df$d, levels = c("0.003", "0.01", "0.04",  "0.1"))
  df$s = factor(df$s, levels = c("Control", "SSP1-RCP2.6", "SSP3-RCP7.0", "SSP5-RCP8.5"))
  df$pft = factor(df$pft, levels = c("Pioneering\nbroadleaf", "Temperate\nbroadleaf", 
                                     "Needleleaf\nevergreen", "Needleleaf\nsummergreen", "Tundra", "Soil"))
 
  (p = ggplot() + 
      geom_col(data = df,  aes(y = fraction, x = d,  fill = pft), color = "black", linewidth = .3) + 
      xlab("\nDisturbance probability") + 
      facet_wrap("s", ncol = 1) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = '.'),
                         breaks = c(0.5, 1), name = paste0("Share of ", short_names_variables(variable)), 
                         expand = expansion(add = c(0, 0))) +
      add_common_layout(fontsize) + 
      add_pft_scale()) 
  
  return(p)

}

plot_1B = function(variable, fontsize) {
  df = read_csv(paste0("data/final/final_vegetationB_", variable, ".csv"), show_col_types = FALSE) %>%
    mutate(s = long_names_scenarios_twolines(s),
           pft = long_names_pfts(pft))
  
  df$d = factor(df$d, levels = c("0.003", "0.01", "0.04",  "0.1"))
  df$s = factor(df$s, levels = c("Control", "SSP1-\nRCP2.6", "SSP3-\nRCP7.0", "SSP5-\nRCP8.5"))
  df$pft = factor(df$pft, levels = c("Pioneering broadleaf", "Temperate broadleaf", 
                                     "Needleleaf evergreen", "Needleleaf summergreen", "Tundra", "Bare soil"))
  
  (p = ggplot() + 
      geom_bar(data = df, aes(x = d, fill = pft, y = mean), stat = 'identity', position = 'stack', color = "black", linewidth = .15) +
      geom_errorbar(data = df, aes(x = d, ymin = total_mean  - total_sd, ymax = total_mean + total_sd), width=0.4, colour="grey20", alpha=0.9, linewidth = .5) +
      facet_wrap(~s, ncol = 4) + 
      scale_y_continuous(name = long_names_variables(variable)) + 
      scale_x_discrete(name = "Disturbance probability") + 
      add_pft_scale() + 
      add_common_layout(fontsize) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust= 1),
            panel.grid.major.y = element_line(color = "grey80", size = .2),
            panel.grid.minor.y = element_line(color = "grey80", size = .2)))
  
  return(p)
}

plot_1C = function(variable, fontsize) {
  
  df1 = st_read(paste0("data/final/shp/vegetation_ssp585_d0.003_", variable, "_30years2100.shp")) %>%
    st_transform(., crs = 3408) 
  
  df2 = st_read(paste0("data/final/shp/vegetation_picontrol_d0.04_", variable, "_30years2100.shp")) %>%
    st_transform(., crs = 3408) 
  
  df3 = st_read(paste0("data/final/shp/vegetation_ssp585_d0.04_", variable, "_30years2100.shp")) %>%
    st_transform(., crs = 3408) 
  
  df = purrr::reduce(list(df1, df2, df3), bind_rows) %>%
    mutate(dominant_pft = long_names_pfts(dmnnt_p),
           c = paste0(long_names_scenarios(s), "/", d))
  
  df$c = factor(df$c, levels = c("SSP5-RCP8.5/0.003", "Control/0.04", "SSP5-RCP8.5/0.04"))
  
  load_basemap(epsg = "EPSG:3408")   

  (p = ggplot() + 
    add_basemap() +
    geom_sf(data = df, aes(fill = dominant_pft), color = NA) + 
    facet_wrap(~c, nrow = 1) +
    coord_sf(ylim = c(-3200000, 3600000), xlim = c(-4100000, 3900000)) + 
    add_pft_scale() +
    add_common_layout(fontsize) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank()))
 
return(p)
}


plot_1 = function(variable, fontsize) {
  
  p1A = plot_1B(variable, fontsize)
  p1B = plot_1B("cmass", fontsize)
  p1C = plot_1C(variable, fontsize)
  legend = get_legend(p1B + theme(legend.direction = "horizontal",
                                  legend.text = element_text(size = fontsize),
                                  legend.spacing.x = unit(.7, "cm"),
                                  legend.text.align = 0,
                                  legend.margin = margin(r = 0.25, unit = "cm")))
  
  (p1 = plot_grid(plot_grid(p1A + theme(legend.position = "None"),
                            p1B + theme(legend.position = "None"),
                            ncol = 2, labels = c("A", "B")),
                 p1C + theme(legend.position = "None"),
                 legend,
                 labels = c("", "C", ""),
                 ncol = 1, rel_heights = c(1.2, 1, .15)))
  
  ggsave(paste0("figures/figure_vegetation_", variable, ".png"), width = 9, height = 7, dpi = 300)
  ggsave(paste0("figures/figure_vegetation_", variable, ".pdf"), width = 9, height = 7, dpi = 300)
}

plot_1("fpc", 15)




