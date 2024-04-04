library(tidyverse)
library(scico)
library(cowplot)
library(sf)
library(rnaturalearth)


source("code/utils_visuals.R")


albedo_A = function(fontsize) {
  df = read_csv("data/final/final_albedoA.csv", show_col_types = F)  %>%
    mutate(s = long_names_scenarios(s))
  
  (p = ggplot() + 
      geom_point(data = df[df$year == 2015, ], aes(x = month, y = albedo_mean_30years, color = s), 
                 pch = 15, size = 5) +
      geom_vline(xintercept = "dec", color = "grey80",  alpha = .4, linewidth = 6) + 
      geom_rect(aes(xmin = 0.75, xmax = 2.25  , ymin = -Inf, ymax = Inf), fill = "grey80", color = NA, alpha = .4) + 
      geom_line(data = df, aes(x = month, y = albedo_mean, color = s, linetype = as.factor(d), group = interaction(s,d, year)), 
                linewidth = .05, alpha = .5) +
      geom_line(data = df, aes(x = month, y = albedo_mean_30years, color = s, linetype = as.factor(d), group = interaction(s,d)), 
                linewidth = 1.2, alpha = .9) +
      add_color_scenarios() +
      scale_x_discrete(name = "Month", 
                       labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"),
                       limits = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")) + 
      scale_y_continuous(name = "Albedo", limits = c(0.1, 0.45), expand = c(0, 0)) +
      add_common_layout(fontsize) +
      theme(legend.direction = "horizontal",
            panel.grid.major.y = element_line(color = "grey80", size = .2),
            panel.grid.minor.y = element_line(color = "grey80", size = .2),
            legend.position = "bottom",
            legend.box = 'vertical',
            legend.box.just = 'left',
            legend.spacing.y = unit(-0.25, "cm"),
            legend.title = element_text(margin = margin(r = 30))) +
      guides(linetype = guide_legend(title="Disturbance probability",
                                     title.position = "left"),
             color = guide_legend(title="Climate scenario",
                                  title.position = "left")))
  
  return(p)
}

albedo_B = function(fontsize) {
  df = read_csv("data/final/final_albedoB.csv", show_col_types = F)  %>%
    mutate(s = long_names_scenarios(s))
  
  (p = ggplot(data = df, aes(x = as.factor(d), fill = s, group = s)) + 
      geom_bar(data = df, aes(y = albedo_mean_30years),  stat = 'identity', position = position_dodge(), color = "black", linewidth = .05) +
      geom_errorbar(data = df, aes(ymin = albedo_mean_30years - albedo_sd_30years, 
                                   ymax = albedo_mean_30years + albedo_sd_30years), 
                    colour="grey20", alpha=0.9, size=0.7, position = position_dodge(.9), width = .4) +
      scale_x_discrete(name = "Disturbance probability") +
      scale_y_continuous(name = "Albedo (DJF)", limits = c(0, 0.45), expand = c(0,0)) +
      coord_cartesian(ylim = c(0.1, 0.45)) + 
      add_fill_scenarios() +
      add_common_layout(fontsize) +
      theme(panel.grid.major.y = element_line(color = "grey80", size = .2),
            panel.grid.minor.y = element_line(color = "grey80", size = .2)))
}

albedo_C = function(fontsize) {
  
  df1 = st_read(paste0("data/final/shp/albedo_anomaly_ssp585_d0.003_albedo_30years2100.shp")) %>%
    st_transform(., crs = 3408) 
  
  df2 = st_read(paste0("data/final/shp/albedo_anomaly_picontrol_d0.04_albedo_30years2100.shp")) %>%
    st_transform(., crs = 3408) 
  
  df3 = st_read(paste0("data/final/shp/albedo_anomaly_ssp585_d0.04_albedo_30years2100.shp")) %>%
    st_transform(., crs = 3408) 
  
  df = purrr::reduce(list(df1, df2, df3), bind_rows) %>%
    mutate(albedo = al___30,
           c = paste0(long_names_scenarios(s), "/", d))
  
  df1_s = st_read(paste0("data/final/shp/albedo_significance_ssp585_d0.003_albedo_30years2100.shp")) %>%
    st_transform(., crs = 3408) 
  
  df2_s = st_read(paste0("data/final/shp/albedo_significance_picontrol_d0.04_albedo_30years2100.shp")) %>%
    st_transform(., crs = 3408) 
  
  df3_s = st_read(paste0("data/final/shp/albedo_significance_ssp585_d0.04_albedo_30years2100.shp")) %>%
    st_transform(., crs = 3408) 
  
  df_s = purrr::reduce(list(df1_s, df2_s, df3_s), bind_rows) %>%
    rename(significance = sgnfcnt) %>%
    mutate(c = paste0(long_names_scenarios(s), "/", d),
           centroids = st_centroid(geometry)) %>%
    select(-geometry) %>%
    filter(significance == 0) 
  
  df$c = factor(df$c, levels = c("SSP5-RCP8.5/0.003", "Control/0.04", "SSP5-RCP8.5/0.04")) 
  df_s$c = factor(df_s$c, levels = c("SSP5-RCP8.5/0.003", "Control/0.04", "SSP5-RCP8.5/0.04"))
  
  load_basemap()
  
  (p = ggplot() +
      add_basemap() + 
      geom_sf(data = df, aes(fill = albedo), color = NA) +
      geom_point(data = df_s, aes(geometry = geometry), stat = "sf_coordinates", size = 0.0001, fill = "grey90",  pch = 21, alpha = .4, stroke = 0) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      add_fill_anomaly(name = "Albedo (DJF)", endpoint = .3) +
      facet_wrap(~c, ncol = 3) + 
      add_common_layout(fontsize) +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.title = element_text(margin = margin(r = 20))))
  
}

plot_albedo = function(fontsize) {
  pA = albedo_A(fontsize)
  pB = albedo_B(fontsize)
  legend = get_legend(pA)
  pC = albedo_C(fontsize)
  
  
  plot_grid(plot_grid(pA + theme(legend.position = "None"),
                      pB + theme(legend.position = "None"), 
                      nrow = 1, labels = c("(a)", "(b)")),
            legend,
            pC,
            ncol = 1, rel_heights = c(1, .2, 1),
            labels = c("", "", "(c)"))
  
  
  ggsave("figures/figure_albedo.png", width = 8, height = 8, dpi = 300)
  ggsave("figures/figure_albedo.pdf", width = 8, height = 8, dpi = 300)
}

plot_albedo(14)

