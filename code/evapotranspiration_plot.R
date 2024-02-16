library(tidyverse)
library(scico)
library(cowplot)
library(rnaturalearth)
library(sf)

source("code/utils_visuals.R")

et_A = function(fontsize) {
  df = read_csv("data/final/final_evapotransA.csv", show_col_types = F)  %>%
    mutate(s = long_names_scenarios(s))
  
  (p = ggplot() + 
      geom_point(data = df[df$year == 2015, ], aes(x = month, y = aet_mean_30years, color = s), 
                 pch = 15, size = 5) +
      geom_rect(aes(xmin = 2.75, xmax = 5.25, ymin = -Inf, ymax = Inf), fill = "grey80", color = NA, alpha = .4) + 
      geom_line(data = df, aes(x = month, y = aet_mean, color = s, linetype = as.factor(d), group = interaction(s,d, year)), 
                linewidth = .05, alpha = .5) +
      geom_line(data = df, aes(x = month, y = aet_mean_30years, color = s, linetype = as.factor(d), group = interaction(s,d)), 
                linewidth = 1.2, alpha = .9) +
      add_color_scenarios() +
      scale_x_discrete(name = "Month", 
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"),
                       limits = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")) + 
      scale_y_continuous(name = "ET in mm/month",  expand = c(0, 0)) +
      add_common_layout(fontsize) +
      theme(legend.direction = "horizontal",
            panel.grid.major.y = element_line(color = "grey80", size = .2),
            panel.grid.minor.y = element_line(color = "grey80", size = .2),
            legend.position = "bottom",
            legend.box = 'vertical',
            legend.box.just = 'left',
            legend.spacing.y = unit(0, "cm"),
            legend.title = element_text(margin = margin(r = 40))) +
      guides(linetype = guide_legend(title="Disturbance probability",
                                     title.position = "left"),
             color = guide_legend(title="Climate scenario",
                                  title.position = "left")))
  
  return(p)
}

et_B = function(fontsize) {
  df = read_csv("data/final/final_evapotransB.csv", show_col_types = F)  %>%
    mutate(s = long_names_scenarios(s))
  
  (p = ggplot(data = df, aes(x = as.factor(d), fill = s, group = s)) + 
      geom_bar(data = df, aes(y = aet_mean_30years),  stat = 'identity', position = position_dodge(), color = "black", linewidth = 0.05) +
      geom_errorbar(data = df, aes(ymin = aet_mean_30years - aet_sd_30years, 
                                   ymax = aet_mean_30years + aet_sd_30years), 
                    colour="grey20", alpha=0.9, size=0.7, position = position_dodge(.9), width = .4) +
      scale_x_discrete(name = "Disturbance probability") +
      scale_y_continuous(name = "ET (JJA) in mm/month", limits = c(0, 35), expand = c(0,0)) +
      add_fill_scenarios() +
      add_common_layout(fontsize) +
      theme(panel.grid.major.y = element_line(color = "grey80", size = .2),
            panel.grid.minor.y = element_line(color = "grey80", size = .2)))
}

et_C = function(fontsize, endpoint) {
  
  df1 = st_read(paste0("data/final/shp/evapotrans_anomaly_ssp585_d0.003_met_30years2100.shp")) %>%
    st_transform(., crs = 3408) 
  
  df2 = st_read(paste0("data/final/shp/evapotrans_anomaly_picontrol_d0.04_met_30years2100.shp")) %>%
    st_transform(., crs = 3408) 
  
  df3 = st_read(paste0("data/final/shp/evapotrans_anomaly_ssp585_d0.04_met_30years2100.shp")) %>%
    st_transform(., crs = 3408) 
  
  df = purrr::reduce(list(df1, df2, df3), bind_rows) %>%
    mutate(et = at___30,
           c = paste0(long_names_scenarios(s), "/", d))
  df1_s = st_read(paste0("data/final/shp/evapotrans_significance_ssp585_d0.003_met_30years2100.shp")) %>%
    st_transform(., crs = 3408) 
  
  df2_s = st_read(paste0("data/final/shp/evapotrans_significance_picontrol_d0.04_met_30years2100.shp")) %>%
    st_transform(., crs = 3408) 
  
  df3_s = st_read(paste0("data/final/shp/evapotrans_significance_ssp585_d0.04_met_30years2100.shp")) %>%
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
      geom_sf(data = df, aes(fill = et), color = NA) + 
      geom_point(data = df_s, aes(geometry = geometry), stat = "sf_coordinates", size = 0.0001, fill = "grey40",  pch = 21, alpha = .5, stroke = 0) +
      add_fill_anomaly(name = "ET (MAM) in mm/month", endpoint = 30) +
      facet_wrap(~c, ncol = 3) + 
      coord_sf(ylim = c(-3200000, 3600000), xlim = c(-4100000, 3900000)) + 
      add_common_layout(fontsize) +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.title = element_text(margin = margin(r = 20))))
  
}

plot_et = function(fontsize) {
  pA = et_A(15)
  pB = et_B(15)
  legend = get_legend(pA)
  pC = et_C(15)
  
  
  plot_grid(plot_grid(pA + theme(legend.position = "None"),
                      pB + theme(legend.position = "None"), 
                      nrow = 1, labels = c("A", "B")),
            legend,
            pC,
            ncol = 1, rel_heights = c(1, .2, 1),
            labels = c("", "", "C"))
  
  
  ggsave("figures/figure_evapotrans.png", width = 8, height = 8, dpi = 300)
  ggsave("figures/figure_evapotrans.pdf", width = 8, height = 8, dpi = 300)
}

plot_et(15)


          

