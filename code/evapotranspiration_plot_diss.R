library(tidyverse)
library(scico)
library(cowplot)
library(rnaturalearth)
library(sf)

source("code/utils_visuals.R")

theme_set(
  theme_classic() + 
    theme(
      axis.text = element_text(color = "black", size = 15),
      axis.title = element_text(color = "black", size = 15),
      plot.title = element_text(color = "black", size = 15),
      plot.subtitle = element_text(color = "black", size = 15),
      plot.caption = element_text(color = "black", size = 15),
      strip.text = element_text(color = "black", size = 15),
      legend.text = element_text(color = "black", size = 15),
      legend.title = element_text(color = "black", size = 15),
      axis.line = element_line(color = "black"),
      panel.grid.major.y = element_line(color = "grey80", linewidth = 0.25),
      legend.background = element_rect(fill='transparent', color = NA),
      legend.box.background = element_rect(fill='transparent', color = NA),
      panel.background = element_rect(fill = "transparent", colour = NA),  
      plot.background = element_rect(fill = "transparent", colour = NA),
      strip.background = element_rect(fill = "transparent", color = NA)
    )
)

et_A = function() {
  df = read_csv("data/final/final_evapotransA.csv", show_col_types = F)  %>%
    mutate(s = long_names_scenarios(s)) %>%
    group_by(month, s, d) %>%
    summarize(mean = mean(aet_mean, na.rn = T),
              min = min(aet_mean, na.rm = T),
              max = max(aet_mean, na.rm = T))
  
  (p = ggplot() + 
      geom_rect(aes(xmin = 2.75, xmax = 5.25, ymin = -Inf, ymax = Inf), fill = "grey80", color = NA, alpha = .5) + 
      geom_ribbon(data = df, aes(x = month, ymin =min, ymax = max, fill = s, group = interaction(s, d)), alpha = .2) +
      geom_line(data = df, aes(x = month, y = mean, color = s, group = interaction(s, d), linetype = as.factor(d)), linewidth = 1) +
      add_color_scenarios() +
      add_fill_scenarios() +
      scale_x_discrete(name = "Month", 
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"),
                       limits = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"),
                     expand = c(0,0)) + 
      scale_y_continuous(name = expression("ET in mm month"^"-1"),  expand = c(0, 0)) +
      theme(legend.direction = "horizontal",
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

et_B = function() {
  df = read_csv("data/final/final_evapotransB.csv", show_col_types = F)  %>%
    mutate(s = long_names_scenarios(s))
  
  (p = ggplot(data = df, aes(x = as.factor(d), fill = s, group = s)) + 
      geom_bar(data = df, aes(y = aet_mean_30years),  stat = 'identity', position = position_dodge(), color = "black", linewidth = 0.05) +
      geom_errorbar(data = df, aes(ymin = aet_mean_30years - aet_sd_30years, 
                                   ymax = aet_mean_30years + aet_sd_30years), 
                    colour="grey20", alpha=0.9, size=0.7, position = position_dodge(.9), width = .4) +
      scale_x_discrete(name = "Disturbance probability", expand = c(0,0)) +
      scale_y_continuous(name = expression("ET (MAM) in mm month"^"-1"), limits = c(0, 31), expand = c(0,0)) +
      add_fill_scenarios())
}

et_C = function(endpoint) {
  
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
      geom_point(data = df_s, aes(geometry = geometry), stat = "sf_coordinates", size = 0.001, fill = "black",  pch = 21, alpha = 1, stroke = 0) +
      add_fill_anomaly(name = expression("ET (MAM) in mm month"^"-1"), endpoint = 30) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      facet_wrap(~c, ncol = 3) + 
      theme(legend.position = "bottom",
            legend.direction = "horizontal",
            axis.title = element_blank(),
            legend.title = element_text(margin = margin(r = 20))))
  
}

et_C_png = function(endpoint) {
  
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
      geom_point(data = df_s, aes(geometry = geometry), stat = "sf_coordinates", size = 0.3, fill = "black",  pch = 21, alpha = 1, stroke = 0) +
      add_fill_anomaly(name = expression("ET (MAM) in mm month"^"-1"), endpoint = 30) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      facet_wrap(~c, ncol = 3) + 
      theme(legend.position = "bottom",
            legend.direction = "horizontal",
            axis.title = element_blank(),
            legend.title = element_text(margin = margin(r = 20))))
  
}

plot_et = function() {
  pA = et_A() +
    theme(legend.position = "right")
  pB = et_B()
  legend = get_legend(pA)
  pC = et_C()
  
  
  plot_grid(plot_grid(pA + theme(legend.position = "None"),
                      pB + theme(legend.position = "None"), 
                      nrow = 1, labels = c("(a)", "(b)"),
                      align = "hv", axis = "lr"),
            legend,
            pC,
            ncol = 1, rel_heights = c(1, .2, 1), 
            labels = c("", "", "(c)"), align = "hv", axis = "lr")
  
  ggsave("figures/figure_evapotrans_diss.pdf", width = 8, height = 7, scale = 1.2)
  
  pC = et_C_png()
  
  
  plot_grid(plot_grid(pA + theme(legend.position = "None"),
                      pB + theme(legend.position = "None"), 
                      nrow = 1, labels = c("(a)", "(b)"),
                      align = "hv", axis = "lr"),
            legend,
            pC,
            ncol = 1, rel_heights = c(1, .2, 1), 
            labels = c("", "", "(c)"), align = "hv", axis = "lr")
  
  ggsave("figures/figure_evapotrans_diss.png", width = 8, height = 7, scale = 1.2, dpi = 600)
}

plot_et()


et_A_appendix = function(fontsize) {
  df = read_csv("data/final/final_evapotrans_appendix.csv", show_col_types = F)  %>%
    mutate(s = long_names_scenarios(s)) %>%
    rename(Interception = interception_mean,
           Runoff = runoff_mean,
           `Soil evaporation` = soil_evaporation_mean,
           Transpiration = transpiration_mean) %>%
    pivot_longer(cols = c(Interception, Runoff, `Soil evaporation`, Transpiration), 
                 names_to = "component", values_to = "mean_30years")  %>%
    pivot_wider(values_from = mean_30years, names_from = d) %>%
    mutate(diff = `0.04` - `0.003`) %>%
    group_by(month, component, s) %>%
    summarize(across(diff, 
                     list(mean = ~round(mean(.x, na.rm = TRUE), digits = 4), 
                          min = ~round(min(.x, na.rm = TRUE), digits = 4), max = ~round(max(.x, na.rm = TRUE), digits = 4)),
                     .names = "{.col}_{.fn}"))
  
  (p = ggplot() + 
      geom_ribbon(data = df, aes(x = month, ymin = diff_min, ymax = diff_max, fill = s, group = s), alpha = .2) +
      geom_line(data = df, aes(x = month, y = diff_mean, color = s, group = s), linewidth = 1) +
      facet_wrap(~component, nrow = 1) +
      add_color_scenarios() +
      add_fill_scenarios() +
      scale_x_discrete(name = "Month", 
                       labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"),
                       limits = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"),
                       expand = c(0,0)) + 
      scale_y_continuous(name = expression("Difference in mm month"^"-1"),  expand = c(0, 0)) +
      theme(legend.direction = "horizontal",
            legend.position = "bottom",
            legend.box = 'vertical',
            legend.box.just = 'left',
            legend.spacing.y = unit(0, "cm"),
            legend.title = element_text(margin = margin(r = 40)),
            axis.text.x = element_text(hjust = 1)) +
      guides(linetype = guide_legend(title="Disturbance probability",
                                     title.position = "left"),
             color = guide_legend(title="Climate scenario",
                                  title.position = "left")))
  
  return(p)
}

et_A_appendix(15)
ggsave("figures/figure_evapotrans_diss_appendix.pdf", width = 10, height = 5, scale = 0.9)

          

