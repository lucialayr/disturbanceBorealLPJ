library(tidyverse)
library(purrr)
library(sf)
library(rnaturalearth)
library(cowplot)

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
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = '.'),
                         breaks = c(0.5, 1), name = paste0("Share of ", short_names_variables(variable)), 
                         expand = c(0,0)) +
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
                                     "Needleleaf evergreen", "Needleleaf summergreen", "Non-tree V.", "Bare soil"))
  
  if (variable == 'fpc') {
    df = df %>%
      mutate(mean = mean*100,
             total_mean = total_mean*100)
  }
  
  (p = ggplot() + 
      geom_bar(data = df, aes(x = d, fill = pft, y = mean), stat = 'identity', position = 'stack', color = "black", linewidth = .15) +
      geom_errorbar(data = df, aes(x = d, ymin = total_mean  - total_sd, ymax = total_mean + total_sd), width=0.4, colour="grey20", alpha=0.9, linewidth = .5) +
      facet_wrap(~s, ncol = 4) + 
      scale_y_continuous(name = long_names_variables(variable), expand = c(0,0)) + 
      scale_x_discrete(name = "Disturbance probability") + 
      add_pft_scale() + 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust= 1)))
  
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
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
    add_pft_scale())
 
return(p)
}

plot_1 = function(variable, fontsize) {
  
  (p1A = plot_1B(variable))
  (p1B = plot_1B("cmass"))
  p1C = plot_1C(variable)
  legend = get_legend(p1B + theme(legend.direction = "horizontal",
                                  legend.spacing.x = unit(.7, "cm"),
                                  legend.text.align = 0,
                                  legend.margin = margin(r = 0.25, b = 0.5, unit = "cm")))
  
  (p1 = plot_grid(plot_grid(p1A + theme(legend.position = "None"),
                            p1B + theme(legend.position = "None"),
                            ncol = 2, labels = c("(a)", "(b)")),
                 p1C + theme(legend.position = "None"),
                 legend,
                 labels = c("", "(c)", ""),
                 ncol = 1, rel_heights = c(1.2, 1, .15)))
  ggsave(paste0("figures/figure_vegetation_", variable, "_diss.pdf"), width = 9, height = 7, scale = 1.1)
  ggsave(paste0("figures/figure_vegetation_", variable, "_diss.png"), width = 9, height = 7, scale = 1.1)
}

plot_1("fpc", 14)

plot_1C_appendix = function(variable, fontsize) {
  
  data = list()
  
  for (s in c("picontrol", "ssp126", "ssp370", "ssp585")) {
    for (d in c("0.04", "0.003", "0.1", "0.01")) {
      
      df1 = st_read(paste0("data/final/shp/vegetation_", s, "_d", d, "_", variable, "_30years2100.shp")) %>%
        st_transform(., crs = 3408) 
      
      data = append(data, list(df1))
    }
  }

  
  df = purrr::reduce(data, bind_rows) %>%
    mutate(dominant_pft = long_names_pfts(dmnnt_p),
           c = paste0(long_names_scenarios(s), "/", d))
  
  load_basemap(epsg = "EPSG:3408")   
  
  (p = ggplot() + 
      add_basemap() +
      geom_sf(data = df, aes(fill = dominant_pft), color = NA) + 
      facet_wrap(~c, nrow = 4, dir = "v") +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      add_pft_scale() +
      theme(strip.text = element_text(size = fontsize -1 ),
            legend.position = "bottom",
            legend.direction = "horizontal") +
      guides(fill = guide_legend(title.position = "top",
                                 nrow = 2)))
  
  return(p)
}

plot_1C_appendix("fpc", 15)

ggsave("figures/figure_vegetation_fpc_diss_appendix.pdf", width = 9, height = 9, scale = 1.1)
ggsave("figures/figure_vegetation_fpc_diss_appendix.png", width = 9, height = 9, scale = 1.1)
