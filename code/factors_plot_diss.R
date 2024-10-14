library(tidyverse)
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

variable = "fpc"
column_of_interest = "total"


plot_2 = function(climate_scenario, disturbance_regime) {
  df = read.csv(paste0("data/final/final_factors_", climate_scenario, "_", disturbance_regime, ".csv")) %>%
    mutate(variable = long_names_attribution(variable))
  
  df$factor = factor(df$factor, levels = c("D_d", "D_s", "D_x", "D_sd"))
  
  panel_labels = data.frame(
    variable = c("Total plant \ncover", "Needleleaf \ncover",  "Albedo \n(DJF)", 
                 "Non-tree \ncover", "ET (JJA) in \nmm/month" ,  "Broadleaf \ncover"),
    label = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),  # Adjust these as per the number of facets
    x = 1850,   # Position to place the label on the x-axis
    y = Inf     # Position to place the label on the y-axis (Inf means the top of the panel)
  )
  
  df$variable = factor(df$variable, levels = c("Total plant \ncover", "Needleleaf \ncover",  "Albedo \n(DJF)", 
                                               "Non-tree \ncover", "ET (JJA) in \nmm/month" ,  "Broadleaf \ncover"
  ))
  
  panel_labels$variable = factor(panel_labels$variable, levels = c("Total plant \ncover", "Needleleaf \ncover",  "Albedo \n(DJF)", 
                                                                   "Non-tree \ncover", "ET (JJA) in \nmm/month" ,  "Broadleaf \ncover"
  ))
  
  

  (p = ggplot() + 
      geom_rect(data = df, aes(xmin=2015, xmax=2100, ymin=-Inf, ymax=Inf), fill="grey90") + 
      geom_hline(yintercept = 0, color = "grey") +
      geom_line(data = df, aes(x = year, y = effect, color = factor, size = type, alpha = type)) +
      facet_wrap(~variable, scales = "free", ncol = 2, strip.position="left") +
      scale_x_continuous(name = "Year", breaks = c(2015, 2100, 2300, 2500), expand = c(0,0)) +
      scale_y_continuous(name = NULL, expand = c(0,0)) +
      scale_color_manual(values = c("D_sd" = "grey20", "D_d" = "#8ead8a", "D_s" = "#4483A6",  "D_x" = "#e9c9aa"),
                         labels = c("D_sd" = expression(Total~effect~Delta[~~SD]), "D_d" = expression(Disturbance~Delta[~~D]), "D_s" = expression(Climate~Delta[~~S]),  "D_x" = expression(Interaction~Delta[~~X])),
                         name = "Driver") +
      geom_point(data = df[df$type == "none", ], aes(x = year, y = effect, color = factor), pch = 15, size = 5) + # trick to get reasonable size legend keys
      scale_size_manual(values = c("effect_global" = .4, "effect_smoothed" = .8), guide = "none") +
      scale_alpha_manual(values = c("effect_global" = 1, "effect_smoothed" = 1), guide = "none") +
      geom_text(data = panel_labels, aes(x = x, y = y, label = label), 
                hjust = 0, vjust = 1.5, size = 5, fontface = "bold", inherit.aes = FALSE) +
      theme(strip.placement = "outside",
            legend.margin = margin(0, 0, 0, -2.5, "cm"),
            legend.text.align = 0,
            legend.position = "bottom",
            legend.direction = "horizontal"))
  
  ggsave(paste0("figures/figure_factors_diss_", climate_scenario, "_", disturbance_regime, ".pdf"), width = 8, height = 6, scale = 1.25)
} 

plot_2("ssp585", "0.04")
plot_2("ssp126", "0.1")
