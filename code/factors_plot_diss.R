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


plot_2 = function() {
  df = read.csv(paste0("data/final/final_factors.csv")) %>%
    mutate(variable = long_names_attribution(variable))
  
  df$variable = factor(df$variable, levels = c("Total plant \ncover in %", "Needleleaf \ncover in %",  "Albedo \n(DJF)", 
                                               "Non-tree \ncover in %", "ET (MAM) in \nmm/month" ,  "Broadleaf \ncover in %"
                                               ))
  
  df$factor = factor(df$factor, levels = c("D_d", "D_s", "D_x", "D_sd"))

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
      theme(strip.placement = "outside",
            legend.margin = margin(0, 0, 0, -2.5, "cm"),
            legend.text.align = 0,
            legend.position = "bottom",
            legend.direction = "horizontal"))
  
  ggsave("figures/figure_factors_diss.pdf", width = 8, height = 6, scale = 1.25)
} 

plot_2()
