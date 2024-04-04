library(tidyverse)
library(cowplot)

source("code/utils_visuals.R")

variable = "fpc"
column_of_interest = "total"


plot_2 = function(fontsize) {
  df = read.csv(paste0("data/final/final_factors.csv")) %>%
    mutate(variable = long_names_attribution(variable))
  
  df$variable = factor(df$variable, levels = c("Total plant \ncover in %", "Needleleaf \ncover in %", 
                                               "Tundra \ncover in %", "Broadleaf \ncover in %",
                                               "Albedo \n(DJF)", "ET (MAM) in \nmm/month"))
  
  df$factor = factor(df$factor, levels = c("D_d", "D_s", "D_x", "D_sd"))

  (p = ggplot() + 
      geom_rect(data = df, aes(xmin=2015, xmax=2100, ymin=-Inf, ymax=Inf), fill="grey90", alpha=0.1) + 
      geom_hline(yintercept = 0, color = "grey") +
      geom_line(data = df, aes(x = year, y = effect, color = factor, size = type, alpha = type)) +
      facet_wrap(~variable, scales = "free", ncol = 1, strip.position="left") +
      scale_x_continuous(name = "Year", breaks = c(2015, 2100, 2300, 2500)) +
      scale_y_continuous(name = NULL) +
      add_scale_factors() +
      geom_point(data = df[df$type == "none", ], aes(x = year, y = effect, color = factor), pch = 15, size = 5) + # trick to get reasonable size legend keys
      scale_size_manual(values = c("effect_global" = .4, "effect_smoothed" = .8), guide = "none") +
      scale_alpha_manual(values = c("effect_global" = 1, "effect_smoothed" = 1), guide = "none") +
      add_common_layout(fontsize) +
      theme(strip.placement = "outside",
            legend.margin = margin(0, 0, 0, -2.5, "cm"),
            legend.text.align = 0,
            legend.position = "bottom",
            legend.direction = "horizontal"))
  
  ggsave("figures/figure_factors.png", width = 7, height = 9.25, dpi = 300)
  ggsave("figures/figure_factors.pdf", width = 7, height = 9.25, dpi = 300)
}

plot_2(fontsize = 14)