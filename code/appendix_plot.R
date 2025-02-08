library(tidyverse)
library(scico)
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

get_data_snow = function() {
  
  data = list()
  
  for (s in c("picontrol", "ssp585", "ssp126", "ssp370")) {
    
    df = read.table(paste0("data/subset/", s, "_d0.003333333_msnowdepth_30years2100.out"), header = T) %>%
      pivot_longer(cols = -c(Lon, Lat, Year), values_to = "height", names_to = "month") %>%
      mutate(s = long_names_scenarios(s),
             month = tolower(month),
             fraction = height/(0.1 + height))
    
    data = append(data, list(df))
    
  }
  
  df = purrr::reduce(data, bind_rows) %>%
    group_by(Year, month, s) %>%
    summarize(fraction = mean(fraction),
              height = mean(height)) %>%
    ungroup() %>%
    group_by(month, s) %>%
    summarize(mean = mean(fraction),
              min = min(fraction),
              max = max(fraction)) 
  
}

df_snow = get_data_snow()

df_pr = read_csv("data/ext/precipitation_studyregion_2085_2100_mmmonth.csv")

order_months = labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
df_pr$month = factor(df_pr$month, c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))
df_snow$month = factor(df_snow$month, c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))

(p1 = ggplot() + 
    geom_ribbon(data = df_snow, aes(x = month, ymin = min, ymax = max, fill = s, group = interaction(s)), alpha = .2) +
    geom_line(data = df_snow, aes(x = month, y = mean, color = s, group = interaction(s)), linewidth = 1) +
    scale_color_manual(values = c("Control" = "#DFB793", "SSP5-RCP8.5" = "#190C65", "SSP1-RCP2.6" = "#4483A6", "SSP3-RCP7.0" = "#274E92"),
                       labels = c("Control", "SSP1-RCP2.6", "SSP3-RCP7.0", "SSP5-RCP8.5"), name = "Scenario") +
    scale_fill_manual(values = c("Control" = "#DFB793", "SSP5-RCP8.5" = "#190C65", "SSP1-RCP2.6" = "#4483A6", "SSP3-RCP7.0" = "#274E92"),
                      labels = c("Control", "SSP1-RCP2.6", "SSP3-RCP7.0", "SSP5-RCP8.5"), name = "Scenario") +
    scale_x_discrete(labels = order_months, name = "Month of the year", expand = c(0,0)) + 
    scale_y_continuous(expand = c(0.01,0), name = expression("Snow fraction"~chi[Snow])) +
    theme(legend.position = c(0.65, 0.5),
          legend.direction = "vertical") +
    guides(color = guide_legend(title.position="top", 
                                title="Climate scenario\n"),
           fill = guide_legend(title.position="top", 
                                title="Climate scenario\n")))



(p2 = ggplot() +
    geom_ribbon(data = df_pr, aes(x = month, ymin = min, ymax = max, fill = s, group = interaction(s)), alpha = .2) +
    geom_line(data = df_pr, aes(x = month, y = mean, color = s, group = interaction(s)), linewidth = 1) +
    scale_color_manual(values = c("Control" = "#DFB793", "SSP5-RCP8.5" = "#190C65", "SSP1-RCP2.6" = "#4483A6", "SSP3-RCP7.0" = "#274E92"),
                       labels = c("Control", "Historical", "SSP1-RCP2.6", "SSP3-RCP7.0", "SSP5-RCP8.5"), name = "Scenario") +
    scale_fill_manual(values = c("Control" = "#DFB793", "SSP5-RCP8.5" = "#190C65", "SSP1-RCP2.6" = "#4483A6", "SSP3-RCP7.0" = "#274E92"),
                      labels = c("Control", "Historical", "SSP1-RCP2.6", "SSP3-RCP7.0", "SSP5-RCP8.5"), name = "Scenario") +
    scale_x_discrete(labels = order_months, name = "Month of the year", expand = c(0,0)) + 
    scale_y_continuous(expand = c(0.01,0), name = expression("Precipitation in mm month"^"-1")) +
    theme(legend.position = c(0.65, 0.25),
          legend.direction = "vertical") +
    guides(color = guide_legend(title.position="top", 
                                title="Climate scenario\n"),
           fill = guide_legend(title.position="top", 
                                title="Climate scenario\n")))

plot_grid(p1, p2, nrow = 1, labels = c("(a)", "(b)"))
ggsave("figures/figure_snow_precipitation_diss.pdf", height = 4, width = 10, scale = 1)
ggsave("figures/figure_snow_precipitation_diss.png", height = 4, width = 10, scale = 1)
