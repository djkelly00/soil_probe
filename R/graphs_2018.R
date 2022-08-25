#######################################################
################# 2018 graphs ##########################
#######################################################

wt18 <- read.csv("C:/Users/jessh/Documents/GitHub/soil_probe/processed_data/data2018_21_jun_2022.csv", header = TRUE)

wt18$date.time <- as.POSIXct(wt18$date.time, tz = "America/New_York", "%Y-%m-%d %H:%M:%S")
wt18$date <- as.POSIXct(wt18$date, "%Y-%m-%d")


library(ggplot2)
### Pressure observed by divers
g0 <- ggplot(wt18, aes(x = date.time, y = WaterPressure_cmH2O, colour = well)) +
  geom_point(show.legend = F, size = 0.5) +
  facet_grid(well ~., scales = "free_y") +
  ylab("Pressure (cmH2O)") + xlab("Date") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d%b%y") +
  theme(axis.text.x = element_text(size = 10, face = "plain", angle = 90)) +
  ggtitle("2018:  Pressure observed by Divers at SERC ForestGEO plot")
ggsave(file.path("C:/Users/jessh/Documents/GitHub/soil_probe/figures/2018_Pressure_observed_by_Divers.jpeg"), plot = g0, height = 7, width = 12, units='in')


ggplot(wt18, aes(x = date, y = GWL_m)) +
  geom_point(aes(group = well, color = well)) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d%b%y") +
  theme(axis.text.x = element_text(size = 10, face = "plain", angle = 90))

#######################################################
################# Not yet working #####################
## plotting watertable
p1 <- ggplot(aes(x = date.time, y = GWL_m, colour = well)) +
  geom_point(size = 0.1) +
  ylab("Ground Water Level (masl)") + xlab("Date") +
  scale_color_discrete("Well") +
  ggtitle("Ground Water Levels at SERC ForestGEO plot") +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  guides(colour = guide_legend(override.aes = list(size = 6))) +
  theme(legend.position = c(0.55, 0.6), legend.background = element_rect(fill="transparent")) +
  theme(axis.text.y = element_text(size = 12, face = "plain")) +
  scale_y_continuous(limits = c(-2, 10)) +
  theme(axis.text.x = element_text(size = 12, face = "plain", angle = 90, margin = margin(b = 2)))
p1.1 <- p1 + scale_x_datetime(date_breaks = "2 months", date_labels = "%d%b%y")
ggsave(file.path("figures/Ground Water Level by wells_single_panel.jpeg"), plot = p1.1, height = 4, width = 6, units='in')
ggsave(file.path("figures/Ground Water Level by wells_single_panel.pdf"), plot = p1.1, height = 4, width = 6, units='in')
p1.2 <- p1 + geom_point(aes(y = GWL_m_manual, fill = ele.probe), size = 3, shape = 24, alpha = 1, color = "black", show.legend = F)
ggsave(file.path("figures/Ground Water Level by wells_single_panel_with_OBS.jpeg"),  plot = p1.2, height = 5, width = 7.5, units='in')
p1.3 <- p1.2 +  scale_x_datetime(limits = c(max(watertable$date.time, na.rm = T) - 1*30*24*60*60, max(watertable$date.time, na.rm = T)),
                                 date_breaks = "2 days", date_labels = "%d%b%y") +
  theme(legend.position = c(0.8, 0.6), legend.background = element_rect(fill="transparent"))
ggsave(file.path("figures/Ground Water Level by wells_single_panel_with_OBS_last month.jpeg"),  plot = p1.3, height = 5, width = 7.5, units='in')
p1.4 <- p1.2 +  scale_x_datetime(limits = c(as.POSIXct("2020-03-02"), as.POSIXct("2020-03-02") + 3*24*60*60),
                                 date_breaks = "1 day", date_labels = "%d%b%y") +
  theme(legend.position = "top", legend.background = element_rect(fill="transparent"))
ggsave(file.path("figures/Ground Water Level by wells_single_panel_with_OBS_measurements.jpeg"),  plot = p1.4, height = 5, width = 7.5, units='in')






