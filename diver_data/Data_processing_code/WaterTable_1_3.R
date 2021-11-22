##-------------------
## Author: Rutuja Chitra-Tarak, Sean McMahon, Jess Shue, and Dominique Kelly
## Date: 6/14/2021
## Title: To compile and plot SERC ForestGEO plot Diver/ water table level data
##-------------------
dgadsfgfaserfg
library(pacman)
library(tidyverse)
library(neonUtilities)
library(scales)
library(cowplot)
library(zoo)
library(readxl)
library(neonUtilities)

rm(list = ls())
if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, mettower, neon, tmon,
               scales, cowplot, soilmoisture, zoo, readxl)

theme_set(theme_bw())
theme_update(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             strip.background = element_blank())



###############  Bring in probe meta-data  #############################
# This should not be necessary to reload after we have the baseline
# data saved. After that, we just ammend.

ele <- readxl::read_excel("soil_moisture_probe_and_diver_details.xlsx",
                sheet = 1)
ele$A.Cable_Length_cm_2018 <- as.numeric(ele$A.Cable_Length_cm_2018)
ele$A.Cable_Length_cm_2020 <- as.numeric(ele$A.Cable_Length_cm_2020)
ele$pipe_height_cm <- ele$`Pipe Height From Ground_cm`
ele$well <- ele$probe
ele$well[nrow(ele)] <- "baro"
## to match with baro files with pattern "bh"
wells <- ele$well; wells[nrow(ele)] <- "bh"
ele$ele.probe <- paste(ele$elevation_meters, " m"," (", ele$well, ")",sep = "")
ele$ele.probe <- factor(ele$ele.probe, levels =
                          c("0.98 m (3a)", "1.7 m (6b)", "5.19 m (5f)", "8 m (4c)",
                            "8.32 m (2d)", "9.55 m (1e)", "8.14 m (NAbaro)"))


### uncompensated files
diver_files <- list.files(".", pattern = "serc_fgeo", ignore.case = TRUE)
total2 = length(diver_files)

pb2 <- txtProgressBar(min = 0, max = total2, style = 3)
for (i in 1:length(diver_files)) {
  new.file <- read.csv(
    diver_files[i],
    skip = 51, ## remove details
    na.strings = c("NA", ""),
    header = T,
    row.names = NULL,
    check.names = F
  )
  # remove last row
  new.file <- new.file[-nrow(new.file),]
  # add well ID
  new.file$well <- ele$well[str_detect(tolower(diver_files[i]), wells)]
  if (i == 1) {
    dat1 <- new.file
  } else {
    dat1 <- rbind.data.frame(dat1, new.file)
  }
  Sys.sleep(0.1)
  # update progress bar
  setTxtProgressBar(pb2, i)
  ## add headers
}
colnames(dat1) <- c("date.time", "WaterPressure_cmH2O", "Temp_C", "well")
dat2 <- dat1
head(dat2)
dat2$date.time <- strptime(dat2$date.time, "%Y/%m/%d %H:%M:%S")

# dat2 <- subset(dat1, !is.na(dat1$date.time))
dat2 <- dat2 %>% arrange(date.time)
dat2 <- dat2 %>% mutate(date.time.well = paste(date.time, well, sep = "_")) %>%
  subset(!duplicated(date.time.well)) %>% arrange(date.time)
diver_neon_dat <- left_join(dat2, select(ele, elevation_meters, well,
                               ele.probe, `Pipe Height From Ground_cm`, A.Cable_Length_cm_2020), by = "well")

date.range <- strftime(as.POSIXlt(range(diver_neon_dat$date.time, na.rm = TRUE)), format = "%Y-%m")
bp.ls <- loadByProduct('DP1.00004.001', site = "SERC",
  startdate = date.range[1], enddate = date.range[2], timeIndex = 30)
y
bp.neon <- bp.ls$BP_30min
bp.neon$Bp_mod <- bp.neon$staPresMean * 10.1972 # from KPa to cm H20
bp.neon$startDateTime <- lubridate::force_tz(as.POSIXct(bp.neon$startDateTime), tz = "America/New_York")
bp.neon$endDateTime <- lubridate::force_tz(as.POSIXct(bp.neon$endDateTime), tz = "America/New_York")

interval.index <- findInterval(as.numeric(diver_neon_dat$date.time), as.numeric(bp.neon$startDateTime))

diver_neon_dat$Bp_mod <- bp.neon$Bp_mod[interval.index]
date <- strptime(Sys.time(), "%Y-%m-%d")
diver_neon_dat <- diver_neon_dat
save(diver_neon_dat, file = sprintf("Diver_data_%s.Rdata", date))


# need to cut out dates that have no calibration.


# dat1$date.time <- lubridate::force_tz(as.POSIXct(dat1$date.time), tz = "America/New_York")
# dat1 <- left_join(dat1, select(ele, elevation_meters, well, ele.probe), by = "well")
# dat1$temp.depth <- dat1$elevation_meters*100 - dat1$GWL_cm/100 # converting meters to cm



# met.P.neon <- subset(neon::neon_met_minute, date.time > range(dat2$date.time, na.rm = TRUE)[1] &
#                        date.time < range(dat2$date.time, na.rm = TRUE)[2])
# # load("data-raw/neon_met_minute.rda")
# # met.P.neon <- subset(neon_met_minute, date.time > range(dat2$date.time, na.rm = TRUE)[1] &
# #                   date.time < range(dat2$date.time, na.rm = TRUE)[2])
# attr(met.P.neon$date.time, "tzone")
# met.P.neon$date.time <- lubridate::force_tz(as.POSIXct(met.P.neon$date.time), tz = "America/New_York")
# met.P.neon$AtmPressure_cmH2O <- met.P.neon$Bp.tower*10.1972

# met.P <- subset(mettower::SERC.minute.data, date.time > range(dat2$date.time, na.rm = TRUE)[1] &
#                   date.time < range(dat2$date.time, na.rm = TRUE)[2])
# attr(met.P$date.time, "tzone")
# met.P$date.time <- lubridate::force_tz(as.POSIXct(met.P$date.time), tz = "America/New_York")
# met.P$AtmPressure_cmH2O <- met.P$Bp*10.1972 # from KPa to cm H20
# ## from Loggernet, atmospheric pressure at SDI 12 box
# div.atm.P <- soilmoisture::auto.watertable %>%
#   subset(div.AtmPressure_cmH2O > 900) %>%
#   mutate(date.time = lubridate::force_tz(as.POSIXct(date.time), tz = "America/New_York")) %>%
#   group_by(date.time) %>%
#   summarise(AtmPressure_cmH2O = mean(div.AtmPressure_cmH2O, na.rm = TRUE)) %>% # from hPa to cm H20
#   subset(!is.na(date.time))
# inter.date.time <- data.frame(date.time = seq(from = min(div.atm.P$date.time, na.rm = TRUE),
#                                     to = max(div.atm.P$date.time, na.rm = TRUE), by = "5 min"))
# div.atm.P <- div.atm.P %>% full_join(inter.date.time, by = "date.time") %>%
#   arrange(date.time) %>% mutate(AtmPressure_cmH2O = na.approx(AtmPressure_cmH2O))
# head(div.atm.P); tail(div.atm.P)
# ## now Loggernet measures every 15 min, while Diver records every 5 min, so need to interpolate times, else can't match time record against Diver data
# atm.P.1 <- bind_rows(met.P %>% select(date.time, AtmPressure_cmH2O),
#                    div.atm.P %>% subset(date.time >= max(met.P$date.time, na.rm = TRUE)))
# atm.P.2 <- bind_rows(atm.P.1,
#                      met.P.neon %>% select(date.time, AtmPressure_cmH2O) %>%
#                        subset(date.time >= max(met.P$date.time, na.rm = TRUE)))

# atm.P.interp.approx <- function(df) {
#   x <- df$date.time
#   y <- df$AtmPressure_cmH2O
#   xout <- df$date.time[is.na(df$AtmPressure_cmH2O)]
#   # yout <- approx(x, y, xout, method = "linear")
#   ## method = "constant" would be more parsimonious
#   yout <- approx(x, y, xout, method = "linear")
#   df.1 <- df %>%
#     left_join(data.frame(date.time = yout$x, AtmPressure_cmH2O.int = yout$y), by = "date.time") %>%
#     ## filling interpolation gap on the day of the census
#     mutate(AtmPressure_cmH2O.int = ifelse(is.na(AtmPressure_cmH2O), AtmPressure_cmH2O.int, AtmPressure_cmH2O))
#   return(df.1)
# }
# atm.P <- atm.P.interp.approx(atm.P.2) %>% select(-AtmPressure_cmH2O) %>% rename(AtmPressure_cmH2O = AtmPressure_cmH2O.int)
# atm <- left_join(subset(dat2, well == "baro"), select(atm.P, date.time, AtmPressure_cmH2O), by = "date.time")
# # atm <- left_join(neon.P, subset(dat2, well == "baro"), by = "date.time")

# head(atm); tail(atm)
# plot(atm$WaterPressure_cmH2O ~ atm$date.time)
# ggplot(atm, aes(x = date.time)) +
#   geom_point(aes(y = WaterPressure_cmH2O, color = 'Baro Diver')) +
#   geom_line(aes(y = AtmPressure_cmH2O, color = 'Met Tower')) +
#   # scale_y_continuous(sec.axis = sec_axis(~./50, name = "Atm Pressure [Hecto Pascals]")) +
#   theme(legend.position = c(0.8, 0.9)) +
#   scale_colour_manual(name = 'Data Source',
#                       values = c('Baro Diver' = 'black', 'Met Tower' = 'red')) +
#   ylab("Atmospheric Pressure [cmH2O]") + xlab("Date") +
#   scale_x_datetime(date_breaks = "1 week", date_labels = "%d%b%y", date_minor_breaks = "1 day") +
#   theme_bw() +
#   theme(axis.text.x = element_text(size = 11, face = "plain", angle = 90)) +
#   ggtitle("Atmospheric Pressure: Baro Diver at ForestGEO plot Vs. Met Tower")
# ggsave(file.path("figures/Atmospheric Pressure Baro vs. MetTower.jpeg"), height = 7, width = 15, units='in')


# ## erroneous data for 1e and 5f
# g0 <- ggplot(diver_neon_dat, aes(x = date.time, y = water_head_cm, colour = ele.probe)) +
#   geom_point(show.legend = F, size = 0.5) +
#   facet_grid(ele.probe ~., scales = "free_y") +
#   ylab("Water Head (cmH2O)") + xlab("Date") +
#   scale_x_datetime(date_breaks = "1 month", date_labels = "%d%b%y") +
#   theme(axis.text.x = element_text(size = 10, face = "plain", angle = 90)) +
#   ggtitle("Water Head above Divers at SERC ForestGEO plot")
# ggsave(file.path("figures/Water Head above Divers.jpeg"), plot = g0, height = 7, width = 12, units='in')

# diver_neon_dat.1 <- left_join(diver_neon_dat %>% subset(date.time < lubridate::force_tz(as.POSIXct("2019-02-14 00:00"), tz = "America/New_York")),
#                                     select(ele, well, A.Cable_Length_cm_2018), by = "well")
# diver_neon_dat.2 <- left_join(diver_neon_dat %>% subset(date.time >= lubridate::force_tz(as.POSIXct("2019-02-14 00:00"), tz = "America/New_York")),
#                                    select(ele, well, A.Cable_Length_cm_2020), by = "well")

############################### START HERE WITH DIVER_NEON_DAT INSTEAD OF diver_neon_dat ##########################################
# ## the depth of the probe bottom from soil surface == string length (which is attached to the top of the pipe sticking out above the surface to the top of the 110 mm long probe)

# # Compensating based on Atm Pressure data from Met Tower
# # Water head_cmH2O = Obs_Well_Diver_Pressure_cmH2O - Obs_Atm_pressure_cmH2O
# # Depth of water table_cm = Obs_Cable_length_cm - Pipe height above surface
# #  - Water head_cmH2O (This can be verified against actual measurement; cable length includes pipe height above ground)
# # OR
# # Ground_water_level_m = Obs_Well_Elevation_m - Depth of water table_cm/100

# diver_neon_dat <- left_join(dat2, subset(met.P, select = c(date.time, Bp_mod)), by = "date.time")
# diver_neon_dat <- left_join(dat2, subset(atm.P, select = c(date.time, )), by = "date.time")
diver_neon_dat$water_head_cm <- diver_neon_dat$WaterPressure_cmH2O - diver_neon_dat$Bp_mod
diver_neon_dat <- diver_neon_dat %>% mutate(water_head_cm = ifelse(water_head_cm > 600 |
                                                 water_head_cm < 0, NA, water_head_cm))

diver_neon_dat$probe.depth <- diver_neon_dat$A.Cable_Length_cm_2020 - 
  diver_neon_dat$`Pipe Height From Ground_cm` + 11 # the length of the probe itself in cm

## the depth of the water table from the surface
diver_neon_dat.3 <- diver_neon_dat
diver_neon_dat.3 <- diver_neon_dat.3 %>% mutate(depth = probe.depth - water_head_cm,
                            date = as.Date(date.time),
                            well.dates = paste(well, date, sep = ".")) %>%
  subset(well != "baro" & depth > 0)

p0 <- ggplot(diver_neon_dat.3, aes(x = date.time, y = depth, colour = ele.probe)) +
  geom_point(size = 0.1) +
  scale_y_reverse() +
  ylab("Water Depth (cm)") + xlab("Date") +
  scale_color_discrete("Elevation_m (probe)") +
  ggtitle("Water depth from soil surface in SERC ForestGEO plot")
p0 + scale_x_datetime(date_breaks = "1 month", date_labels = "%d%b%y") +
  theme(axis.text.x = element_text(size = 12, face = "plain", angle = 90)) + #vjust = -0.1 +
  theme(legend.position = c(0.6, 0.5))

# ## this needs some depth corrections

# # to remove data on days loggers were downloaded#-----------
# # dat2$date <- as.Date(dat2$date.time)
# # dat2$well.dates <- paste(dat2$well, dat2$date, sep = ".")
# ## erroneous data when loggers are removed for downloading
# ## so for now dates of download are removed
# dates <- read.csv("data-raw/dates_diver_download.csv",
#                 na.strings = c("NA", "NaN", ""),
#                 header = TRUE,
#                 row.names = NULL,
#                 check.names = TRUE)
# dates <- dates %>%
#   mutate(date = lubridate::force_tz(mdy(download_date), tz = "America/New_York"), ## correct from %Y to %y when excel is opened
#          well.dates = paste(well, date, sep = "."),
#          date.time = lubridate::force_tz(ymd_hm(paste(date, download_time, sep = " ")), tz = "America/New_York"),
#          date.time_floor = floor_date(date.time, unit = "5 mins"),
#          well.date.time_floor = paste(well, date.time_floor, sep = "."))
# diver_neon_dat.3$well.date.time <- paste(diver_neon_dat.3$well, diver_neon_dat.3$date.time, sep = ".")
# # removing an hour long record after the diver was downloaded
# ## corresponding date.time in diver_neon_dat
# corresponding.dt <- diver_neon_dat.3$date.time[diver_neon_dat.3$well.date.time %in% dates$well.date.time_floor]
# row.at.download <- sort(which(diver_neon_dat.3$well.date.time %in% dates$well.date.time_floor), decreasing = FALSE)
# ## remove 4 rows more covering 20 min after download:
# rows.to.remove <- sort(unlist(lapply(row.at.download, function(x) { c(x: c(x+4))})))
# # but some rows.to.remove index beyond diver_neon_dat rows, so excluding those
# row.to.remove.mod <- rows.to.remove[rows.to.remove < nrow(diver_neon_dat)]
# data <- diver_neon_dat.3 %>% mutate(depth = replace(depth, row.to.remove.mod, NA))
# ## then there are instances when time is not recorded, so removing whole day of record
# data <- data %>% mutate(depth = replace(depth, which(data$well.dates %in% dates$well.dates[is.na(dates$date.time)]), NA))
# # Met a hard layer of Marlboro clay at 4c, first couple of days data looks weird, so removing
# data <- data %>% mutate(depth = replace(depth, which(data$well.dates == "4c.2018-06-01"), NA))
# #

# ## 6b cable length clearly must have changed on June 6th, 2018 and July 19th, 2018
# # (it's string was loose (so cable length was variable)).
# # This was fixed on Aug 30, 2018, by pulling up the string by 20 cm)
# # so what was the drop in depth from "2018-06-07" to "2018-06-05"?
# mean(data$depth[data$well == "6b" & data$date == as.Date("2018-06-07")]) -
# mean(data$depth[data$well == "6b" & data$date == as.Date("2018-06-05")])
# # 47.56246
# # what was the gain in depth from "2018-07-18" to "2018-07-20"?
# offset <- mean(data$depth[data$well == "6b" & data$date == as.Date("2018-07-18")]) -
# mean(data$depth[data$well == "6b" & data$date == as.Date("2018-07-20")])
# offset
# # 11.91304
# # # Add offset for data between "2018-07-19" & "2018-08-31".
# # # do not repeat this step:
# # p0 +  scale_x_datetime(limits = c(as.POSIXct("2018-07-20") - 2*24*60*60, as.POSIXct("2018-07-20")),
# #                        date_breaks = "1 hour", date_labels = "%H")
# # ## so shifted from 10:10 to 10:15 on "2018-07-19"
# period1 <- seq(from = as.POSIXct("2018-07-19 10:10:00"), to = dates$date.time_floor[dates$well.dates == "6b.2018-08-30"], by = "5 min")
# data$depth[data$well == "6b" & data$date.time %in% period1] <-
#   data$depth[data$well == "6b" & data$date.time %in% period1] + offset

# # ## before "2018-06-06" as well
# # p0 +  scale_x_datetime(limits = c(as.POSIXct("2018-06-07") - 1*24*60*60, as.POSIXct("2018-06-07")),
# #                        date_breaks = "1 hour", date_labels = "%H")
# # ## so shifted from 08:10 to 08:15 on "2018-07-19"
# data$depth[data$well == "6b" & data$date.time < as.POSIXct("2018-06-06 08:15:00")] <-
#   data$depth[data$well == "6b" & data$date.time < as.POSIXct("2018-06-06 08:15:00")] + offset
# # ## seems to be an outlier for 4c on July 30, 0218
# # p0 +  geom_point(size = 2) + scale_x_datetime(limits = c(as.POSIXct("2018-05-14 16:30:00") - 2*60*60, as.POSIXct("2018-05-14 16:30:00")),
# #                        date_breaks = "5 min", date_labels = "%H%M")
# period2 <- seq(from = as.POSIXct("2018-05-14 15:40:00"), to = as.POSIXct("2018-05-14 15:55:00"), by = "5 min")
# data$depth[data$well == "2d" & data$date.time %in% period2] <- NA


# data$well.date.time <- paste(data$well, data$date.time, sep = ".")
# data <- full_join(data, select(manual, well.date.time, water_depth_manual_cm), by = "well.date.time")
# ## converting depth to GWL
# ## Elevation at 3a and 6b cant be perfect. Minimum values for water depths are negative(~ 10-20 m). Correcting elevation by thhose amounts
# data %>% subset(!is.na(ele.probe)) %>% group_by(ele.probe) %>% summarise(max_depth_cm = max(depth, na.rm = T))
# # ele.probe   max_depth_cm
# # <chr>              <dbl>
# # 1 0.98 m (3a)        -19.5
# # 2 1.7 m (6b)         -13.4
# # 3 5.19 m (5f)        -15.9
# # 4 8 m (4c)           -42.0
# # 5 8.32 m (2d)        -14.8
# # 6 9.55 m (1e)        -10.1
# data$GWL_m <- data$elevation_meters - data$depth/100 # from depth in cm to m
# data$GWL_m_manual <- data$elevation_meters - data$water_depth_manual_cm/100 # from depth in cm to m
# watertable <- data

# p2 <- ggplot(watertable,
#              aes(x = date.time, y = depth, colour = elevation_meters)) +
#   geom_point(size = 0.1) +
#   scale_y_reverse(limits = c(400, 0)) +
#   ylab("Water Depth (cm)") + xlab("Date") +
#   scale_color_continuous("Elevation at \nwell locations \n(masl)", breaks = sort(ele$elevation_meters)) +
#   theme(panel.grid.minor = element_blank()) +
#   guides(colour = guide_legend(override.aes = list(size = 4))) +
#   theme(legend.position = c(0.7, 0.4), legend.background = element_rect(fill = "transparent")) +
#   ggtitle("Water depth from soil surface at six locations at SERC ForestGEO plot") +
#   scale_x_datetime(date_breaks = "1 month", date_labels = "%d%b%y") +
#   theme(axis.text.x = element_text(size = 11, face = "plain", angle = 90))
# ggsave(file.path("figures/Water depth from surface by wells_single_panel_proposal.jpeg"),
#        plot = p2, height = 5, width = 7.5, units ='in')
# p2.1 <- p2 + geom_point(aes(y = water_depth_manual_cm),
#                         size = 3, shape = 3, alpha = 0.7, color = "red", show.legend = F) +
#   theme(axis.text.x = element_text(size = 12, face = "plain", angle = 90, margin = margin(b = 2)))
# ggsave(file.path("figures/Water depth from surface by wells_single_panel_proposal_OBS.jpeg"),
#        plot = p2.1, height = 5, width = 7.5, units='in')

# ## with observations overlaid
# p2.1 <- p2 +  geom_point(aes(y = water_depth_manual_cm), size = 3, color = "red", alpha = 0.7, shape = "triangle")

# p3 <- ggplot(subset(watertable, !is.na(ele.probe)),
#              aes(x = date.time, y = depth, colour = ele.probe)) +
#   geom_point(size = 0.1) +
#   scale_y_reverse(limits = c(400, 0)) +
#   ylab("Water Depth (cm)") + xlab("Date") +
#   scale_color_discrete("Elevation_m (probe)") +
#   theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
#   guides(colour = guide_legend(override.aes = list(size = 4))) +
#   theme(legend.position = c(0.3, 0.6), legend.background = element_rect(fill="transparent")) +
#   theme(axis.text = element_text(size = 12, face = "plain", angle = 0)) +
#   ggtitle("Water depth from soil surface at six locations at SERC ForestGEO plot") +
#   theme(axis.text.x = element_text(size = 12, face = "plain", angle = 90, margin = margin(b = 2)))
# p3.1 <- p3 + scale_x_datetime(date_breaks = "1 month", date_labels = "%d%b%y")
# ggsave(file.path("figures/Water depth from surface by wells_single_panel.jpeg"), plot = p3.1, height = 5, width = 7.5, units='in')
# ggsave(file.path("figures/Water depth from surface by wells_single_panel.pdf"), plot = p3.1, height = 5, width = 7.5, units='in')
# # p3.2 <- p3 +  scale_x_datetime(limits = c(max(watertable$date.time, na.rm = T) - 2*30*24*60*60, max(watertable$date.time, na.rm = T)),
# #                        date_breaks = "1 month", date_labels = "%d%b%y")
# # ggsave(file.path("figures/Water depth from surface by wells_single_panel_last_two_months.pdf"), plot = p3.2, height = 5, width = 7.5, units='in')
# # ggsave(file.path("figures/Water depth from surface by wells_single_panel_last_two_months.jpeg"), plot = p3.2, height = 5, width = 7.5, units='in')
# ## overlaid with obs
# p3.3 <- p3 + geom_point(aes(y = water_depth_manual_cm, fill = ele.probe),
#                         size = 3, shape = 23, color = "black", alpha = 0.7, show.legend = F) +
#   scale_x_datetime(date_breaks = "1 month", date_labels = "%d%b%y")
# ggsave(file.path("figures/Water depth from surface by wells_single_panel_with_OBS.jpeg"),  plot = p3.3, height = 5, width = 7.5, units='in')

# p3.4 <- p3.3 + scale_x_datetime(limits = c(max(watertable$date.time, na.rm = T) - 1*30*24*60*60, max(watertable$date.time, na.rm = T)),
#                                  date_breaks = "2 days", date_labels = "%d%b%y") +
#   theme(legend.position = c(0.8, 0.6), legend.background = element_rect(fill="transparent"))
# ggsave(file.path("figures/Water depth from surface by wells_single_panel_with_OBS_last month.jpeg"),  plot = p3.4, height = 5, width = 7.5, units='in')
# p3.5 <- p3.3 +  scale_x_datetime(limits = c(as.POSIXct("2020-03-02"), as.POSIXct("2020-03-02") + 3*24*60*60),
#                                  date_breaks = "1 day", date_labels = "%d%b%y") +
#   theme(legend.position = c(0.8, 0.6), legend.background = element_rect(fill="transparent"))
# ggsave(file.path("figures/Water depth from surface by wells_single_panel_with_OBS_measurements.jpeg"),  plot = p3.5, height = 5, width = 7.5, units='in')

# p3.6 <- p3.3 +  scale_x_datetime(limits = c(as.POSIXct("2020-07-15"), as.POSIXct("2020-09-15")),
#                                  date_breaks = "1 week", date_labels = "%d%b%y") +
#   theme(legend.position = c(0.9, 0.3), legend.background = element_rect(fill="transparent"))
# ggsave(file.path("figures/Water depth from surface by wells_single_panel_with_OBS_measurements_July-Aug-Sep2020.jpeg"),  plot = p3.6, height = 5, width = 7.5, units='in')
# p3.7 <- p3.3 +  scale_x_datetime(limits = c(as.POSIXct("2020-05-01"), as.POSIXct("2020-07-15")),
#                                  date_breaks = "1 week", date_labels = "%d%b%y") +
#   theme(legend.position = c(0.2, 0.3), legend.background = element_rect(fill="transparent"))
# ggsave(file.path("figures/Water depth from surface by wells_single_panel_with_OBS_measurements_May-Jun-July2020.jpeg"),  plot = p3.7, height = 5, width = 7.5, units='in')

# ###----
# ## plotting watertable
# p1 <- ggplot(subset(watertable, !is.na(ele.probe)),
#              aes(x = date.time, y = GWL_m, colour = ele.probe)) +
#   geom_point(size = 0.1) +
#   ylab("Ground Water Level (masl)") + xlab("Date") +
#   scale_color_discrete("Elevation_m (probe)") +
#   ggtitle("Ground Water Levels at SERC ForestGEO plot") +
#   theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
#   guides(colour = guide_legend(override.aes = list(size = 5))) +
#   theme(legend.position = c(0.55, 0.6), legend.background = element_rect(fill="transparent")) +
#   theme(axis.text.y = element_text(size = 12, face = "plain")) +
#   scale_y_continuous(limits = c(-2, 10)) +
#   theme(axis.text.x = element_text(size = 12, face = "plain", angle = 90, margin = margin(b = 2)))
# p1.1 <- p1 + scale_x_datetime(date_breaks = "2 months", date_labels = "%d%b%y")
# ggsave(file.path("figures/Ground Water Level by wells_single_panel.jpeg"), plot = p1.1, height = 4, width = 6, units='in')
# ggsave(file.path("figures/Ground Water Level by wells_single_panel.pdf"), plot = p1.1, height = 4, width = 6, units='in')
# p1.2 <- p1 + geom_point(aes(y = GWL_m_manual, fill = ele.probe), size = 3, shape = 24, alpha = 1, color = "black", show.legend = F)
# ggsave(file.path("figures/Ground Water Level by wells_single_panel_with_OBS.jpeg"),  plot = p1.2, height = 5, width = 7.5, units='in')
# p1.3 <- p1.2 +  scale_x_datetime(limits = c(max(watertable$date.time, na.rm = T) - 1*30*24*60*60, max(watertable$date.time, na.rm = T)),
#                        date_breaks = "2 days", date_labels = "%d%b%y") +
#   theme(legend.position = c(0.8, 0.6), legend.background = element_rect(fill="transparent"))
# ggsave(file.path("figures/Ground Water Level by wells_single_panel_with_OBS_last month.jpeg"),  plot = p1.3, height = 5, width = 7.5, units='in')
# p1.4 <- p1.2 +  scale_x_datetime(limits = c(as.POSIXct("2020-03-02"), as.POSIXct("2020-03-02") + 3*24*60*60),
#                                  date_breaks = "1 day", date_labels = "%d%b%y") +
#   theme(legend.position = "top", legend.background = element_rect(fill="transparent"))
# ggsave(file.path("figures/Ground Water Level by wells_single_panel_with_OBS_measurements.jpeg"),  plot = p1.4, height = 5, width = 7.5, units='in')

# div.man <- watertable %>% subset(date.time > as.POSIXct("01-03-2020") & !is.na(GWL_m_manual)) %>%
#   select(date.time, ele.probe, depth, water_depth_manual_cm, GWL_m, GWL_m_manual, `Pipe Height From Ground_cm`)
# View(div.man)

# load("data-raw/auto.watertable.rda")
# auto.watertable <- auto.watertable %>% rename(new = well) %>%
#   separate(new, c("delete", "well"), sep = 1) %>%
#   select(date.time, well, GWL_m) %>%
#   rename(auto.GWL_m = GWL_m) %>%
#   subset(!is.na(well)) %>%
#   subset(!is.na(auto.GWL_m))
# unique(auto.watertable$well)
# str(auto.watertable)

# watertable  <- watertable %>%
#   mutate(ObsLgw = GWL_m/100) %>%
#   left_join(ele %>% select(probe, Lat, Long), by = c("well" = "probe")) %>%
#   full_join(auto.watertable, by = c("well", "date.time")) %>%
#   mutate(GWL_m.filled = ifelse(is.na(GWL_m), auto.GWL_m, GWL_m)) %>%
#   mutate(date = as.Date(date.time)) %>%
#   arrange(date.time, well) %>%
#   subset(!is.na(date.time))

# ggplot(watertable, aes(x = date, y = GWL_m.filled)) +
#   geom_point(aes(group = well, color = well))

# write.csv(watertable,
#           file.path("data-raw/watertable.csv"),
#           row.names = FALSE)
# usethis::use_data(watertable, overwrite = TRUE)

# ## saving at daily scale--------

# watertable_daily <- watertable %>% group_by(date, well) %>%
#   select(-date.time, -ele.probe, -well.dates) %>%
#   summarise_all(funs(mean), na.rm = T)
# head(watertable_daily)
# table.well <- select(watertable, c(well, ele.probe))
# table.well <- table.well[!duplicated(table.well$well),]
# watertable_daily <- left_join(watertable_daily, table.well, by = "well")


# ## adding brewer well data

# write.csv(watertable_daily,
#           file.path("data-raw/watertable_daily.csv"),
#           row.names = FALSE)
# usethis::use_data(watertable_daily, overwrite = TRUE)


# p4 <- ggplot(subset(watertable_daily, !is.na(ele.probe)),
#              aes(x = date, y = depth, colour = ele.probe)) +
#   geom_point(size = 0.5) +
#   # geom_point(aes(x = date.time, y = water_depth_manual_cm), size = 3, pch = 8) +
#   ylab("Water Depth (cm)") + xlab("MonthDay, 2018") +
#   scale_y_continuous(limits = c(-400, 0)) +
#   scale_color_discrete("Elevation_m (probe)") +
#   theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
#   guides(colour = guide_legend(override.aes = list(size = 4))) +
#   theme(legend.position = c(0.2,0.25), legend.background = element_rect(fill="transparent")) +
#   theme(axis.text = element_text(size = 12, face = "plain", angle = 0)) +
#   ggtitle("Water depth from soil surface at six locations at SERC ForestGEO plot")
# p4 + scale_x_date(date_breaks = "1 week", date_labels = "%b%d", date_minor_breaks = "1 day") +
#   theme(axis.text.x = element_text(size = 12, face = "plain", angle = 90, margin = margin(b = 2)))
# ###------------
# p5 <- ggplot(subset(dat2, well == "baro"), aes(x = date.time, y = WaterPressure_cmH2O)) +
#   geom_point(size = 0.1) +
#   ylab("Atmospheric Pressure [cm H2O]") + xlab("Date") +
#   scale_color_discrete("Elevation_m (probe)") +
#   ggtitle("Atmospheric pressure time series") +
#   theme(legend.position = c(0.3,0.3))
# p5 + scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d", date_minor_breaks = "1 day") +
#   theme(axis.text.x = element_text(size = 12, face = "plain", angle = 45)) #vjust = -0.1
# ggsave(file.path("figures/Atpospheric pressure from Baro Diver.jpeg"), height = 6, width = 9, units='in')

# p6 <- ggplot(subset(dat2, well == "baro"), aes(x = date.time, y = Temp_C)) +
#   geom_point(size = 0.1) +
#   ylab("Temperature [deg C]") + xlab("Date") +
#   scale_color_discrete("Elevation_m (probe)") +
#   ggtitle("Temperature time series") +
#   theme(legend.position = c(0.85, 0.1))
# p6 + scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d", date_minor_breaks = "1 day") +
#   theme(axis.text.x = element_text(size = 12, face = "plain", angle = 45)) #vjust = -0.1
# ggsave(file.path("figures/Temperature from Baro Diver.jpeg"), height = 6, width = 9, units='in')



