##-------------------
## Author: Rutuja Chitra-Tarak, Sean McMahon, Jess Shue, and Dominique Kelly
## Date: 6/14/2021
## Title: To compile and plot SERC ForestGEO plot Diver/ water table level data
##-------------------

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
dat2$date.time <- strptime(dat2$date.time, "%Y/%m/%d %H:%M:%S", tz = "UTC")
dat2$date.time <- dat2$date.time - 5 * 60 * 60

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

bp.neon$startDateTime <- lubridate::force_tz(as.POSIXct(bp.neon$startDateTime), tz = "UTC")
bp.neon$endDateTime <- lubridate::force_tz(as.POSIXct(bp.neon$endDateTime), tz = "UTC")

bp.neon$startDateTime <- bp.neon$startDateTime - 5 * 60 * 60
bp.neon$endDateTime <- bp.neon$endDateTime - 5 * 60 * 60



interval.index <- findInterval(as.numeric(diver_neon_dat$date.time), as.numeric(bp.neon$startDateTime))

diver_neon_dat$Bp_mod <- bp.neon$Bp_mod[interval.index]
date <- strptime(Sys.time(), "%Y-%m-%d")
diver_neon_dat <- diver_neon_dat
save(diver_neon_dat, file = sprintf("Diver_data_%s.Rdata", date))

diver_neon_dat$water_head_cm <- diver_neon_dat$WaterPressure_cmH2O - diver_neon_dat$Bp_mod
diver_neon_dat <- diver_neon_dat %>% mutate(water_head_cm = ifelse(water_head_cm > 600 |
                                                 water_head_cm < 0, NA, water_head_cm))

diver_neon_dat$probe.depth <- diver_neon_dat$A.Cable_Length_cm_2020 - 
  diver_neon_dat$`Pipe Height From Ground_cm` + 11 # the length of the probe itself in cm

## the depth of the water table from the surface
diver_neon_dat.3 <- diver_neon_dat
diver_neon_dat.3$date.time <- as.POSIXct(diver_neon_dat.3$date.time)
diver_neon_dat.3 <- diver_neon_dat.3 %>% mutate(depth = probe.depth - water_head_cm,
                            date = as.POSIXct(date.time),
                            well.dates = paste(well, date, sep = ".")) %>%
  subset(well != "baro" & depth > 0)

save(diver_neon_dat.3, file = sprintf("DATA_FILES/diver_neon_dat_%s.Rdata", Sys.Date()))