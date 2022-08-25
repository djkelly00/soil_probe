##-------------------
## Author: Rutuja Chitra-Tarak, Sean McMahon, Jess Shue
## Date: 3/29/2022
## Title: To compile and plot 2018 SERC ForestGEO plot Diver/ water table level data
##-------------------

#rm(list = ls())
if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, mettower, neonUtilities, tmon,
#               scales, cowplot, soilmoisture, zoo, readxl)

#theme_set(theme_bw())
#theme_update(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             strip.background = element_blank())

ele <- readxl::read_excel(path = "data-raw/soil_moisture_probe_and diver_details.xlsx",
                          sheet = 1)
ele$A.Cable_Length_cm_2018 <- as.numeric(ele$A.Cable_Length_cm_2018)
ele$pipe_height_cm <- ele$`Pipe Height From Ground_cm`
ele$well <- ele$probe
ele$well[nrow(ele)] <- "baro"
## to match with baro files with pattern "bh"
wells <- ele$well; wells[nrow(ele)] <- "bh"
ele$ele.probe <- paste(ele$elevation_meters, " m"," (", ele$well, ")",sep = "")
ele$ele.probe <- factor(ele$ele.probe, levels =
                          c("0.98 m (3a)", "1.7 m (6b)", "5.19 m (5f)", "8 m (4c)",
                            "8.32 m (2d)", "9.55 m (1e)", "8.14 m (NAbaro)"))

## manual water depth measurements
manual <- read.csv(
  file.path("data-raw/water_depth_manual_measurements_Kay_0920.csv"),
  na.strings = c("NA", ""),
  header = T,
  row.names = NULL,
  check.names = F
)
manual$date <- lubridate::force_tz(lubridate::mdy(manual$date), tz = "America/New_York")
manual$date.time <- lubridate::force_tz(as.POSIXct(paste(manual$date, manual$time.matching,
                                                         sep = " ", format = "%Y-%m-%d %H:%M:%S %p")), tz = "America/New_York")
attr(manual$date.time, "tzone")
manual$date.time_ceiling <- lubridate::ceiling_date(manual$date.time, unit = "5 mins")
manual$well.date.time <- paste(manual$well, manual$date.time_ceiling, sep = ".")
manual$water_depth_manual_cm <- as.numeric(manual$water_depth_manual_cm)
manual$water_depth_manual_cm[2] <- NA ## was this depth inaccurate?

## adding diver data exported from diver.office. files have 51 lines of text, then header which does not get read, so 52 lines removed
path1 = "data-raw/Diver-Office_exported data/CSV/compensated"
path2 = "data-raw/Diver-Office_exported data/CSV/uncompensated"

###########################################################################################
################# compensated files; May 30th, 2018 - August 30th, 2018 ###################
########################### data not used ##############################
all.files1 <- list.files(
  path =
    path1,
  pattern = ".CSV",
  recursive = FALSE,
  full.names = TRUE
)

str(all.files1)
total1 = length(all.files1)

# create progress bar
pb1 <- txtProgressBar(min = 0, max = total1, style = 3)
for (i in 1:length(all.files1)) {
  new.file <- read.csv(
    all.files1[i],
    skip = 51, ## remove details
    na.strings = c("NA", ""),
    header = T,
    row.names = NULL,
    check.names = F
  )
  # remove last row
  new.file <- new.file[-nrow(new.file), ]
  # add well ID
  new.file$well <- ele$well[str_detect(tolower(all.files1[i]), wells)]
  if (i == 1) {
    dat1 <- new.file
  } else {
    dat1 <- rbind.data.frame(dat1, new.file)
  }
  Sys.sleep(0.1)
  # update progress bar
  setTxtProgressBar(pb1, i)
  ## add headers
}
colnames(dat1) <- c("date.time", "GWL_cm", "Temp_C", "well")
#water head = water pressure cm H2O - baro pressure cm H2O (compensated in Diver Office); becomes GWL_cm
head(dat1); tail(dat1)

dat1$date.time <- strptime(dat1$date.time, "%Y/%m/%d %H:%M:%S")
str(dat1)
dat1$date.time <- lubridate::force_tz(as.POSIXct(dat1$date.time), tz = "America/New_York")
dat1 <- left_join(dat1, select(ele, elevation_meters, well, ele.probe), by = "well")
dat1$temp.depth <- dat1$elevation_meters*100 - dat1$GWL_cm/100 # converting meters to cm


##################################################################################
##################### uncompensated files #######################################
################################################################################
all.files2 <- list.files(
  path =
    path2,
  pattern = ".CSV",
  recursive = FALSE,
  full.names = TRUE
)

### need to remove file serc_fgeo_6b_aj638_190221085436_AJ638.CSV as it has data which erroneously repeats data from the beginning of file beginning 1/27/2019

all.files2 <- all.files2[!str_detect(all.files2, "serc_fgeo_6b_aj638_190221085436_AJ638.CSV")]
str(all.files2)

total2 = length(all.files2)

pb2 <- txtProgressBar(min = 0, max = total2, style = 3)
for (i in 1:length(all.files2)) {
  new.file <- read.csv(
    all.files2[i],
    skip = 51, ## remove details
    na.strings = c("NA", ""),
    header = T,
    row.names = NULL,
    check.names = F
  )
  # remove last row
  new.file <- new.file[-nrow(new.file),]
  # add well ID
  new.file$well <- ele$well[str_detect(tolower(all.files2[i]), wells)]
  if (i == 1) {
    dat2 <- new.file
  } else {
    dat2 <- rbind.data.frame(dat2, new.file)
  }
  Sys.sleep(0.1)
  # update progress bar
  setTxtProgressBar(pb2, i)
  ## add headers
}

colnames(dat2) <- c("date.time", "WaterPressure_cmH2O", "Temp_C", "well")
dat2$date.time <- strptime(dat2$date.time, "%Y/%m/%d %H:%M:%S")
str(dat2)
head(dat2); tail(dat2)
dat2$date.time <- as.POSIXct(lubridate::force_tz(as.POSIXct(dat2$date.time), tz = "America/New_York"))
dat2 <- dat2 %>% arrange(date.time)

dat2 <- dat2 %>% mutate(date.time.well = paste(date.time, well, sep = "_")) %>%
  subset(!duplicated(date.time.well)) %>% arrange(date.time)
dat2 <- left_join(dat2, select(ele, elevation_meters, well,
                               ele.probe, `Pipe Height From Ground_cm`), by = "well")

#### remove final rows of dat2 with NA's for date.time
dat2 <- dat2[-c(597893:597898),]


################################################################################################
################### separate dat2 into separate year files ####################################

############################## 2018 #########################################
dat2018 <- subset(dat2, dat2$date.time < "2019-01-01")
#every 5 minutes; May 9th - Dec 31st
#baro May 9th - Sep 20th

dat2018.1 <- left_join(dat2018, subset(atm.P, select = c(date.time, AtmPressure_cmH2O)), by = "date.time")
dat2018.1$water_head_cm <- dat2018.1$WaterPressure_cmH2O - dat2018.1$AtmPressure_cmH2O
dat2018.1 <- dat2018.1 %>% mutate(water_head_cm = ifelse(water_head_cm > 600 |
                                                 water_head_cm < 0, NA, water_head_cm))

##############################################################
### check baro against Neon and Met tower? Rutuja didn't use the baro data?
datbaro <- subset(dat2018.1, dat2018.1$well == "baro")

## remove baro data
dat2018.1 <- subset(dat2018.1, dat2018.1$well != "baro")

plot(dat2018.1$date.time, dat2018.1$water_head_cm)
plot(dat2018.1$date.time, dat2018.1$depth)

### add elevation and cable length
dat2018.2 <- left_join(dat2018.1, select(ele, well, A.Cable_Length_cm_2018), by = "well")

## the depth of the probe bottom from soil surface == string length (which is attached to the top of the pipe sticking out above the surface to the top of the 110 mm long probe)
dat2018.2$probe.depth <- dat2018.2$A.Cable_Length_cm_2018 -
  dat2018.2$`Pipe Height From Ground_cm` + 11 # the length of the probe itself in cm

## the depth of the water table from the surface
dat2018.3 <- dat2018.2 %>% mutate(depth = probe.depth - water_head_cm,
                            date = as.Date(date.time),
                            well.dates = paste(well, date, sep = ".")) %>%
                            subset(depth > 0)

plot(dat2018.3$date.time, dat2018.3$depth)

# to remove data on days loggers were downloaded
dates <- read.csv("data-raw/dates_diver_download.csv",
                  na.strings = c("NA", "NaN", ""),
                  header = TRUE,
                  row.names = NULL,
                  check.names = TRUE)

dates <- dates %>%
  mutate(date = lubridate::force_tz(mdy(download_date), tz = "America/New_York"), ## correct from %Y to %y when excel is opened
         well.dates = paste(well, date, sep = "."),
         date.time = lubridate::force_tz(ymd_hm(paste(date, download_time, sep = " ")), tz = "America/New_York"),
         date.time_floor = floor_date(date.time, unit = "5 mins"),
         well.date.time_floor = paste(well, date.time_floor, sep = "."))
# 174 failed to parse because there was no download time recorded

dat2018.3$well.date.time <- paste(dat2018.3$well, dat2018.3$date.time, sep = ".")
# removing an hour long record after the diver was downloaded
## corresponding date.time in dat3
corresponding.dt <- dat2018.3$date.time[dat2018.3$well.date.time %in% dates$well.date.time_floor]
row.at.download <- sort(which(dat2018.3$well.date.time %in% dates$well.date.time_floor), decreasing = FALSE)
## remove 4 rows more covering 20 min after download:
rows.to.remove <- sort(unlist(lapply(row.at.download, function(x) { c(x: c(x+4))})))
# but some rows.to.remove index beyond dat3 rows, so excluding those
#row.to.remove.mod <- rows.to.remove[rows.to.remove < nrow(dat3)]
data2018 <- dat2018.3 %>% mutate(depth = replace(depth, rows.to.remove, NA))
## then there are instances when time is not recorded, so removing whole day of record
data2018 <- data2018 %>% mutate(depth = replace(depth, which(data2018$well.dates %in% dates$well.dates[is.na(dates$date.time)]), NA))
# Met a hard layer of Marlboro clay at 4c, first couple of days data looks weird, so removing
data2018 <- data2018 %>% mutate(depth = replace(depth, which(data2018$well.dates == "4c.2018-06-01"), NA))

## 6b cable length clearly must have changed on June 6th, 2018 and July 19th, 2018
# (it's string was loose (so cable length was variable)).
# This was fixed on Aug 30, 2018, by pulling up the string by 20 cm)
# so what was the drop in depth from "2018-06-07" to "2018-06-05"?
mean(data2018$depth[data2018$well == "6b" & data2018$date == as.Date("2018-06-07")]) -
  mean(data2018$depth[data2018$well == "6b" & data2018$date == as.Date("2018-06-05")])
# 47.56246
# what was the gain in depth from "2018-07-18" to "2018-07-20"?
offset <- mean(data2018$depth[data2018$well == "6b" & data2018$date == as.Date("2018-07-18")]) -
  mean(data2018$depth[data2018$well == "6b" & data2018$date == as.Date("2018-07-20")])
offset
# 27.99072
# # Add offset for data between "2018-07-19" & "2018-08-31".
# # do not repeat this step:
# p0 +  scale_x_datetime(limits = c(as.POSIXct("2018-07-20") - 2*24*60*60, as.POSIXct("2018-07-20")),
#                        date_breaks = "1 hour", date_labels = "%H")
# ## so shifted from 10:10 to 10:15 on "2018-07-19"
period1 <- seq(from = as.POSIXct("2018-07-19 10:10:00"), to = dates$date.time_floor[dates$well.dates == "6b.2018-08-30"], by = "5 min")
data2018$depth[data2018$well == "6b" & data2018$date.time %in% period1] <-
  data2018$depth[data2018$well == "6b" & data2018$date.time %in% period1] + offset

# ## before "2018-06-06" as well
# p0 +  scale_x_datetime(limits = c(as.POSIXct("2018-06-07") - 1*24*60*60, as.POSIXct("2018-06-07")),
#                        date_breaks = "1 hour", date_labels = "%H")
# ## so shifted from 08:10 to 08:15 on "2018-07-19"
data2018$depth[data2018$well == "6b" & data2018$date.time < as.POSIXct("2018-06-06 08:15:00")] <-
  data2018$depth[data2018$well == "6b" & data2018$date.time < as.POSIXct("2018-06-06 08:15:00")] + offset
# ## seems to be an outlier for 4c on July 30, 0218
# p0 +  geom_point(size = 2) + scale_x_datetime(limits = c(as.POSIXct("2018-05-14 16:30:00") - 2*60*60, as.POSIXct("2018-05-14 16:30:00")),
#                        date_breaks = "5 min", date_labels = "%H%M")
period2 <- seq(from = as.POSIXct("2018-05-14 15:40:00"), to = as.POSIXct("2018-05-14 15:55:00"), by = "5 min")
data2018$depth[data2018$well == "2d" & data2018$date.time %in% period2] <- NA




plot(data2018$date.time, data2018$depth, main = "2018 water depth", col = data2018$well)
legend(as.POSIXct("2018-05-31 00:00:00"), 400, legend = well.name)
plot(data2018$date.time, data2018$water_head_cm, main = "2018 water head")
data2018$well <- as.factor(as.character(data2018$well))
well.name <- as.data.frame(c("1e", "2d", "3a", "4c", "5f", "6b"))

identify(data2018$date.time, data2018$depth, 2)
f5 <- subset(data2018, well == "5f")
plot(f5$date.time, f5$depth, main = "well 5f") ### data downloaded 9/28 but not removed
#16:25 - 9/29 04:00
## need to remove part of 9/28 for 5f - why was it not removed by Rutuja's code? Data downloaded
period3.5 <- seq(from = as.POSIXct("2018-09-28 16:25:00"), to = as.POSIXct("2018-09-29 04:00:00"), by = "5 min")
data2018$depth[data2018$well == "5f" & data2018$date.time %in% period3.5] <- NA

############ add in calculated fields
data2018$GWL_m <- data2018$elevation_meters - data2018$depth/100

data2018final <- data2018[ , c(1, 4, 2, 3, 9, 10, 17, 12, 13, 5, 14)]
write.csv(data2018final, "C:/Users/jessh/Documents/GitHub/soil_probe/processed_data/data2018_21_jun_2022.csv", row.names = FALSE)

####### metadata
metadata <- ele[ , c(2, 4:8, 12:14, 16:21)]
write.csv(metadata, "C:/Users/jessh/Documents/GitHub/soil_probe/diver_data/diver_metadata.csv", row.names = FALSE)
########################### 2019 #######################################################

dat2019 <- subset(dat2,  dat2$date.time >= "2019-01-01" & dat2$date.time < "2020-01-01")
#every 5 minutes, stops Feb. 14th - needs auto network data added

######################### 2020 ###########################################################
dat2020 <- subset(dat2,  dat2$date.time >= "2020-01-01" & dat2$date.time < "2021-01-01")
#every 5 minutes starts Feb. 27th - Apr 6th; switches to measurement every hour
#diver 4c on 5/21 begins at 9:57 - round to next hour?

####################### 2021 ############################################################
dat2021 <- subset(dat2,  dat2$date.time >= "2021-01-01" & dat2$date.time < "2022-01-01")
#every hour

###################### 2022 #############################################################
dat2022 <- subset(dat2,  dat2$date.time >= "2022-01-01" & dat2$date.time < "2023-01-01")
#every hour; stops 3/4 - needs auto network data - 6b will need to be manually added
