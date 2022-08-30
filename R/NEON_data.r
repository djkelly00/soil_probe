install.packages("neonUtilities")
install.packages("BiocManager")
BiocManager::install('rhdf5')
library(neonUtilities)
library(tidyverse)
options(stringsAsFactors=F)

### Precipitation
zipsByProduct(dpID="DP1.00006.001", package="basic",
              site="SERC",
              startdate="2018-01-01", enddate="2022-08-23",
              savepath="C:/Users/jessh/Dropbox (Smithsonian)/NEON/data-raw/JS",
              check.size=F)

### BP
zipsByProduct(dpID="DP1.00004.001", package="basic",
              site="SERC",
              startdate="2018-01-01", enddate="2022-08-23",
              savepath="C:/Users/jessh/Dropbox (Smithsonian)/NEON/data-raw/JS",
              check.size=F)


precip <- stackByTable(filepath="C:/Users/jessh/Dropbox (Smithsonian)/NEON/data-raw/JS/filesToStack00006",                      nCores=parallel::detectCores())

atmprs <- stackByTable(filepath="C:/Users/jessh/Dropbox (Smithsonian)/NEON/data-raw/JS/filesToStack00004",
                       nCores=parallel::detectCores())


#########################################################################
#################### Precipitation #####################################
#######################################################################
minutes <- "1min" # minute data
precip <-
  read.csv(
    file.path(paste("C:/Users/jessh/Dropbox (Smithsonian)/NEON/data-raw/JS/filesToStack00006/stackedFiles/SECPRE_", minutes, ".csv", sep = "")),
    na.strings = c("NA", ""),
    header = T,
    row.names = NULL,
    check.names = F
  )


### throughfall from the ground
precipTH <-
  read.csv(
    file.path(paste("C:/Users/jessh/Dropbox (Smithsonian)/NEON/data-raw/JS/filesToStack00006/stackedFiles/THRPRE_", minutes, ".csv", sep = "")),
    na.strings = c("NA", ""),
    header = T,
    row.names = NULL,
    check.names = F
  )



## For precip,  verticalPosition is only 60 (top)
unique(precip$horizontalPosition); unique(precip$verticalPosition)
# 0, 60
precip.min.tower <- precip %>% select(startDateTime, secPrecipBulk) %>%
  mutate(date = as.Date(startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")) %>%
  mutate(date.time = as.POSIXct(startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")) %>%
  mutate(Precip.tower = secPrecipBulk, na.rm = T) %>%
  select(date.time, date, Precip.tower)

precip.tower <- precip.min.tower %>%
  group_by(date) %>% summarise(Precip.tower = sum(Precip.tower, na.rm = T))

### throughfall
precip.min.TH <- precipTH %>% select(startDateTime, TFPrecipBulk) %>%
  mutate(date = as.Date(startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")) %>%
  mutate(date.time = as.POSIXct(startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")) %>%
  group_by(date.time, date) %>% summarise(throughfall = mean(TFPrecipBulk, na.rm = T)) %>%
  select(date.time, date, throughfall)

precip.TH <- thf.min %>%
  group_by(date) %>% summarise(throughfall = sum(throughfall, na.rm = T))

### 2018 ###
precip.daily.2018 <- subset(precip.tower, precip.tower$date >= "2018-01-01" & precip.tower$date < "2019-01-01")

write.csv(precip.daily.2018, "C:/Users/jessh/Dropbox (Smithsonian)/SERC_water_balance/NEON_daily_precip_2018.csv", row.names = FALSE)


### 2019 ###
precip.daily.2019 <- subset(precip.tower, precip.tower$date >= "2019-01-01" & precip.tower$date < "2020-01-01")

write.csv(precip.daily.2019, "C:/Users/jessh/Dropbox (Smithsonian)/SERC_water_balance/2019/NEON_daily_precip_2019.csv", row.names = FALSE)

### 2020 ###
precip.daily.2020 <- subset(precip.tower, precip.tower$date >= "2020-01-01" & precip.tower$date < "2021-01-01")

write.csv(precip.daily.2020, "C:/Users/jessh/Dropbox (Smithsonian)/SERC_water_balance/2020/NEON_daily_precip_2020.csv", row.names = FALSE)

TF.daily.2020 <- subset(precip.TH, precip.TH$date >= "2020-01-01" & precip.TH$date < "2021-01-01")

write.csv(TF.daily.2020, "C:/Users/jessh/Dropbox (Smithsonian)/SERC_water_balance/2020/NEON_daily_throughfall_2020.csv", row.names = FALSE)

### 2021 ###
precip.daily.2021 <- subset(precip.tower, precip.tower$date >= "2021-01-01" & precip.tower$date < "2022-01-01")

write.csv(precip.daily.2021, "C:/Users/jessh/Dropbox (Smithsonian)/SERC_water_balance/2021/NEON_daily_precip_2021.csv", row.names = FALSE)


### 2022 ###
precip.daily.2022 <- subset(precip.tower, precip.tower$date >= "2022-01-01" & precip.tower$date < "2023-01-01 00:00:00")

write.csv(precip.daily.2022, "C:/Users/jessh/Dropbox (Smithsonian)/SERC_water_balance/2022/NEON_daily_precip.csv", row.names = FALSE)
##########################################################################
################### Barometric Pressure #################################
##### for 2020, need 5 minute data January - April 6th #################
########################################################################
minutes <- "1min" # minute data
bp.big <-
  read.csv(
    file.path(paste("C:/Users/jessh/Dropbox (Smithsonian)/NEON/data-raw/JS/filesToStack00004/stackedFiles/BP_", minutes, ".csv", sep = "")),
    na.strings = c("NA", ""),
    header = T,
    row.names = NULL,
    check.names = F
  )

library(tidyverse)
bp.min.tower <- bp.big %>% select(startDateTime, corPres, corPresFinalQF) %>%
  filter(corPresFinalQF == 0) %>%
  mutate(date = as.Date(startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")) %>%
  mutate(date.time = as.POSIXct(startDateTime, format = "%Y-%m-%dT%H:%M:%SZ")) %>%
  mutate(Bp.tower = corPres) %>% select(date.time, date, Bp.tower)

# Calculate KPa to cm H2O
bp.min.tower$corPres.cmH2O <- bp.min.tower$Bp.tower * 10.1972 
View(BP_30min)

# time zone?
library(lubridate)
bp.min.tower$date.time <- as.POSIXct(bp.min.tower$date.time, format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone())

bp.min.tower$date.time <- as.POSIXct(lubridate::force_tz(as.POSIXct(bp.min.tower$date.time), tz = "America/New_York"))
min.date.time <- min(bp.min.tower$date.time, na.rm = TRUE)
max.date.time <- max(bp.min.tower$date.time, na.rm = TRUE)

all.date.time <- data.frame(date.time = seq(min.date.time, max.date.time, by = 'mins'))

# 5 minute Mean station pressure corrected to sea level
## all 5 minute data in 2020 have pressure from Rutuja's code, so not needed.
bp.min.tower$time.interval <- cut(bp.min.tower$date.time, breaks = "5 min")

bp.NEON <- bp.min.tower %>%
  group_by(time.interval) %>% summarise(Bp.tower = mean(corPres.cmH2O, na.rm = T))

#write.csv(bp.NEON, "C:/Users/jessh/Documents/GitHub/soil_probe/MET_data/NEON_5min_bp.csv", row.names = FALSE)
#### for 2020, After April 6th, need hourly
bp.min.tower$hour <- cut(bp.min.tower$date.time, breaks = "60 min")

bp.min.2020 <- subset(bp.min.tower, bp.min.tower$date.time >= "2020-01-01 00:00:00" & bp.min.tower$date.time < "2021-01-01 00:00:00")

bp.NEON.hour <- bp.min.2020 %>%
  group_by(hour) %>% summarise(AtmPressure_cmH2O = mean(corPres.cmH2O, na.rm = T))

write.csv(bp.NEON.hour, "C:/Users/jessh/Documents/GitHub/soil_probe/MET_data/NEON_hourly_bp_2020.csv", row.names = FALSE)

########################################
##### export 2021 and 2022 data #######
######################################

### 2021
bp.hour.2021 <- subset(bp.min.tower, bp.min.tower$date.time >= "2021-01-01" & bp.min.tower$date.time < "2022-01-01 00:00:00")

bp.hour.2021 <- bp.hour.2021 %>%
  group_by(hour) %>% summarise(AtmPressure_cmH2O = mean(corPres.cmH2O, na.rm = T))

write.csv(bp.hour.2021, "C:/Users/jessh/Documents/GitHub/soil_probe/MET_data/NEON_hourly_bp_2021.csv", row.names = FALSE)


### 2022
bp.hour.2022 <- subset(bp.min.tower, bp.min.tower$date.time >= "2022-01-01" & bp.min.tower$date.time < "2023-01-01 00:00:00")

bp.hour.2022 <- bp.hour.2022 %>%
  group_by(hour) %>% summarise(AtmPressure_cmH2O = mean(corPres.cmH2O, na.rm = T))

write.csv(bp.hour.2022, "C:/Users/jessh/Documents/GitHub/soil_probe/MET_data/NEON_hourly_bp_2022.csv", row.names = FALSE)
