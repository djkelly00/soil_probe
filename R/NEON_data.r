install.packages("neonUtilities")
install.packages("BiocManager")
BiocManager::install('rhdf5')
library(neonUtilities)

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


##### for 2020, need 5 minute data January - April 6th
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
