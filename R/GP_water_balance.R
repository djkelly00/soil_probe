install.packages("neonUtilities")
install.packages("BiocManager")
BiocManager::install('rhdf5')
library(neonUtilities)
library(tidyverse)
options(stringsAsFactors=F)

### Precipitation - Data collected in any given month are published during the second full week of the following month.
### data for January are missing...
zipsByProduct(dpID="DP1.00006.001", package="basic",
              site="SERC",
              startdate="2023-01-01", enddate="2023-07-14",
              savepath="C:/Users/jessh/Dropbox (Smithsonian)/NEON/data-raw/JS",
              check.size=F)

precip <- stackByTable(filepath="C:/Users/jessh/Dropbox (Smithsonian)/NEON/data-raw/JS/filesToStack00006",                      nCores=parallel::detectCores())

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


## quick view of the year
precip.plot <- precip.tower[which(precip.tower$Precip.tower > 0),]
plot(precip.plot$Precip.tower ~ precip.plot$date, col = 'blue', pch = 19, type = 'b', xlab = "Date 2023", ylab = "Precipitation mm")
abline(h = 0, lty = 2)
#############################################################
### Add Jess Parker's evapotranspiration calculations #######
#############################################################
library(lubridate)
precip.tower$DOY <- yday(precip.tower$date) # add day of year

# add January data from Met tower? January missing as well...
met.precip <- read.table("~/Dropbox (Smithsonian)/SERC_Tower_MET/SERC_Tower_Rawdata_Loggernet/SERC_Tower_current_data/SERC_TOWER_MET_SERC_TOWER.dat", header = TRUE, skip = 1, sep = ",")

### Add LAI value to precip dataframe
LAI <- read.csv("~/Dropbox (Smithsonian)/NEON/data/DOY_LAI_non_leap_year.csv")

precip.tower <- merge(precip.tower, LAI, by = "DOY", all = TRUE)

#### replace or remove January values with 0 for now
#precip.tower <- within(precip.tower[c(1:31), ], Precip.tower <- 0)
precip.tower <- precip.tower[-c(1:31), ]

### create a column of cumulative increase in precipitation
precip.tower$cumulative.INC <- 0
precip.tower <- precip.tower %>% 
  mutate(cumulative.INC = accumulate(Precip.tower, function(x, y) (x + y)))

### create Through Fall
precip.tower$Throughfall <- ifelse(precip.tower$Precip.tower < 0.644, 0,((precip.tower$Precip.tower * 0.9075) - 0.584)) 

### create Stem flow
precip.tower$StemFlow <- ifelse(precip.tower$Precip.tower < 2.466, 0, ((precip.tower$Precip.tower * 0.0515) - 0.127))

### add throughfall and stem flow
precip.tower$TF.SF <- precip.tower$Throughfall + precip.tower$StemFlow

### add net precipitation
precip.tower$NetPrecip <- precip.tower$TF.SF - 0.089

### add cumulative net precipitation
precip.tower$cumulative.NETP <- 0
precip.tower <- precip.tower %>% 
  mutate(cumulative.NETP = accumulate(NetPrecip, function(x, y) (x + y)))

### add transpiration
trsp <- 701.3/182.15
precip.tower$Transpiration <- precip.tower$relLAI * trsp

### precipitation accounting for loss due to transpiration
precip.tower$TRSP.Loss <- precip.tower$NetPrecip - precip.tower$Transpiration

### cumulative precipitation after accounting for transpiration
precip.tower$cumulative.PRECIP <- 0
precip.tower <- precip.tower %>% 
  mutate(cumulative.PRECIP = accumulate(TRSP.Loss, function(x, y) (x + y)))


## plot of water balance

plot(precip.tower$cumulative.PRECIP ~ precip.tower$date, col = 'blue', pch = 19, xlab = "Date 2023", ylab = "Precipitation mm", main = "Water Balance 2023")
abline(h = 0, lty = 2)
