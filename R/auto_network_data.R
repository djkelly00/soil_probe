##### Explore data uploaded by Roy's Network ######################
###### Check file format - need to skip row 1 as well as 2 and 3
#readLines("~/Dropbox (Smithsonian)/FGEO_Sensor_Data/Current_data/FGEO-1_DIVER.dat")

### read in current data from Roy's FGEO folder to check Diver's are connected and data is coming in
diver <- read.table("~/Dropbox (Smithsonian)/FGEO_Sensor_Data/Current_data/FGEO-1_DIVER.dat", header = TRUE, skip = 1, sep = ",")

 
### remove original rows 2 and 3; after skipping line 1 now they are rows 1 and 2
diver <- diver[-c(1,2), ]

##### read in one of the raw data files just to see what they look like
diver1 <- read.table("~/Dropbox (Smithsonian)/FGEO_Sensor_Data/Raw_data/raw_dir/FGEO-1_DIVER_20211123034402.dat", header = TRUE, skip = 1, sep = ",")

##################################################################
########## Read in multiple files and compile ###################
#################################################################
setwd("~/Dropbox (Smithsonian)/FGEO_Sensor_Data/Raw_data/raw_dir/")

#### make a list of all the files in the raw data folder
filelist <- list.files(path = "~/Dropbox (Smithsonian)/FGEO_Sensor_Data/Raw_data/raw_dir", pattern = "FGEO-1_DIVER_\\d+\\.dat$")

library(data.table)
#### read the list of files and combine into one dataframe

dataset <- do.call("rbind", lapply(filelist, FUN = function(file) {
  read.table(file, header=TRUE, sep=",", skip = 1, na.strings = "99999")
}))

### remove rows with 'TS' and blank in column TIMESTAMP - extra header rows not easy to remove in previous step
dataset <- subset(dataset, TIMESTAMP != "TS")
dataset <- subset(dataset, TIMESTAMP != "")

#### convert from wide to long using header names...
### rename columns:  .1 = pressure cm H2O diver, .2 = temp C diver, .3 = baro pressure cm H2O SDI12 box, .4 = temp C SDI12 box, .5 = compensated pressure cm H2O (column 1 - column 3) 
data.long <- melt(setDT(dataset), measure.vars = patterns("DIV_\\d+\\_Avg.1", "DIV_\\d+\\_Avg.2", "DIV_\\d+\\_Avg.3", "DIV_\\d+\\_Avg.4", "DIV_\\d+\\_Avg.5"), value.name = c("WaterPressure_cmH2O", "Temp_C", "AtmPressure_cmH2O", "temp.SDI12", "water_head_cm"))


summary(data.long$variable)
colnames(data.long)[colnames(data.long)=="variable"] <- "well"

### add letter designations to well names 1e, 2d, 3a, 4c, 5f, 6b
levels(data.long$well)[match("1",levels(data.long$well))] <- "1e"
levels(data.long$well)[match("2",levels(data.long$well))] <- "2d"
levels(data.long$well)[match("3",levels(data.long$well))] <- "3a"
levels(data.long$well)[match("4",levels(data.long$well))] <- "4c"
levels(data.long$well)[match("5",levels(data.long$well))] <- "5f"
levels(data.long$well)[match("6",levels(data.long$well))] <- "6b"

summary(data.long$well)

########## reformat date column to match formatting in watertable?
library(dplyr)
library(lubridate)
colnames(data.long)[colnames(data.long)=="TIMESTAMP"] <- "date.time"
data.long$date.time <- as.POSIXct(data.long$date.time, format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone())

data.long$date.time <- as.POSIXct(lubridate::force_tz(as.POSIXct(data.long$date.time), tz = "America/New_York"))

head(data.long); tail(data.long)
### data begins February 14th 2019 and ends at current date - data sometimes not available and 99999 given

#####################################################################################
### format like manually downloaded files -  Have barometric pressure at each diver #
#####################################################################################
data.long$date.time.well <- paste(data.long$date.time, data.long$well, sep = "_")

ele <- readxl::read_excel(path = "~/Dropbox (Smithsonian)/watertable_package/watertable/data-raw/soil_moisture_probe_and diver_details.xlsx",
                          sheet = 1)
#ele$A.Cable_Length_cm_2018 <- as.numeric(ele$A.Cable_Length_cm_2018)
ele$A.Cable_Length_cm_2020 <- as.numeric(ele$A.Cable_Length_cm_2020)
ele$pipe_height_cm <- ele$`Pipe Height From Ground_cm`
ele$well <- ele$probe

## to match with baro files with pattern "bh"
wells <- ele$well
ele$ele.probe <- paste(ele$elevation_meters, " m"," (", ele$well, ")",sep = "")
ele$ele.probe <- factor(ele$ele.probe, levels =
                          c("0.98 m (3a)", "1.7 m (6b)", "5.19 m (5f)", "8 m (4c)",
                            "8.32 m (2d)", "9.55 m (1e)", "8.14 m (NAbaro)"))

data.long.ele <- left_join(data.long, select(ele, elevation_meters, well,
                               ele.probe, `Pipe Height From Ground_cm`), by = "well")

#############################################################################
dat.filtered <- data.long.ele %>% mutate(water_head_cm = ifelse(water_head_cm > 600 |
                                                 water_head_cm < 0, NA, water_head_cm))

############################################################
##### probe depth using 2018 cable length until Feb. 13, 2019
### only using the 2020 length

datfil.2 <- left_join(dat.filtered %>% subset(date.time >= lubridate::force_tz(as.POSIXct("2019-02-14 00:00"), tz = "America/New_York")), select(ele, well, A.Cable_Length_cm_2020), by = "well")


datfil.2$probe.depth <- datfil.2$A.Cable_Length_cm_2020  -
  datfil.2$`Pipe Height From Ground_cm` + 11 # the length of the probe itself in cm

## the depth of the water table from the surface
datfil.2$water_head_cm <- as.numeric(as.character(datfil.2$water_head_cm))

datfil.3 <- datfil.2 %>% mutate(depth = probe.depth - water_head_cm,
                            date = as.Date(date.time),
                            well.dates = paste(well, date, sep = ".")) %>%
  subset(well != "baro" & depth > 0)


# to remove data on days loggers were downloaded??#
# remove NA's?#

###### Add GWL_m:  ground water level in meters
datfil.3$GWL_m <- datfil.3$elevation_meters - datfil.3$depth/100




################################################################################################
####################### repeat for 2021 data ###################################################
autdat21 <- subset(datfil.3, datfil.3$date.time >= "2021-01-01" & datfil.3$date.time < "2022-01-01")


