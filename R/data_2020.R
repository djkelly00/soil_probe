# missing Jan 1, 2020 - Feb 26, 2020
#every 5 minutes starts Feb. 27th - Apr 6th; switches to measurement every hour after April 6th
#diver 4c on 5/21 begins at 9:57 - rounded to next hour
#Rutuja's code caused data to be eliminated and manually downloaded data to stop on September 1st

library(dplyr)

## Start from dat2 from WaterTable_1_1.R and run through line 225
## Missing data prior to 2/27/2020 and after 9/1/2020
man.dat20 <- subset(dat2, dat2$date.time >= "2020-01-01" & dat2$date.time < "2021-01-01")

man.dat20.1 <- left_join(man.dat20, subset(atm.P, select = c(date.time, AtmPressure_cmH2O)), by = "date.time")
man.dat20.1$water_head_cm <- man.dat20.1$WaterPressure_cmH2O - man.dat20.1$AtmPressure_cmH2O
man.dat20.2 <- man.dat20.1 %>% mutate(water_head_cm = ifelse(water_head_cm > 600 |
                                                           water_head_cm < 0, NA, water_head_cm))

###################################################################################
### diver 4 issue - 5/21 times short of the hour mark - round to nearest hour
###############################################################################
d4 <- subset(man.dat20, well == "4c")
mayd4 <- subset(d4, d4$date.time > "2020-05-21")
mayd4$date.time <- format(round(mayd4$date.time, units="hours"), format="%Y-%m-%d %H:%M:%S")

### add back to diver 4 data
d4 <- subset(d4, d4$date.time < "2020-05-21")
d4 <- rbind(d4, mayd4)

### remove date.time.well, atm. pressure, and water head
d4 <- d4[ , -5]
d4$date.time.well <- paste(d4$date.time, d4$well, sep = "_")

#### add back to man.dat20
man.dat20 <- subset(man.dat20, well != "4c")
man.dat20 <- rbind(man.dat20, d4)

### Now, re-add atmospheric data...lines 12-14


#####################################################################################
### visualize atm pressure and water pressure at each well to check missing data ###
##########3##########################################################################

## subset data by diver
d1 <- subset(man.dat20.2, well == "1e")
d2 <- subset(man.dat20.2, well == "2d")
d3 <- subset(man.dat20.2, well == "3a")
d4 <- subset(man.dat20.2, well == "4c")
d4.2 <- subset(best2020, well =="4c")
d5 <- subset(man.dat20.2, well == "5f")
d6 <- subset(man.dat20.2, well == "6b")

## plot atmospheric pressure - believe values are missing - can just have one set of pressure values for all wells? Once SDI12 is used we do have a value at each well...

#ends 9/1/2020 2 am
plot(d1$date.time, d1$AtmPressure_cmH2O, pch = 19, col = 'purple', main = "1e") 
plot(d2$date.time, d2$AtmPressure_cmH2O, pch = 19, col = 'purple', main = "2d")
plot(d3$date.time, d3$AtmPressure_cmH2O, pch = 19, col = 'purple', main = "3a") 
plot(d4$date.time, d4$AtmPressure_cmH2O, pch = 19, col = 'purple', main = "4c") 
plot(d4.2$date.time, d4.2$AtmPressure_cmH2O, pch = 19, col = 'purple', main = "4c")
plot(d5$date.time, d5$AtmPressure_cmH2O, pch = 19, col = 'purple', main = "5f")
plot(d6$date.time, d6$AtmPressure_cmH2O, pch = 19, col = 'purple', main = "6b")


# diver 3a and 6b were missing Apr 22 11 am - May 21 11 am; had to re-download manual data from 4/6 - 5/21 to folder and re-run the creation of dat2 in WaterTable_1_1.R through line 225


## plot water pressure - check for outliers and missing data
rownames(d1) <- NULL
### need to remove outliers - possibly from diver download day when diver was removed from well, but not listed as a diver download date in file
#4/6 10am - 11210, 11 am - 11211; 4/8 5 am - 11253, 9 am - 11257, 1 pm - 11261, 5 pm - 11265
d1 <- d1[-c(11210, 11211, 11253, 11257, 11261, 11265), ]
plot(d1$date.time, d1$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "1e")
### some outliers remain

plot(d2$date.time, d2$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "2d")
plot(d3$date.time, d3$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "3a")
plot(d4$date.time, d4$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "4c")
plot(d4.2$date.time, d4.2$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "4c")

# similar issue to diver 1 - values are suspiciously the same but one hour earlier...
rownames(d5) <- NULL
d5 <- d5[-c(11209, 11264, 11260, 11256, 11252, 11213, 11210), ]
plot(d5$date.time, d5$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "5f") 
### some outliers remain

plot(d6$date.time, d6$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "6b")

##################################################################################
### atmospheric pressure values stop in September - need to add from NEON
#################################################################################
# Barometric Pressure: DP1.00004.001

### re-calculate water_head_cm
#$water_head_cm <- $WaterPressure_cmH2O - $AtmPressure_cmH2O

########## auto network data failed in September - manual data from 5/21/2020 - 12/31/2020
## no atmospheric pressure in manual data and need to add




################################################################################################
####################### repeat for automatically downloaded 2020 data #####################
#################################################################################################
## subset data from 2019 only from auto_network_data.R
autdat20 <- subset(datfil.3, datfil.3$date.time >= "2020-01-01" & datfil.3$date.time < "2021-01-01")


#### no data except for diver 4 - January 1, 00:00 - February 13, 00:15
autdat20.values <- subset(autdat20, autdat20$Temp_C > 0) # subset to data with values - removing NA's wasn't working with other code options
autdat20.values <- autdat20.values[ , c(1, 7, 8, 6, 12:15, 9, 11 )]

### add small auto data from diver 4 to the manually downloaded data
best2020 <- rbind(man.dat20.2, autdat20.values)

# save just in case
write.csv(best2020, "C:/Users/jessh/Documents/GitHub/soil_probe/processed_data/data2020_24_aug_2022.csv", row.names = FALSE)
###########################################################################################
########## Subset best 2020 to data without atmospheric pressure #########################
###########################################################################################
best2020$WaterPressure_cmH2O <- as.numeric(as.character(best2020$WaterPressure_cmH2O))
best2020$AtmPressure_cmH2O <- as.numeric(as.character(best2020$AtmPressure_cmH2O))

# order by date/time
best2020.sequential <- best2020[order(best2020$date.time),]
rownames(best2020.sequential) <- NULL

# data with atmospheric pressure
best2020.prs <- best2020.sequential[!is.na(best2020.sequential$AtmPressure_cmH2O), ]


# recalculate water head = water pressure cm H2O - baro pressure cm H2O
# did Rutuja remove (-) water head values?
best2020.prs$water_head_cm <- best2020.prs$WaterPressure_cmH2O - best2020.prs$AtmPressure_cmH2O

#########################################################################
##### data to add atmospheric pressure - as of 2020-09-01 02:00:00 (hourly)
best2020.wo.prs <- best2020.sequential[is.na(best2020.sequential$AtmPressure_cmH2O),]

NEON.bp <- read.csv("C:/Users/jessh/Documents/GitHub/soil_probe/MET_data/NEON_hourly_bp_2020.csv", header = TRUE)
colnames(NEON.bp) <- c("date.time", "AtmPressure_cmH2O")
NEON.bp$date.time <- as.POSIXct(NEON.bp$date.time, tz = "America/New_York", "%Y-%m-%d %H:%M:%S")

best2020.add.prs <- merge(best2020.wo.prs, NEON.bp, by = "date.time", all.x = TRUE)
best2020.add.prs <- best2020.add.prs[ , -c(6,7,9,10)]
colnames(best2020.add.prs)[7] <- "AtmPressure_cmH2O"

best2020.add.prs$water_head_cm <- best2020.add.prs$WaterPressure_cmH2O - best2020.add.prs$AtmPressure_cmH2O

### merge the added pressure data with the rest of the data

### add probe.depth, GWL_m, and depth
datfil.2 <- left_join(dat.filtered %>% subset(date.time >= lubridate::force_tz(as.POSIXct("2019-02-14 00:00"), tz = "America/New_York")), select(ele, well, A.Cable_Length_cm_2020), by = "well")

probe.depth <- $A.Cable_Length_cm_2020 - $Pipe Height From Ground_cm + 11

depth <- probe.depth - water_head_cm

GWL_m <- elevation_meters - depth/100



[ , c(1,4,2,3,9,10,GWL, probe depth, depth, 5, date)]












#######################################################################################################
#Extra, not needed
#########################################################################################################
#man.dat20 <- man.dat20[ , -c(6:8, 14:16, 18:23)]

### merge unique data only, no NA's? Combine by unique date.time.well?
#autdat20.2 <- subset(autdat20.1, autdat20.1$WaterPressure_cmH2O != "NAN")
man.dat20.1 <- na.omit(man.dat20)

### fix column type
man.dat20.1$date.time <- as.POSIXct(man.dat20.1$date.time, tz = "America/New_York", "%Y-%m-%d %H:%M:%S")
man.dat20.1$date <- as.Date(man.dat20.1$date.time)
autdat20.2$WaterPressure_cmH2O <- as.double(as.character(autdat20.2$WaterPressure_cmH2O), digits = 7)
autdat20.2$Temp_C <- as.numeric(as.character(autdat20.2$Temp_C))
autdat20.2$AtmPressure_cmH2O <- as.numeric(as.character(autdat20.2$AtmPressure_cmH2O))

### find common rows
dat_common <- generics::intersect(autdat20.2, man.dat20.1)

data2020 <- merge(autdat20.2, man.dat20.1, all = TRUE)
########### missing data after September 1st, 2020 due to time/atmospheric pressure issues? files are available...

data2019 <- data2019[ , c(1:6, 11, 8,9,7,10)]
write.csv(data2019, "C:/Users/jessh/Documents/GitHub/soil_probe/processed_data/data2019_21_jun_2022.csv", row.names = FALSE)


