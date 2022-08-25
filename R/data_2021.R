# Diver 2021 data

# Manual: data recorded hourly; well 4c off by 1 hour
### diver 4c recording 3 minutes early for the hourly measurement - round to next hour

# Automatic: data every 15 minutes, didn't come back online until August 2021 - October for diver 1e; manual data may be better in this instance.
### 1e - October values odd with data missing prior to 2021-10-2 13:45
### 2d and 6b spotty - values popping in and out due to cable issue
### 4c missing some values in December


###################################################################################
######## Look at manual data - starting with dat2 #############################
######## from WaterTable_1_1.R and run through line 225 ##########################
###################################################################################

man.dat21 <- subset(dat2, dat2$date.time >= "2021-01-01" & dat2$date.time < "2022-01-01")

###################################################################################
### diver 4 issue - 5/21 times short of the hour mark - round to nearest hour
###############################################################################
md4 <- subset(dat2, dat2$date.time >= "2020-12-31 23:57:58" & dat2$date.time < "2022-01-01")
md4 <- subset(md4, well == "4c")

md4$date.time <- format(round(md4$date.time, units="hours"), format="%Y-%m-%d %H:%M:%S")

#### remove original 4c data from manual dataset - 49437 rows originally
man.dat21 <- subset(man.dat21, well != "4c") # 40679 rows

#### add back to man.dat21 - added one row to include midnight on 2021-01-01 - 49438 rows
man.dat21.1 <- rbind(man.dat21, md4)

#####################################################################################
### visualize atm pressure and water pressure at each well to check missing data ###
#####################################################################################

## subset data by diver
d1 <- subset(man.dat21.1, well == "1e")
d2 <- subset(man.dat21.1, well == "2d")
d3 <- subset(man.dat21.1, well == "3a")
d4 <- subset(man.dat21.1, well == "4c")
d5 <- subset(man.dat21.1, well == "5f")
d6 <- subset(man.dat21.1, well == "6b")

### no atmospheric pressure data with the manual data ###

## plot water pressure - check for outliers and missing data
### need to remove outliers:
#rownames(d1) <- NULL
#d1 <- d1[-c(11210, 11211, 11253, 11257, 11261, 11265), ]

plot(d1$date.time, d1$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "1e")
plot(d2$date.time, d2$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "2d")
plot(d3$date.time, d3$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "3a")
plot(d4$date.time, d4$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "4c")
plot(d5$date.time, d5$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "5f")
plot(d6$date.time, d6$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "6b") # some gaps after Aug

###############################################################################
########## Add hourly barometric pressure ####################################
##############################################################################
#########################################################################

NEON.bp <- read.csv("C:/Users/jessh/Documents/GitHub/soil_probe/MET_data/NEON_hourly_bp_2021.csv", header = TRUE)
colnames(NEON.bp) <- c("date.time", "AtmPressure_cmH2O")
NEON.bp$date.time <- as.POSIXct(NEON.bp$date.time, tz = "America/New_York", "%Y-%m-%d %H:%M:%S")

man.dat21.2 <- merge(man.dat21.1, NEON.bp, by = "date.time", all.x = TRUE)


man.dat21.2$water_head_cm <- man.dat21.2$WaterPressure_cmH2O - man.dat21.2$AtmPressure_cmH2O

man.dat21.3 <- man.dat21.2 %>% mutate(water_head_cm = ifelse(water_head_cm > 600 |
                                       water_head_cm < 0, NA, water_head_cm))


## add cable length 2020 in again...
dat2021 <- left_join(man.dat21.3, select(ele, well, A.Cable_Length_cm_2020), by = "well")

### add probe.depth, GWL_m, and depth
dat2021$probe.depth <- dat2021$A.Cable_Length_cm_2020 - dat2021$"Pipe Height From Ground_cm" + 11

dat2021$depth <- dat2021$probe.depth - dat2021$water_head_cm

dat2021$GWL_m <- dat2021$elevation_meters - dat2021$depth/100


### re-order to match 2018 and 2019, write.csv
dat2021.1 <- dat2021[ , c(1,4,2,3,9,10,14, 12, 13, 5)]

### add date column
dat2021.1$date <- as.Date(dat2021.1$date.time)

write.csv(dat2021.1, "C:/Users/jessh/Documents/GitHub/soil_probe/processed_data/data2021_25_aug_2022.csv", row.names = FALSE)





################################################################################################
################# compiled automated data from auto_network_data.R###################
autdat21 <- subset(datfil.3, datfil.3$date.time >= "2021-01-01" & datfil.3$date.time < "2022-01-01")
