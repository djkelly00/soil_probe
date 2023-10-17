# Diver 2022 data

# Manual: data recorded hourly; well 4c off by 1 hour
### diver 4c recording 3 minutes early for the hourly measurement - round to next hour
### all divers have been downloaded manually this year as of Sept. 21st, but need to make sure earlier measurements are included - auto data has been running with hiccups

# Automatic: data every 15 minutes, some patchiness based on individual diver

# Plan:
# 1e - automated data - add in manual data from January
# 2d - automated data - add manual data for gaps in March and August
# 3a - automated data - one gap in June?
# 4c - automated data - need to shift and add manual data from Mar - May
# 5f - automated data spotty April - end of May - manual Jan - May
# 6b - manual data Jan - May; automatic data after May


################################################################################################
### compiled automated data from auto_network_data.R - read in from automated data file #########

autdat22 <- subset(datfil.3, datfil.3$date.time >= "2022-01-01" & datfil.3$date.time < "2023-01-01")
#write.csv(datfil.3, "C:/Users/jessh/Documents/GitHub/soil_probe/processed_data/automated_data_30_aug_2022.csv", row.names = FALSE)

## mean of water pressure to create daily data - mean of atm pressure as well or add in from NEON?
autdat22$time.interval <- cut(autdat22$date.time, breaks = "1 hour")

# summarise_at deprecated - look into across() and vignette("colwise")
autdat22.1 <- 
  autdat22 %>%
  group_by(well, time.interval) %>% 
  summarise_at(vars("WaterPressure_cmH2O", 
                    "Temp_C", 
                    "AtmPressure_cmH2O", 
                    "temp.SDI12", 
                    na.rm = all_of(T)), 
               mean)

#####################################################################################
### visualize atm pressure and water pressure at each well to check missing data ###
#####################################################################################

## subset data by diver
d1 <- subset(autdat22, well == "1e")
d2 <- subset(autdat22, well == "2d")
d3 <- subset(autdat22, well == "3a")
d4 <- subset(autdat22, well == "4c")
d5 <- subset(autdat22, well == "5f")
d6 <- subset(autdat22, well == "6b")

### plot atmospheric pressure ###
plot(d1$date.time, d1$AtmPressure_cmH2O, pch = 19, col = 'purple', main = "1e") # 2 outliers in May/June
# missing some data in Jan/Feb
### need to remove outliers:
rownames(d1) <- NULL
d1 <- d1[-c(8174, 5347), ] # strange readings - one in May and one in June removed

plot(d2$date.time, d2$AtmPressure_cmH2O, pch = 19, col = 'purple', main = "2d") 
# missing August

plot(d3$date.time, d3$AtmPressure_cmH2O, pch = 19, col = 'purple', main = "3a")

plot(d4$date.time, d4$AtmPressure_cmH2O, pch = 19, col = 'purple', main = "4c")
# missing Mar-May; Dec

plot(d5$date.time, d5$AtmPressure_cmH2O, pch = 19, col = 'purple', main = "5f") 
# missing Jan-Feb, some of April

plot(d6$date.time, d6$AtmPressure_cmH2O, pch = 19, col = 'purple', main = "6b") 
# issues earlier in the year


## plot water pressure - check for outliers and missing data
### need to remove outliers:
#rownames(d1) <- NULL
#d1 <- d1[-c(11210, 11211, 11253, 11257, 11261, 11265), ]

plot(d1$date.time, d1$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "1e") # missing Feb
plot(d2$date.time, d2$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "2d") # some gaps, Aug
plot(d3$date.time, d3$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "3a")
plot(d4$date.time, d4$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "4c") # gaps April, May, June, Dec
plot(d5$date.time, d5$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "5f") # gaps April - June
plot(d6$date.time, d6$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "6b") # missing prior to June

###################################################################################
######## Look at manual data - starting with dat2 #############################
######## from WaterTable_1_1.R and run through line 225 ##########################
###################################################################################

man.dat22 <- subset(dat2, dat2$date.time >= "2022-01-01" & dat2$date.time < "2023-01-01")
# write.csv(dat2, "C:/Users/jessh/Documents/GitHub/soil_probe/processed_data/manual_data_17_oct_2023.csv", row.names = FALSE)
###################################################################################
### diver 4 issue - times short of the hour mark - round to nearest hour
###############################################################################
md4 <- subset(dat2, dat2$date.time >= "2021-12-31 23:57:58" & dat2$date.time < "2023-01-01")
md4 <- subset(md4, well == "4c")

md4$date.time <- format(round(md4$date.time, units="hours"), format="%Y-%m-%d %H:%M:%S")

#### remove original 4c data from manual dataset - 16297 rows originally
man.dat22 <- subset(man.dat22, well != "4c") # 13024 rows

#### add back to man.dat22 - added one row to include midnight on 2021-01-01 - 16298 rows
man.dat22.1 <- rbind(man.dat22, md4)



## subset data by diver
dm1 <- subset(man.dat22.1, well == "1e")
dm2 <- subset(man.dat22.1, well == "2d")
dm3 <- subset(man.dat22.1, well == "3a")
dm4 <- subset(man.dat22.1, well == "4c")
dm5 <- subset(man.dat22.1, well == "5f")
dm6 <- subset(man.dat22.1, well == "6b")


plot(dm1$date.time, dm1$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "manual 1e")

plot(dm2$date.time, dm2$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "manual 2d")

plot(dm3$date.time, dm3$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "manual 3a") 

# One outlier in late April/May

plot(dm4$date.time, dm4$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "manual 4c") 

plot(dm5$date.time, dm5$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "manual 5f")

plot(dm6$date.time, dm6$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "manual 6b") 


###############################################################################
########## Add hourly barometric pressure ####################################
##############################################################################
#########################################################################

NEON.bp22 <- read.csv("C:/Users/jessh/Documents/GitHub/soil_probe/MET_data/NEON_hourly_bp_2022.csv", header = TRUE)

colnames(NEON.bp22) <- c("date.time", "AtmPressure_cmH2O")

NEON.bp22$date.time <- as.POSIXct(NEON.bp22$date.time, tz = "America/New_York", "%Y-%m-%d %H:%M:%S")

man.dat22.2 <- merge(man.dat22.1, NEON.bp22, by = "date.time", all.x = TRUE)


man.dat22.2$water_head_cm <- man.dat22.2$WaterPressure_cmH2O - man.dat22.2$AtmPressure_cmH2O

man.dat22.3 <- man.dat22.2 %>% mutate(water_head_cm = ifelse(water_head_cm > 600 |
                                                               water_head_cm < 0, NA, water_head_cm))


## add cable length 2020 in again...
dat2022 <- left_join(man.dat22.3, select(ele, well, A.Cable_Length_cm_2020), by = "well")

### add probe.depth, GWL_m, and depth
dat2022$probe.depth <- dat2022$A.Cable_Length_cm_2020 - dat2022$"Pipe Height From Ground_cm" + 11

dat2022$depth <- dat2022$probe.depth - dat2022$water_head_cm

dat2022$GWL_m <- dat2022$elevation_meters - dat2022$depth/100


### re-order to match 2018 and 2019, write.csv
dat2022.1 <- dat2022[ , c(1,4,2,3,9,10,14, 12, 13, 5)]

### add date column
dat2022.1$date <- as.Date(dat2022.1$date.time)

write.csv(dat2022.1, "C:/Users/jessh/Documents/GitHub/soil_probe/processed_data/data2022_17_oct_2023.csv", row.names = FALSE)



