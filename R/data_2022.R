# Diver 2022 data

# Manual: data recorded hourly; well 4c off by 1 hour
### diver 4c recording 3 minutes early for the hourly measurement - round to next hour
### not all divers have been downloaded manually this year as of August 30th - auto data has been running

# Automatic: data every 15 minutes, some patchiness based on individual diver

# Plan:
# 1e - automated data - add in manual data from January
# 2d - automated data - add manual data for gaps in March and August
# 3a - automated data - one gap in June?
# 4c - shift automated - add manual data from Mar - May
# 5f - automated data spotty April - end of May - manual Jan - May
# 6b - manual data Jan - May; automatic data after May


################################################################################################
### compiled automated data from auto_network_data.R - read in from autmated data file #########

autdat22 <- subset(datfil.3, datfil.3$date.time >= "2022-01-01" & datfil.3$date.time < "2023-01-01")
#write.csv(datfil.3, "C:/Users/jessh/Documents/GitHub/soil_probe/processed_data/automated_data_30_aug_2022.csv", row.names = FALSE)

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

plot(d2$date.time, d2$AtmPressure_cmH2O, pch = 19, col = 'purple', main = "2d") # missing August
plot(d3$date.time, d3$AtmPressure_cmH2O, pch = 19, col = 'purple', main = "3a")
plot(d4$date.time, d4$AtmPressure_cmH2O, pch = 19, col = 'purple', main = "4c") # missing Mar-May
plot(d5$date.time, d5$AtmPressure_cmH2O, pch = 19, col = 'purple', main = "5f") # missing Jan, some of April
plot(d6$date.time, d6$AtmPressure_cmH2O, pch = 19, col = 'purple', main = "6b") # issues earlier in the year


## plot water pressure - check for outliers and missing data
### need to remove outliers:
#rownames(d1) <- NULL
#d1 <- d1[-c(11210, 11211, 11253, 11257, 11261, 11265), ]

plot(d1$date.time, d1$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "1e") # missing Feb
plot(d2$date.time, d2$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "2d") # some gaps, Aug
plot(d3$date.time, d3$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "3a")
plot(d4$date.time, d4$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "4c") # gaps April, May, June
plot(d5$date.time, d5$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "5f") # gaps April - June
plot(d6$date.time, d6$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "6b") # missing prior to June

###################################################################################
######## Look at manual data - starting with dat2 #############################
######## from WaterTable_1_1.R and run through line 225 ##########################
###################################################################################

man.dat22 <- subset(dat2, dat2$date.time >= "2022-01-01" & dat2$date.time < "2023-01-01")
#write.csv(dat2, "C:/Users/jessh/Documents/GitHub/soil_probe/processed_data/manual_data_30_aug_2022.csv", row.names = FALSE)

## subset data by diver
dm1 <- subset(man.dat22, well == "1e")
dm2 <- subset(man.dat22, well == "2d")
dm3 <- subset(man.dat22, well == "3a") ### not downloaded
dm4 <- subset(man.dat22, well == "4c")
dm5 <- subset(man.dat22, well == "5f")
dm6 <- subset(man.dat22, well == "6b")


plot(dm1$date.time, dm1$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "manual 1e") # only Jan-Feb
plot(dm2$date.time, dm2$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "manual 2d")
plot(dm3$date.time, dm3$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "manual 3a") # not downloaded
plot(dm4$date.time, dm4$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "manual 4c") # ends June
plot(dm5$date.time, dm5$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "manual 5f") # ends June
plot(dm6$date.time, dm6$WaterPressure_cmH2O, pch = 19, col = 'blue', main = "manual 6b") # ends June
