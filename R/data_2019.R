################################################
### combine auto_network_data.R with manually downloaded 2019 data ##
###############################################
man.dat19 <- read.csv("C:/Users/jessh/Documents/GitHub/soil_probe/processed_data/archive/watertable_2019.csv", header = TRUE)

## subset data from 2019 only from auto_netowrk_data.R (to line ###
autdat19 <- subset(datfil.3,  datfil.3$date.time >= "2019-01-01" & datfil.3$date.time < "2020-01-01")

### remove unnecessary columns ###
autdat19.1 <- autdat19[ , -c(2:5, 10, 13:16, 20)]

man.dat19.1 <- man.dat19[ , -c(6:8, 14:16, 18:23)]

### merge unique data only, no NA's? Combine by unique date.time.well?
autdat19.2 <- subset(autdat19.1, autdat19.1$WaterPressure_cmH2O != "NAN")
man.dat19.2 <- na.omit(man.dat19.1)


### fix column type
man.dat19.2$date.time <- as.POSIXct(man.dat19.2$date.time, tz = "America/New_York", "%m/%d/%Y %H:%M")
man.dat19.2$date <- as.Date(man.dat19.2$date.time)
autdat19.2$WaterPressure_cmH2O <- as.double(as.character(autdat19.2$WaterPressure_cmH2O), digits = 7)
autdat19.2$Temp_C <- as.numeric(as.character(autdat19.2$Temp_C))
autdat19.2$AtmPressure_cmH2O <- as.numeric(as.character(autdat19.2$AtmPressure_cmH2O))

### find common rows
dat_common <- generics::intersect(autdat19.2, man.dat19.2)  ## 0 columns in common between the automatically downloaded data and the manually downloaded data...manual data downloaded prior to joining to auto network

#dat_diff <- anti_join(autdat19.2, man.dat19.2)

data2019 <- merge(autdat19.2, man.dat19.2, all = TRUE)
data2019$date[data2019$date == as.Date("2020-01-01")] <- "2019-12-31"

data2019 <- data2019[ , c(1:6, 11, 8,9,7,10)]
write.csv(data2019, "C:/Users/jessh/Documents/GitHub/soil_probe/processed_data/data2019_21_jun_2022.csv", row.names = FALSE)
