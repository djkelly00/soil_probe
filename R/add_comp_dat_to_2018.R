dat1.1
### water head = Obs_Well_Elevation_m - Depth of water table_cm/100
### renamed to GWL_cm
### Ground_water_level_m = Obs_Well_Elevation_m - Depth of water table_cm/100

### temp.depth = dat1$elevation_meters*100 - dat1$GWL_cm/100

wt18 <- read.csv("C:/Users/jessh/Documents/GitHub/soil_probe/processed_data/watertable_2018.csv", header = TRUE)

wt18$date <- as.POSIXct(wt18$date, format =  "%Y-%m-%d")

w1 <- subset(wt18, well == "1e")
plot(w1$date, w1$WaterPressure_cmH2O, main = "Well 1e Water Pressure cm H2O")
identify(w1$date, w1$WaterPressure_cmH2O)
w1.outliers <- w1[c(14338, 17511, 32526, 60753), ] #dates data downloaded manually; not removed by Rutuja's code somehow?? lines 296 removed these dates I thought...



w2 <- subset(wt18, well == "2d")
plot(w2$date, w2$WaterPressure_cmH2O, main = "Well 1e Water Pressure cm H2O")


w3 <- subset(wt18, well == "3a")
plot(w1$date, w1$WaterPressure_cmH2O, main = "Well 1e Water Pressure cm H2O")

w4 <- subset(wt18, well == "4c")
plot(w1$date, w1$WaterPressure_cmH2O, main = "Well 1e Water Pressure cm H2O")

w5 <- subset(wt18, well == "5f")
plot(w1$date, w1$WaterPressure_cmH2O, main = "Well 1e Water Pressure cm H2O")


w6 <- subset(wt18, well == "6b")
plot(w1$date, w1$WaterPressure_cmH2O, main = "Well 1e Water Pressure cm H2O")

