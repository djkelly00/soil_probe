install.packages("neonUtilities")
#install.packages("BiocManager")
#BiocManager::install('rhdf5')
library(neonUtilities)

options(stringsAsFactors=F)

##### We want a file for each year of barometric pressure - every 30 minutes
##### Code below exports 12 folders, one for each month
#### Export and append to one file for the entire year


### 2018
zipsByProduct(dpID="DP1.00004.001", package="basic", 
              site="SERC", 
              startdate="2018-01", enddate="2018-12",
              savepath="~/Dropbox (Smithsonian)/watertable_package_JS",
              tabl =  
              check.size=F)

stackByTable(filepath = "~/Dropbox (Smithsonian)/watertable_package_JS/filesToStack0004/NEON.D02.SERC.DP1.00004.001.2018-01.basic.20210123T023002Z.RELEASE-2021.zip")

loadByProduct(dpID="DP1.00004.001", package="basic", 
              site="SERC", 
              startdate="2018-01", enddate="2018-12",
              tabl = "NEON.D02.SERC.DP1.00004.001.000.025.030.BP_30min.2018-01.basic.20200621T113753Z.csv",
              check.size=F)


### 2019
zipsByProduct(dpID="DP4.00200.001", package="basic", 
              site="SERC", 
              startdate="2019-01", enddate="2019-12",
              savepath="~/Downloads", 
              check.size=F)
### 2020
zipsByProduct(dpID="DP4.00200.001", package="basic", 
              site="SERC", 
              startdate="2020-01", enddate="2020-12",
              savepath="~/Downloads", 
              check.size=F)


###########################################################
################### flux data examples
################################################################
flux.2018 <- stackEddy(filepath="~/Downloads/filesToStack00200/may_2018",
                  level="dp04")
flux.2019 <- stackEddy(filepath="~/Downloads/filesToStack00200/may_2019",
                     level="dp04")
flux.2020 <- stackEddy(filepath="~/Downloads/filesToStack00200/may_2020",
                       level="dp04")

### plot downloaded 2018 data to see what it looks like
plot(flux2.2018$data.fluxCo2.nsae.flux~flux2.2018$timeBgn, 
     pch=".", xlab="Date", ylab="CO2 flux")
plot(flux2.2018$data.fluxCo2.nsae.flux~flux2.2018$Time, 
     pch=".", xlab="Date", ylab="CO2 flux")

########################################################################################
##### SERC data with time zone adjusted to EST
library(lubridate)

### subset to just the necessary data
flux2.2018 <- flux.2018$SERC
write.csv(flux2.2018, "~/Dropbox (Smithsonian)/NEON_flux_data/flux_2018.csv", row.names = FALSE)

### attempt to change the time zone; doesn't appear to work...
flux2.2018$Time <- as.POSIXct(force_tz(flux2.2018$timeBgn, tzone = "America/New_York"))
### the time itself does not change, but it shifts the data...12 am UTC = 8 pm EDT; should move the data back from 5/10/2018 12 am to 5/09/2018 8 pm but seems to shift the data forward...

#######################################
### plot flux data for May 2018
test.2018 <- read.csv("~/Dropbox (Smithsonian)/NEON_flux_data/flux_2018_EDT.csv", header = TRUE)

test.2018$EDT <- as.POSIXct(paste(test.2018$date, test.2018$time), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
par()
plot(test.2018$data.fluxCo2.nsae.flux~test.2018$EDT, type = "o",
     pch=20, xlab="Date", ylab = "",
     xlim=c(as.POSIXct("2018-05-10", tz="UTC"),
            as.POSIXct("2018-05-13", tz="UTC")),
     ylim=c(-50,50), xaxt= "n", yaxt= "n")

abline(h = 0)
axis(2, seq(-40, 40, 20), las=2)
### add x-axis labels 
axis.POSIXct(1, x=test.2018$EDT, 
             format="%m-%d", labels = TRUE)
#time.x <- c("6 am", "8 pm", "6 am", "8 pm", "6 am", "8 pm")
#axis(3, at = c(1525945965, 1525995974, 1526032429, 1526082437, 1526118892, 1526168901), labels = time.x)

### hack a way to show day and night transition
locator()
time.x <- c("5 am", "7 pm", "5 am", "7 pm", "5 am", "7 pm")
axis(3, at = c(1525942694, 1525993170, 1526028690, 1526079166, 1526115153, 1526165629), labels = time.x)
# 6 am:  1525945965, 1526032429, 1526118892
abline(v=as.numeric(test.2018$EDT[19]), lwd=2, col='gold')
abline(v=as.numeric(test.2018$EDT[47]), lwd=2, col='darkblue')

abline(v=as.numeric(test.2018$EDT[67]), lwd=2, col='gold')
abline(v=as.numeric(test.2018$EDT[95]), lwd=2, col='darkblue')

abline(v=as.numeric(test.2018$EDT[115]), lwd=2, col='gold')
abline(v=as.numeric(test.2018$EDT[143]), lwd=2, col='darkblue')

identify(test.2018$EDT, test.2018$data.fluxCo2.nsae.flux, test.2018$EDT)


### plot data for May 2020
plot(flux.2020$SERC$data.fluxCo2.nsae.flux~flux.2020$SERC$timeBgn, type = "o",
     pch=20, xlab="Date", ylab="CO2 flux umolCo2 m-2 s-1",
     xlim=c(as.POSIXct("2020-05-10", tz="GMT"),
            as.POSIXct("2020-05-13", tz="GMT")),
     ylim=c(-50,50), main= "May 2020 NEON Tower" )
abline(h=0)

### add x-axis labels
dates.2020 <- flux.2020$SERC[flux.2020$SERC$timeBgn >= "2020-05-10" & flux.2020$SERC$timeBgn < "2020-05-13", ]
dates.2020 <- dates.2020[ , 1]
axis.POSIXct(1, x=dates.2020, 
             format="%m-%d", labels = TRUE)
abline(v=as.POSIXct.Date(05-10))
