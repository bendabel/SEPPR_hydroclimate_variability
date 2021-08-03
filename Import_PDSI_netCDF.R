#Import netCDF file
#PDSI
library(tidyverse)
library(ncdf4)
library(ncdf4.helpers)
library(PCICt)

#Open netCDF file for reading
#PDSI_nc <- nc_open("./NCEI_CAG_Data/Climate_Divisions/PDSI/pdsi.mon.mean.selfcalibrated.nc")
PDSI_nc <- nc_open("../PPR_Precip/NCEI_CAG_Data/Climate_Divisions/PDSI/pdsi.mon.mean.selfcalibrated.nc")

#Retrieve lat and lon
lat <- ncvar_get(PDSI_nc, varid = "lat")
lon <- ncvar_get(PDSI_nc, varid = "lon")

#Determine the time format and whether it uses leap years
PDSI_nc$dim$time$units
PDSI_nc$dim$time$calendar

#Read in the time and pdsi value; display start and end dates
pdsi_time <- nc.get.time.series(PDSI_nc, v = "pdsi",
                               time.dim.name = "time")
pdsi_time[c(1, length(pdsi_time))]
pdsi_time <- as.character.PCICt(pdsi_time)
pdsi_time <- as.Date(pdsi_time)
pdsi <- ncvar_get(PDSI_nc, "pdsi")

#Select specific years
pdsi_time_1895_2014 <- pdsi_time[541:1980]
pdsi_1895_2014 <- pdsi[ , ,541:1980]

#Find summer average and place it in new array
years <- c(1895:2014)
nyears <- length(years)
x <- seq(-178.75, 178.75, by = 2.5 )
nx <- length(x)
y <- seq(-58.75, 76.25, by = 2.5 )
ny <- length(y)
nmonths <- length(pdsi_time_1895_2014)

pdsi_JJAS_avg <- array(dim=c( nx, ny, nyears ))

for(iyear in 1:nyears){
  for(iy in 1:ny){
    for(ix in 1:nx){
      pdsi_JJAS_avg[ix, iy, iyear] <-
        mean( c(pdsi_1895_2014[ix, iy, 6+((iyear-1)*12)], pdsi_1895_2014[ix, iy, 7+((iyear-1)*12)],
                pdsi_1895_2014[ix, iy, 8+((iyear-1)*12)], pdsi_1895_2014[ix, iy, 9+((iyear-1)*12)]) )
    }
  }
}

# Creates a 2 column matrix with all combinations of lat/lon points
xygrid = matrix(0, nrow = nx*ny, ncol = 2)
i=0
for(iy in 1:ny){
  for(ix in 1:nx){
    i=i+1
    xygrid[i,1] = y[iy]
    xygrid[i,2] = x[ix]
  }
}

# Determine which lat/lon locations have data
#Read in last year of dataset
dataY1 <- pdsi_JJAS_avg[ , ,1]

#Create index, find which indexes have data, then find which lat/lon combinations have data
index = 1:(nx*ny)
index_notmiss = index[is.na(dataY1) == FALSE]
xygrid_notmiss = xygrid[index_notmiss, ]

# Store data
#Determine number of sites with data; preallocate matrix for PDSI
nsites = length(index_notmiss)
pdsidata = matrix(NA, nrow = nyears, ncol = nsites)

#Loop through all years, store data for that year, find the locations with values, store as a row
for(i in 1:nyears){
  data_year = pdsi_JJAS_avg[ , ,i]
  data_notmiss = data_year[index_notmiss]
  pdsidata[i, ] = data_notmiss
}

### other code to examine dataset
#####
# #Look at dimensions of pdsi; should be lon, lat, time
# dim(pdsi_1895_2014)
# 
# #Define indexes to select specific data
# lon_index <- which.min(abs(lon - -100.81))
# lat_index <- which.min(abs(lat - 46.65))
# time_index <- which(format(pdsi_time, "%Y-%m-%d") == "2000-07-01")
# 
# #Display specific data
# pdsi[lon_index, lat_index, time_index]
# 
# #Choose a specific month by using only time_index
# july_2000 <- nc.get.var.subset.by.axes(PDSI_nc, "pdsi",
#                                        axis.indices = list(T = time_index))
# july_2000 <- as_tibble(july_2000)
# 
# #Choose one location and get all months
# SD_CD8 <- nc.get.var.subset.by.axes(PDSI_nc, "pdsi",
#                                  axis.indices = list(X = lon_index,
#                                                      Y = lat_index))
# SD_CD8 <- tibble("time" = pdsi_time, "PDSI" = SD_CD8[1,1,])
# 
# #Filter by months
# SD_CD8[months(SD_CD8$time) %in% month.name[6:9],]
# 
# #Plot a time series of PDSI at one location
# SD_CD8 <- SD_CD8 %>% 
#   filter(PDSI != "NA")
# SD_CD8 %>%
#   ggplot(aes(x = time, y = PDSI)) + 
#   geom_line() + 
#   xlab("Date") + ylab("PDSI") + 
#   ggtitle("Monthly PDSI",
#           subtitle = "At location nearest to SD_CD8")
