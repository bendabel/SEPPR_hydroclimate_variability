#Import NOAA NCEP-NCAR CDAS-1 500 mb heights for JJAS 1949-2017

### Read in JJAS 500mb height average

#CDAS-1 500mb height data info - total dataset
#grid: X (degree_east) periodic (0) to (2.5W) by 2.5 N= 144 pts
#grid: Y (degree_north) ordered (90N) to (90S) by 2.5 N= 73 pts
#grid: P (mb) ordered [ (500)] :grid
#grid: T (months since 1960-01-01) ordered (Jun-Sep 1949) to (Jun-Sep 2017) by 12.0 N= 69 pts

# IRI expert mode codes to pull desired data; second code is newer and more compact
#SOURCES .NOAA .NCEP-NCAR .CDAS-1 .MONTHLY .Intrinsic .PressureLevel .phi
#P (500) (500) RANGEEDGES
#T (Jun 1949) (Dec 2017) RANGE
#T 4 boxAverage
#T 12 STEP

#SOURCES .NOAA .NCEP-NCAR .CDAS-1 .MONTHLY .Intrinsic .PressureLevel .phi
#P (500) VALUES
#T (Dec-Feb) seasonalAverage

#Create variables designating the size of the data
nrows = 144
ncols = 73
nyears = 69
#nyears = 70
N = nrows*ncols

# Read CDAS-1 500mb height data for 1949-2017
data = readBin("./500mb/CDAS_JJAS_500mb_Heights_1949-2017.r4",what = "numeric",
             n = ( nrows * ncols * nyears ), size = 4,endian = "swap")
#data = readBin("./500mb/CDAS_DJF_500mb_Heights_1950-2018.r4",what = "numeric",
#               n = ( nrows * ncols * nyears ), size = 4,endian = "swap")
#data = readBin("./500mb/CDAS_Mar_500mb_Heights_1949-2018.r4",what = "numeric",
#               n = ( nrows * ncols * nyears ), size = 4,endian = "swap")
#data = readBin("./500mb/CDAS_Apr_500mb_Heights_1949-2018.r4",what = "numeric",
#               n = ( nrows * ncols * nyears ), size = 4,endian = "swap")

data <- array(data = data, dim=c( nrows, ncols, nyears ) )

### lat-long grid - creates matrices containing corresponding lat/lon values for CDAS-1
ygrid = seq(-90.0, 90.0, by = 2.5)
ny = length(ygrid)

xgrid = seq(0, 357.5, by = 2.5)
nx = length(xgrid)

# Creates a 2 column matrix with all combinations of lat/lon points
xygrid = matrix(0,nrow = nx*ny,ncol = 2)
i = 0
for(iy in 1:ny){
  for(ix in 1:nx){
    i=i+1
    xygrid[i,1]=ygrid[iy]
    xygrid[i,2]=xgrid[ix]
  }
}

# Determine which lat/lon locations have data
#Read in first year of dataset
dataY1 <- data[ , ,1]

#Create index, find which indices have data, then find which lat/lon combinations have data
index = 1:(nx*ny)
index_notmiss = index[dataY1 != "NaN"]
xygrid_notmiss = xygrid[index_notmiss, ]

#Store 500mb data
#Determine number of sites with data; preallocate matrix for 500mb heights
nsites = length(index_notmiss)
h500mbdata = matrix(NA, nrow = nyears, ncol = nsites)

#Loop through all years, store data for that year, find the locations with values, store as a row
for(i in 1:nyears){
  data_year = data[ , ,i]
  data_notmiss = data_year[index_notmiss]
  h500mbdata[i, ] = data_notmiss
}

#Remove imported dataset
rm("data")

# Generate a plot with the values, add contours, then add land mass boundaries
#image.plot(xgrid, ygrid, dataY1, xlim = range(0,360), ylim = range(-20,80))
#contour(xgrid, ygrid, (dataY1), ylim = range(-20,80), add = TRUE, nlev = 10, lwd = 2)
#world(add = TRUE, wrap = c(0,360))#, fill = TRUE, col="grey")
