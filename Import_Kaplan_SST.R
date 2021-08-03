#Import Kaplan SST for JJAS 1895-2017

### Read in JJAS, DJF, Mar, Apr SST average

#Kaplan SST data info - total dataset
#grid: X (degree_east) periodic (27.5E) to (22.5E) by 5.0 N= 72 pts
#grid: Y (degree_north) ordered (87.5S) to (87.5N) by 5.0 N= 36 pts
#grid: T (months since 1960-01-01) ordered (Jan 1856) to (Oct 2017) by 1.0 N= 1942 pts

#Create variables designating the size of the data
nrows = 72
ncols = 36
nyears = 123
#nyears = 124
N = nrows*ncols

# Read Kaplan SST data
#data = readBin("./Kaplan-SST/Kaplan_JJAS_SST_anomalies_1895-2017.r4",what = "numeric",
#data = readBin("../PPR_Precip/Kaplan-SST/Kaplan_JJAS_SST_anomalies_1895-2017.r4",what = "numeric",               
#             n = ( nrows * ncols * nyears ), size = 4,endian = "swap")
data = readBin("./Kaplan-SST/Kaplan_DJF_SST_anomalies_1895-2018.r4",what = "numeric",
             n = ( nrows * ncols * nyears ), size = 4,endian = "swap")
#data = readBin("./Kaplan-SST/Kaplan_Mar_SST_anomalies_1895-2018.r4",what = "numeric",
#               n = ( nrows * ncols * nyears ), size = 4,endian = "swap")
#data = readBin("./Kaplan-SST/Kaplan_Apr_SST_anomalies_1895-2018.r4",what = "numeric",
#               n = ( nrows * ncols * nyears ), size = 4,endian = "swap")

data <- array(data = data, dim=c( nrows, ncols, nyears ) )

### lat-long grid - creates matrices containing corresponding lat/lon values for Kaplan
ygrid = seq(-87.5,87.5,by = 5)
ny = length(ygrid)

xgrid = seq(27.5,382.5,by=5)
nx = length(xgrid)

# Creates a 2 column matrix with all combinations of lat/lon points
xygrid = matrix(0,nrow = nx*ny,ncol = 2)
i=0
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

#Create index, find which indexes have data, then find which lat/lon combinations have data
# monthly averages seem to use NaN, whereas single months seem to have 1e+30 as a missing value
index = 1:(nx*ny)
index_notmiss = index[dataY1 != "NaN"]
#index_notmiss = index[dataY1 != dataY1[1,1]]
xygrid_notmiss = xygrid[index_notmiss, ]

# Store SST data
#Determine number of sites with data; preallocate matrix for SST
nsites = length(index_notmiss)
sstdata = matrix(NA, nrow = nyears, ncol = nsites)

#Loop through all years, store data for that year, find the locations with values, store as a row
for(i in 1:nyears){
  data_year = data[ , ,i]
  data_notmiss = data_year[index_notmiss]
  sstdata[i, ] = data_notmiss
}

#Remove imported dataset
rm("data")

