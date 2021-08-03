#plot figures downloaded from PSL's composite plotter
library(tidyverse)
library(maps)

#import data
pc <- 1
yr_type <- "Wet"
plot_var <- "PDSI"

path <- "./PCA_results/composite_plot_data/"
file <- paste0("PCA_precip_JJAS_PC", pc, "_", tolower(yr_type), "_", plot_var, "_data.txt")
data_plot <- read_delim(paste0(path, file),
                        skip = 10, delim = " ", col_names = F, col_types = cols(.default = "n"))

#import wind data if plotting height
if(plot_var == "500mb" | plot_var == "850mb") {
  file2 <- paste0("PCA_precip_JJAS_PC", pc, "_", tolower(yr_type), "_", plot_var, "_wind_u_data.txt")
  data_plot2 <- read_delim(paste0(path, file2),
                           skip = 10, delim = " ", col_names = F, col_types = cols(.default = "n"))
  file3 <- paste0("PCA_precip_JJAS_PC", pc, "_", tolower(yr_type), "_", plot_var, "_wind_v_data.txt")
  data_plot3 <- read_delim(paste0(path, file3),
                           skip = 10, delim = " ", col_names = F, col_types = cols(.default = "n"))
}

## Create lat, lon data
#GeoHeight, u and v wind: 90.0N,90.0S ; 0E,357.5E
# 2.5 degree increments both lat, lon
#SST: 88.0N,88.0S ; 0E,358.0E
# 2.0 degree increments both lat, lon
#PDSI: 76.25N,58.75S ; -178.75W,178.75
# 2.5 degree increments both lat, lon
if(plot_var == "500mb" | plot_var == "850mb") {
  lat_st <- 90.0; lat_end <- -90.0
  lon_st <- 0; lon_end <- 357.5
  increment <- 2.5
} else if(plot_var == "SST") {
  lat_st <- 88.0; lat_end <- -88.0
  lon_st <- 0; lon_end <- 358.0
  increment <- 2.0
} else if(plot_var == "PDSI") {
  lat_st <- 76.25; lat_end <- -58.75
  lon_st <- -178.75; lon_end <- 178.75
  increment <- 2.5
}

#create lat and lon sequences and preallocate matrix of data
lat <- seq(lat_st, lat_end, -increment)
lon <- seq(lon_st, lon_end, increment)
if(plot_var == "500mb" | plot_var == "850mb") { ncols <- 5 } else { ncols <- 3 }
data_lat_lon <- matrix(NA, nrow = length(lat) * length(lon), ncol = ncols)

#create matrix of data
ind <- 1
for(i in 1:length(lat)) {
  for(j in 1:length(lon)) {
    data_lat_lon[ind, 1] <- lat[i]
    data_lat_lon[ind, 2] <- lon[j]
    if(data_plot[[i, j]] > -100) {
      data_lat_lon[ind, 3] <- data_plot[[i, j]]
    }
    if(plot_var == "500mb" | plot_var == "850mb") {
      if(data_plot2[[i, j]] > -100) {
        data_lat_lon[ind, 4] <- data_plot2[[i, j]]
      }
      if(data_plot3[[i, j]] > -100) {
        data_lat_lon[ind, 5] <- data_plot3[[i, j]]
      }
    }
    
    ind <- ind+1
  }
}

#convert to tibble and change column names
data_lat_lon <- as_tibble(data_lat_lon)
if(plot_var == "500mb" | plot_var == "850mb") { col_nms <- c("lat", "lon", "val", "u", "v") } else {
  col_nms <-c("lat", "lon", "val")
}
colnames(data_lat_lon) <- col_nms

### Plot
## the below code creates a world map that allows zooming on the Pacific Ocean
{
  world_map <- data.frame(map(plot = F)[c("x", "y")]); names(world_map) = c("lon", "lat")
  world_map <- within(world_map, {
    lon = ifelse(lon < 0, lon + 360, lon)
  })
  mp2 <- mp1 <- fortify(map(fill=TRUE, plot=FALSE))
  mp2$long <- mp2$long + 360
  mp2$group <- mp2$group + max(mp2$group) + 1
  mp <- rbind(mp1, mp2)
}
##

#plot bounds
if(plot_var == "500mb" | plot_var == "850mb") {
  x_low <- 200; x_high <- 320
  y_low <- 10; y_high <- 70
} else if(plot_var == "SST") {
  x_low <- 110; x_high <- 330
  y_low <- -20; y_high <- 80
} else if(plot_var == "PDSI") {
  x_low <- -150; x_high <- -50
  y_low <- 15; y_high <- 65
}
#titles
mtitle <- "PDSI Anomaly"
# "500mb Height (m) and Wind (m/s) Anomaly"; "SST (deg C) Anomaly"; "PDSI Anomaly"
stitle <- paste0("JJAS PC", pc, " ", yr_type, " Years vs 1981-2010 Climatology")
#capt <- "JJAS Wet Years"
leg_title <- "PDSI\nAnomaly"
# "Height\nAnomaly\n(m)", "SST\nAnomaly\n(deg C)", "PDSI\nAnomaly"
#fill limits (when using a continuous scale)
lims <- c(-23, 23)
# PC1 # 500mb: c(-23, 23), 850mb: c(-18, 18), SST: c(-1.5, 1.5), PDSI: c(-3.5, 3.5)
# PC2 # 500mb: c(-22, 22), 850mb: c(-12, 12), SST: c(-1.5, 1.5), PDSI: c(-3.0, 3.0)
#fill breaks
fbreaks <- c(-3.75, -3, -2.25, -1.5, -0.75, 0.0, 0.75, 1.5, 2.25, 3, 3.75)
leg_labs <- c("", -3, -2.25, -1.5, -0.75, 0.0, 0.75, 1.5, 2.25, 3, "")
# 500mb: c(-22.5, -18, -13.5, -9, -4.5, 0, 4.5, 9, 13.5, 18, 22.5); c("", -18, -13.5, -9, -4.5, 0, 4.5, 9, 13.5, 18, "")
# 850mb: c(-18.75, -15, -11.25, -7.5, -3.75, 0, 3.75, 7.5, 11.25, 15, 18.75); c("", -15, -11.25, -7.5, -3.75, 0, 3.75, 7.5, 11.25, 15, "")
# SST: c(-1.5, -1.2, -0.9, -0.6, -0.3, 0.0, 0.3, 0.6, 0.9, 1.2, 1.5); c("", -1.2, -0.9, -0.6, -0.3, 0.0, 0.3, 0.6, 0.9, 1.2, "")
# PDSI: c(-3.75, -3, -2.25, -1.5, -0.75, 0.0, 0.75, 1.5, 2.25, 3, 3.75); c("", -3, -2.25, -1.5, -0.75, 0.0, 0.75, 1.5, 2.25, 3, "")
#wind arrow scaling
scaler <- 2

ggplot() +
  #geom_raster(data = data_lat_lon, aes(lon, lat, fill = val)) +
  geom_contour_filled(data = data_lat_lon, aes(lon, lat, z = val),
                      breaks = fbreaks) +
  # geom_contour(data = data_lat_lon, aes(lon, lat, z = val), breaks = c(0.00),
  #              linetype = "solid", color = "black", lwd = 0.8) +
  # geom_contour(data = data_lat_lon, aes(y = lat, x = lon, z = val), lwd = 0.8,
  #              breaks = c(-5, -10, -15, -20), color = "black", linetype = "dotdash") +
  # geom_contour(data = data_lat_lon, aes(y = lat, x = lon, z = val), lwd = 0.8,
  #              breaks = c(5, 10, 15, 20), color = "black", linetype = "longdash") +
  # geom_segment(data = data_lat_lon, aes(lon, lat, xend = lon+u*scaler, yend = lat+v*scaler),
  #              arrow = arrow(length = unit(0.1, "cm"), type = "closed")) +
  geom_polygon(data = mp, aes(x = long, y = lat, group = group), color = "black", fill = "NA")  + 
  coord_quickmap(xlim = c(x_low, x_high), ylim = c(y_low, y_high)) +
  labs(title = mtitle, subtitle = stitle) +#, caption = capt) +
  xlab("Longitude") + ylab("Latitude") + #labs(fill = plot_var) +
  theme(text = element_text(size = 18),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # scale_fill_gradientn(limits = lims,
  #                      colors = c("#2166ac", "#67a9cf", "#d1e5f0", "#FFFFFF", "#fddbc7", "#ef8a62", "#b2182b"))
  scale_fill_manual(values = c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0",
                               "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f"),
                    labels = leg_labs, name = leg_title, drop = F) +
  guides(fill = guide_legend(label.vjust = -0.5, reverse = T))

# playing around with legend on bottom
# within theme()
#legend.position = "bottom", legend.direction = "horizontal", legend.box = "horizontal",
#legend.spacing.x = unit(0.0, "cm"), legend.key.width = unit(1.0, "cm")) +
# within guide_legend()
# title.position = "top", nrow = 1, direction = "horizontal",
# title.hjust = 0.5, label.position = "bottom", label.hjust = 1)