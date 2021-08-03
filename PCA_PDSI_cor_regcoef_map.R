#Create correlation maps for PCs and PDSI
library(maps)
#library(akima)
#library(fields)

#Correlate a principal component with 1 value for each year (120 values)
# to PDSI at each land gridpoint containing PDSI data (123 values)

#See Import_PDSI_netCDF.R, Import_NCEI_precip_data.R
# to get data as needed

#Note: PDSI data ends at 2014
# create matrix with only these years
PCs_PDSI_cor <- PC_scores[1:120, ]

#Preallocate a matrix for correlation values
PC_PDSI_cor = matrix(NA, nrow = length(PCs_PDSI_cor[1, ]), ncol = length(index_notmiss))
PC_PDSI_reg_coef <- PC_PDSI_reg_pval <- PC_PDSI_cor

#Correlate each PC with PDSI data
# i is PC number
# j is location on world grid
for(i in 1:length(PCs_PDSI_cor[1, ])) {
  for(j in 1:length(index_notmiss)) {
    PC_PDSI_cor[i,j] <- cor(PCs_PDSI_cor[ ,i], pdsidata[ ,j])
    
    #lm regression coefficient and pval
    PC_PDSI_lm_temp <- lm(PCs_PDSI_cor[ ,i] ~ pdsidata[ ,j])
    
    PC_PDSI_reg_coef[i,j] <- summary(PC_PDSI_lm_temp)$coefficients[2, 1]
    PC_PDSI_reg_pval[i,j] <- summary(PC_PDSI_lm_temp)$coefficients[2, 4]
  }
}

range(PC_PDSI_cor, na.rm = T)

#Plot the correlation map
# Preallocate a matrix, enter in correlation values at grid points with data, then enter in matrix
pc_plot <- 2 #PC to plot
zfull = zfull2 = rep(NaN, length(index))
zfull[index_notmiss] = PC_PDSI_cor[pc_plot, ]#PC_PDSI_cor[pc_plot, ] OR PC_PDSI_reg_coef[pc_plot, ] OR PC_PDSI_reg_pval[pc_plot, ]
zfull2[index_notmiss] = PC_PDSI_reg_pval[pc_plot, ]
zmat = matrix(zfull, nrow = nx, ncol = ny)

#Create df with plotting data for use with ggplot
plot_lat_lon <- cbind(xygrid, zfull)
colnames(plot_lat_lon) <- c("lat", "lon", "val")
plot_lat_lon <- as_tibble(plot_lat_lon)

#pvals
pval_lat_lon <- cbind(xygrid, zfull2)
colnames(pval_lat_lon) <- c("lat", "lon", "pval")
pval_lat_lon <- as_tibble(pval_lat_lon)

# change negative lon values to positive for plotting
plot_lat_lon$lon[plot_lat_lon$lon < 180] <- plot_lat_lon$lon[plot_lat_lon$lon < 180] + 360
pval_lat_lon$lon[pval_lat_lon$lon < 180] <- pval_lat_lon$lon[pval_lat_lon$lon < 180] + 360

### using image.plot ###
#####
# Generate a plot with the correlation values, add contours, then add land mass boundaries
# west_bound <- -140; east_bound <- -50
# south_bound <- 20; north_bound <- 60
# par(mar = c(4.75, 4.75, 3, 2))
# image.plot(x, y, zmat, xlim = range(west_bound,east_bound),
#            ylim = range(south_bound,north_bound), zlim = range(-0.6, 0.6),
#            main = "PC2 and PDSI Correlation", xlab = "Longitude", ylab = "Latitude",
#            cex.main = 2, cex.lab = 1.75, cex.axis = 1.5, axis.args = list(cex.axis = 1.5))
# contour(x, y, (zmat), ylim = range(south_bound, north_bound),
#         add = TRUE, nlev = 6, lwd = 2, zlim = range(-0.6, 0.6))
# world(add = TRUE, wrap = c(-180,180))
# US(add = TRUE, wrap = c(-180,180))

#####
### using ggplot ###
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
west_bound <- 220; east_bound <- 310
south_bound <- 20; north_bound <- 60

title <- paste0("PC", pc_plot, " and PDSI Correlation")
#"PC", pc_plot, " and SST LM Reg Coef"
map_fill <- "R"#"Reg\nCoef"

ggplot() +
  geom_raster(data = plot_lat_lon, aes(y = lat, x = lon, fill = val)) +
  geom_contour(data = plot_lat_lon, aes(y = lat, x = lon, z = val),
               breaks = c(0), color = "black", linetype = "solid", lwd = 0.8) +
  geom_contour(data = plot_lat_lon, aes(y = lat, x = lon, z = val),
               breaks = c(-0.1, -0.2, -0.3, -0.4, -0.5), color = "black", linetype = "dotdash") +
  geom_contour(data = plot_lat_lon, aes(y = lat, x = lon, z = val),
               breaks = c(0.1, 0.2, 0.3, 0.4, 0.5), color = "black", linetype = "longdash") +
  #geom_contour(data = pval_lat_lon, aes(y = lat, x = lon, z = pval), breaks = c(0.05), color = "black") +
  geom_polygon(data = mp, aes(x = long, y = lat, group = group), color = "black", fill = "NA")  + 
  coord_quickmap(xlim = c(west_bound, east_bound), ylim = c(south_bound, north_bound)) +
  ggtitle(title) +
  xlab("Longitude") + ylab("Latitude") + labs(fill = map_fill, tag = "(C)") +
  theme(
    text = element_text(size = 18),
    plot.tag =  element_text(size = 26, face = "bold"),
    plot.tag.position = c(0.92, 1) ) +
  scale_fill_gradientn(colors = c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#FFFFFF",
                                  "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f"),
                       limits = c(-0.61, 0.61))
