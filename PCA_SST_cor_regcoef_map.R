#Create correlation maps for PCs and SST
library(maps)
#library(akima)
#library(fields)
#library(sp)
#library(rnaturalearth)
#library(rnaturalearthdata)

#Correlate a principal component with 1 value for each year (123 values)
# to SST at each ocean gridpoint containing SST data (123 values)

#See Import_NCEI_T_data.R, Import_NCEI_precip_data.R, PCA.R, and Import_Kaplan_SST.R
# to get data as we need

#Preallocate a matrix for correlation values
#PC_SST_cor = matrix(NA, nrow = length(PCs[1, ]), ncol = length(index_notmiss))
PC_SST_cor = matrix(NA, nrow = length(PC_scores[1, ]), ncol = length(index_notmiss))
PC_SST_reg_coef <- PC_SST_reg_pval <- PC_SST_cor

#Correlate each PC with SST data and grab lm regression coefficient and pval
# i is PC number
# j is location on world grid
for(i in 1:length(PC_scores[1, ])) {
  for(j in 1:length(index_notmiss)) {
    PC_SST_cor[i,j] <- cor(PC_scores[ ,i], sstdata[ ,j])
    
    #lm regression coefficient and pval
    PC_SST_lm_temp <- lm(PC_scores[ ,i] ~ sstdata[ ,j])
    
    PC_SST_reg_coef[i,j] <- summary(PC_SST_lm_temp)$coefficients[2, 1]
    PC_SST_reg_pval[i,j] <- summary(PC_SST_lm_temp)$coefficients[2, 4]
  }
}

###Plot the correlation map
# Preallocate a matrix, enter in correlation values at grid points with data, then enter in matrix
pc_plot <- 2 #PC to plot
zfull = zfull2 = rep(NaN, length(index))
zfull[index_notmiss] = PC_SST_cor[pc_plot, ]#PC_SST_cor[pc_plot, ] OR PC_SST_reg_coef[pc_plot, ] OR PC_SST_reg_pval[pc_plot, ]
zfull2[index_notmiss] = PC_SST_reg_pval[pc_plot, ]
zmat = matrix(zfull, nrow = nrows, ncol = ncols)

#Create df with plotting data for use with ggplot
plot_lat_lon <- cbind(xygrid, zfull)
colnames(plot_lat_lon) <- c("lat", "lon", "val")
plot_lat_lon <- as_tibble(plot_lat_lon)

#pvals
pval_lat_lon <- cbind(xygrid, zfull2)
colnames(pval_lat_lon) <- c("lat", "lon", "pval")
pval_lat_lon <- as_tibble(pval_lat_lon)

#plot_lat_lon$lon[plot_lat_lon$lon > 180] <- plot_lat_lon$lon[plot_lat_lon$lon > 180] - 360

### using image.plot ###
#####
# # Generate a plot with the correlation values, add contours, then add land mass boundaries
# #Full globe
# image.plot(xgrid, ygrid, zmat, xlim = range(25,385), ylim = range(-90,90), zlim = range(-0.4, 0.4),
#            main = "PC1 and SST Correlation", xlab = "longitude", ylab = "latitude")
# contour(xgrid, ygrid, (zmat), ylim = range(-90,90), zlim = range(-0.4, 0.4), add = TRUE, nlev = 8, lwd = 2)
# world(add = TRUE, wrap = c(25,385))#, lwd = 2, fill = TRUE,  col="lightgreen")
# 
# #Portion of globe
# par(mar = c(4.75, 4.75, 3, 2))
# z_rng <- c(-2, 5.25)
# image.plot(xgrid, ygrid, zmat, xlim = range(100,350), ylim = range(-20,90), zlim = range(z_rng),
#            main = "PC1 and SST LM Reg Coef", xlab = "Longitude", ylab = "Latitude",
#            cex.main = 2, cex.lab = 1.75, cex.axis = 1.5, axis.args = list(cex.axis = 1.5),
#            legend.shrink = 0.75, legend.args = list(text = "Coef", cex = 1.75, side = 3, line = 1))
# contour(xgrid, ygrid, (zmat), ylim = range(-20,90), zlim = range(z_rng), add = TRUE, nlev = 8, lwd = 2)
# #contour(xgrid, ygrid, (zmat), ylim = range(-20,90), zlim = range(z_rng), add = TRUE, levels = c(0.05), lwd = 2)
# world(add = TRUE, wrap = c(25,385))#, lwd = 2, fill = TRUE,  col="lightgreen")

#####
### using ggplot ###
#world <- ne_countries(scale = "medium", returnclass = "sf") #using natural earth package
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
title <- paste0("PC", pc_plot, " and SST Correlation")
  #"PC", pc_plot, " and SST LM Reg Coef"
map_fill <- "R"#"Reg\nCoef"

ggplot() +
  geom_raster(data = plot_lat_lon, aes(y = lat, x = lon, fill = val)) +
  geom_contour(data = plot_lat_lon, aes(y = lat, x = lon, z = val),
               breaks = c(0), color = "black", linetype = "solid", lwd = 0.8) +
  geom_contour(data = plot_lat_lon, aes(y = lat, x = lon, z = val),
               breaks = c(-0.1, -0.2, -0.3), color = "black", linetype = "dotdash") +
  geom_contour(data = plot_lat_lon, aes(y = lat, x = lon, z = val),
               breaks = c(0.1, 0.2, 0.3), color = "black", linetype = "longdash") +
  #geom_contour(data = pval_lat_lon, aes(y = lat, x = lon, z = pval), breaks = c(0.05), color = "black") +
  geom_polygon(data = mp, aes(x = long, y = lat, group = group), color = "black", fill = "grey")  + 
  coord_quickmap(xlim = c(130,335), ylim = c(-20,70)) +
  ggtitle(title) +
  xlab("Longitude") + ylab("Latitude") + labs(fill = map_fill, tag = "(A)") +
  theme(
    text = element_text(size = 18),
    plot.tag =  element_text(size = 26, face = "bold"),
    plot.tag.position = c(0.92, 1)) +
  scale_fill_gradientn(colors = c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#FFFFFF",
                                  "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f"),
                       limits = c(-0.4, 0.4))
