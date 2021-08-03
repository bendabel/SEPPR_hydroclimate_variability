#Create correlation maps for PCs and 500mb heights
library(maps)
#library(akima)
#library(fields)

#Correlate a principal component with 1 value for each year (restrict to final 69 values)
# to 500mb at each ocean gridpoint containing SST data (69 values)

#See Import_NCEI_T_data.R, Import_NCEI_precip_data.R, PCA.R, and Import_500mb_Heights.R
# to get data as we need

#Note: index 55 corresponds to 1949, the first year of the 500 mb dataset
# create matrix with only these years
#PCs_500mb_cor <- PCs[55:123, ]
PCs_500mb_cor <- PC_scores[55:123, ]

#Preallocate a matrix for correlation values
PC_500mb_cor = matrix(NA, nrow = length(PCs_500mb_cor[1, ]), ncol = length(index_notmiss))
PC_500mb_reg_coef <- PC_500mb_reg_pval <- PC_500mb_cor

#Correlate each PC with 500mb height data
# i is PC number
# j is location on world grid
for(i in 1:length(PC_scores[1, ])) {
  for(j in 1:length(index_notmiss)) {
    PC_500mb_cor[i,j] <- cor(PCs_500mb_cor[ ,i], h500mbdata[ ,j])
    
    #lm regression coefficient and pval
    PC_500mb_lm_temp <- lm(PCs_500mb_cor[ ,i] ~ h500mbdata[ ,j])
    
    PC_500mb_reg_coef[i,j] <- summary(PC_500mb_lm_temp)$coefficients[2, 1]
    PC_500mb_reg_pval[i,j] <- summary(PC_500mb_lm_temp)$coefficients[2, 4]
  }
}

#Plot the correlation map
# Preallocate a matrix, enter in correlation values at grid points with data, then enter in matrix
pc_plot <- 1 #PC to plot
zfull = zfull2 = rep(NaN, length(index))
zfull[index_notmiss] = PC_500mb_cor[pc_plot, ]#PC_500mb_cor[pc_plot, ] OR PC_500mb_reg_coef[pc_plot, ] OR PC_500mb_reg_pval[pc_plot, ]
zfull2[index_notmiss] = PC_500mb_reg_pval[pc_plot, ]
zmat = matrix(zfull, nrow = nrows, ncol = ncols)

#Create df with plotting data for use with ggplot
plot_lat_lon <- cbind(xygrid, zfull)
colnames(plot_lat_lon) <- c("lat", "lon", "val")
plot_lat_lon <- as_tibble(plot_lat_lon)

#pvals
pval_lat_lon <- cbind(xygrid, zfull2)
colnames(pval_lat_lon) <- c("lat", "lon", "pval")
pval_lat_lon <- as_tibble(pval_lat_lon)

### using image.plot ###
#####
# Generate a plot with the correlation values, add contours, then add land mass boundaries
# #Full globe
# image.plot(xgrid, ygrid, zmat, xlim = range(0,360), ylim = range(-90,90), zlim = range(-0.6, 0.6),
#            main = "PC1 and 500 mb Height Correlation",xlab = "longitude", ylab = "latitude")
# contour(xgrid, ygrid, (zmat), ylim = range(-90,90), add = TRUE, zlim = range(-0.6, 0.6), nlev = 12, lwd = 2)
# world(add = TRUE, wrap = c(0,360))
# 
# #Portion of the globe
# par(mar = c(4.75, 4.75, 3, 2))
# image.plot(xgrid, ygrid, zmat, xlim = range(100,350), ylim = range(-20,90), zlim = range(-0.4, 0.4),
#            main = "PC2 and 500 mb Height Correlation",xlab = "Longitude", ylab = "Latitude",
#            cex.main = 2, cex.lab = 1.75, cex.axis = 1.5, axis.args = list(cex.axis = 1.5))
# contour(xgrid, ygrid, (zmat), ylim = range(-20,90), add = TRUE, zlim = range(-0.4, 0.4), nlev = 8, lwd = 2)
# world(add = TRUE, wrap = c(0,360))

#world(xlim = c(0,360), ylim = c(-90,90), wrap = c(0,360))
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
title <- paste0("PC", pc_plot, " and 500 mb Height Correlation")
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
  geom_polygon(data = mp, aes(x = long, y = lat, group = group), color = "black", fill = "NA")  + 
  coord_quickmap(xlim = c(130,335), ylim = c(-20,70)) +
  ggtitle(title) +
  xlab("Longitude") + ylab("Latitude") + labs(fill = map_fill, tag = "(B)") +
  theme(
    text = element_text(size = 18),
    plot.tag =  element_text(size = 26, face = "bold"),
    plot.tag.position = c(0.92, 1)
  ) +
  scale_fill_gradientn(colors = c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#FFFFFF",
                                  "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f"),
                       limits = c(-0.4, 0.4))
