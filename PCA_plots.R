#Various plots for PCA
library(tidyverse)
library(rgdal)
#library(ggmap)

#Import CD info
PPR_CD_info <- read_csv("./NCEI_CAG_Data/Climate_Divisions/PPR_CD_info.csv")

###Eigen Spectrum/Scree plot
plot(1:length(lambdas), lambdas[1:length(lambdas)], type="l", xlab="Mode"
     , ylab="Frac. Var. explained")
points(1:length(lambdas), lambdas[1:length(lambdas)], col="red")

#using ggplot
scree_y <- eig_val #eig_val or frac_var
ggplot() +
  geom_line(aes(x = 1:22, y = scree_y), color = "#2171b5", size = 1) +
  geom_point(aes(x = 1:22, y = scree_y), color = "#2171b5", size = 3, shape = 19) +
  geom_point(aes(x = 1:22, y = scree_y), color = "white", size = 2, shape = 19) +
  labs(x = "Mode (PC Number)", y = "Eigenvalue") +
  #labs(x = "Mode (PC Number)", y = "% Variance Explained") +
  #ylim(0.0, 0.6) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )
  

###Plot Eigen Vectors Spatially
#Choose center location of map - this seems to be CD3 in South Dakota
#SD_CD3_loc <- c(lon = -98.30, lat = 45.31)

#Download the map - various options available here
#PPR_map <- get_map(location = SD_CD3_loc, zoom = 6, source = "stamen", maptype = "toner")
#PPR_map <- get_map(location = SD_CD3_loc, zoom = 5, source = "osm", color = "bw")

#data = data.frame(Zsvd$u[ ,2])

#Display map
# ggmap(PPR_map) +
#   geom_point(aes(x = PPR_CD_info$Avg_long, y = PPR_CD_info$Avg_lat,
#                  color = data, size = data),
#              data = data, shape = 16)

#Plot on raster map
#Climate Divisions
clim_div <- readOGR("../Shapefiles/CONUS_CD_Shapefiles", "GIS_OFFICIAL_CLIM_DIVISIONS")
clim_div_df <- fortify(clim_div)
clim_div_df_PPR <- filter(clim_div_df, id == 78 | id == 81 | id == 128 | id == 131 | id == 133 | id == 134 |
                            id == 157 | id == 158 | id == 159 | id == 160 | id == 161 | id == 162 | id == 163 |
                            id == 166 | id == 183 | id == 254 | id == 255 | id == 257 | id == 258 | id == 259 |
                            id == 260 | id == 271)
clim_div_df_PPR$id <- as.numeric(clim_div_df_PPR$id)

cd_name <- c("IA_CD2", "IA_CD5", "MN_CD1", "MN_CD4", "MN_CD5", "MN_CD7", "MN_CD8",
             "NE_CD3", "ND_CD1", "ND_CD2", "ND_CD3", "ND_CD4", "ND_CD5", "ND_CD6",
             "ND_CD8", "ND_CD9", "SD_CD2", "SD_CD3", "SD_CD6", "SD_CD7", "SD_CD8", "SD_CD9")
cd_id <- c(78, 81, 128, 183, 131, 133, 134,
           166, 157, 158, 271, 159, 160, 161,
           162, 163, 254, 255, 257, 258, 260, 259)
eig_spatial <- tibble(cd_name = cd_name,
                      id = cd_id,
                      #eig_vec_1 = Zsvd$u[ ,1], #PCA by SVD
                      #eig_vec_2 = Zsvd$u[ ,2])
                      eig_vec_1 = eig_vec[ ,1], #PCA by princomp()
                      eig_vec_2 = eig_vec[ ,2])
clim_div_df_PPR_eig <- clim_div_df_PPR %>%
  left_join(eig_spatial)

#US State Boundaries
states <- readOGR("../Shapefiles/US_states", "cb_2017_us_state_500k")
states_PPR <- subset(states, states$NAME == "North Dakota" |
                       states$NAME == "South Dakota" |
                       states$NAME == "Nebraska" |
                       states$NAME == "Minnesota" |
                       states$NAME == "Iowa")

projection <- proj4string(states)

#Canada Provinces
provinces <- readOGR("../Shapefiles/Canada_provinces", "Canada")
provinces <- spTransform(provinces, projection)

#Plots
p1 <- ggplot(clim_div_df_PPR_eig) +
  aes(long, lat, group = group) +
  geom_polygon(aes(fill = eig_vec_1)) +
  geom_path(color = "black") +
  geom_polygon(data = states, color = "black", fill = NA) +
  geom_polygon(data = provinces, color = "black", fill = NA) +
  scale_fill_gradientn(name = "PC1\nLoading", colors = c("#FFFFFF", "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f")) +
  #c("#f7fbff", "#6baed6", "#08306b")) +
  #c("#08306b", "#6baed6", "#f7fbff")) +
  #labs(title = "PC1") +
  labs(x = "Longitude", y = "Latitude", tag = "(A)") +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 22),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.90, 0.80),
        legend.background = element_rect(color = "black"),
        legend.key.size = unit(0.39, "cm"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        plot.tag = element_text(size = 26, face = "bold"),
        plot.tag.position = "topright") +
  coord_quickmap(xlim = c(-106, -90), ylim = c(41, 50))

p2 <- ggplot(clim_div_df_PPR_eig) +
  aes(long, lat, group = group) +
  geom_polygon(aes(fill = eig_vec_2)) +
  geom_path(color = "black") +
  geom_polygon(data = states, color = "black", fill = NA) +
  geom_polygon(data = provinces, color = "black", fill = NA) +
  scale_fill_gradientn(name = "PC2\nLoading", colors = c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0",
                                                         "#FFFFFF", "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f")) +
  #c("#00441b", "#74c476", "#f7fbff", "#6baed6", "#08306b")) +
  #c("#08306b", "#6baed6", "#f7fbff", "#74c476", "#00441b")) +
  #labs(title = "PC2") +
  labs(x = "Longitude", y = "Latitude", tag = "(B)") +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 22),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.90, 0.80),
        legend.background = element_rect(color = "black"),
        legend.key.size = unit(0.39, "cm"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        plot.tag =  element_text(size = 26, face = "bold"),
        plot.tag.position = "topright") +
  coord_quickmap(xlim = c(-106, -90), ylim = c(41, 50))

source("../R_functions/multiplot.R")
multiplot(p1, p2, cols = 2, title = "Spatial Eigenvalues")

#PC plot (temporal signature)
pc_plot <- 1
year <- c(1895:2017)
#model_lin <- lm(PCs[ ,pc_plot] ~ year) #PCA by SVD
model_lin <- lm(PC_scores[ ,pc_plot] ~ year) #PCA by princomp()
intercept <- model_lin$coefficients[1]
slope <- model_lin$coefficients[2]
ggplot() +
  geom_line(aes(year, PC_scores[ ,pc_plot])) +
  geom_abline(intercept = intercept, slope = slope, color = "red", size = 1) +
  #ggtitle("PC Temporal Signal") +
  labs(x = "Year", y = "PC2 Score", tag = "(D)") +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 22),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.tag =  element_text(size = 26, face = "bold"),
        plot.tag.position = "topright")

#trend tests
summary(model_lin) #significance of coefficient

library(trend)

PC_ts <- PC_scores[ ,pc_plot]

mk.test(PC_ts) #Mann-Kendall Trend test

cs.test(PC_ts) #Cox and Stuart Trend test
