#Map precipitation
library(tidyverse)
library(rgdal)

#Import data
PPR_CD_precip_JJAS_percent <- read_csv("./NCEI_CAG_Data/Climate_Divisions/Precip/PPR_CD_precip_JJAS_percent_1896-2017.csv")
PPR_precip_yearly_mean <- read_csv("./NCEI_CAG_Data/Climate_Divisions/Precip/PPR_precip_yearly_mean_1896-2017.csv")
PPR_precip_JJAS_mean <- read_csv("./NCEI_CAG_Data/Climate_Divisions/Precip/PPR_precip_JJAS_mean_1896-2017.csv")

#Import CD info
PPR_CD_info <- read_csv("./NCEI_CAG_Data/Climate_Divisions/PPR_CD_info.csv")

#Plot on raster map
#Import raster maps
#Climate Divisions
clim_div <- readOGR("../Shapefiles/CONUS_CD_Shapefiles", "GIS_OFFICIAL_CLIM_DIVISIONS")
clim_div_df <- fortify(clim_div)
clim_div_df_PPR <- filter(clim_div_df, id == 78 | id == 81 | id == 128 | id == 131 | id == 133 | id == 134 |
                            id == 157 | id == 158 | id == 159 | id == 160 | id == 161 | id == 162 | id == 163 |
                            id == 166 | id == 183 | id == 254 | id == 255 | id == 257 | id == 258 | id == 259 |
                            id == 260 | id == 271)
clim_div_df_PPR$id <- as.numeric(clim_div_df_PPR$id)

#IA_CD2 = 78, IA_CD5 = 81
#MN_CD1 = 128, MN_CD4 = 183, MN_CD5 = 131, MN_CD7 = 133, MN_CD8 = 134
#NE_CD3 = 166
#ND_CD1 = 157, ND_CD2 = 158, ND_CD3 = 271, ND_CD4 = 159, ND_CD5 = 160, ND_CD6 = 161, ND_CD8 = 162, ND_CD9 = 163
#SD_CD2 = 254, SD_CD3 = 255, SD_CD6 = 257, SD_CD7 = 258, SD_CD8 = 260, SD_CD9 = 259

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


cd_name <- c("IA_CD2", "IA_CD5", "MN_CD1", "MN_CD4", "MN_CD5", "MN_CD7", "MN_CD8",
             "NE_CD3", "ND_CD1", "ND_CD2", "ND_CD3", "ND_CD4", "ND_CD5", "ND_CD6",
             "ND_CD8", "ND_CD9", "SD_CD2", "SD_CD3", "SD_CD6", "SD_CD7", "SD_CD8", "SD_CD9")
cd_id <- c(78, 81, 128, 183, 131, 133, 134,
           166, 157, 158, 271, 159, 160, 161,
           162, 163, 254, 255, 257, 258, 260, 259)

#create tibbles for plotting
JJAS_percent <- tibble(cd_name = cd_name,
                   id = cd_id,
                   JJAS_percent = PPR_CD_precip_JJAS_percent$JJAS_percent*100)
year_mean <- tibble(cd_name = cd_name,
                    id = cd_id,
                    year_mean = PPR_precip_yearly_mean$yearly_mean)
JJAS_mean <- tibble(cd_name = cd_name,
                    id = cd_id,
                    JJAS_mean = PPR_precip_JJAS_mean$JJAS_mean)

clim_div_df_PPR_JJAS_percent <- clim_div_df_PPR %>%
  left_join(JJAS_percent)
clim_div_df_PPR_year_mean <- clim_div_df_PPR %>%
  left_join(year_mean)
clim_div_df_PPR_JJAS_mean <- clim_div_df_PPR %>%
  left_join(JJAS_mean)

# Plot JJAS percent
ggplot(clim_div_df_PPR_JJAS_percent) + 
  aes(long, lat, group = group) + 
  geom_polygon(aes(fill = JJAS_percent)) +
  geom_path(color = "black") +
  geom_polygon(data = states, color = "black", fill = NA) +
  geom_polygon(data = provinces, color = "black", fill = NA) +
  scale_fill_gradientn(name = "Percent", colors = c("#FFFFFF", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061"), limits = c(45, 60)) +
  #"#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0"
  #scale_fill_gradientn(name = "Percent", colors = c("#f7fbff", "#6baed6", "#08306b"), limits = c(45, 60)) +
  xlab("Longitude") + ylab("Latitude") +
  theme(legend.position = c(0.90, 0.80),
        legend.background = element_rect(color = "black"),
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)) +
  coord_quickmap(xlim = c(-106, -90), ylim = c(41, 50))

# Plot yearly mean
ggplot(clim_div_df_PPR_year_mean) + 
  aes(long, lat, group = group) + 
  geom_polygon(aes(fill = year_mean)) +
  geom_path(color = "black") +
  geom_polygon(data = states, color = "black", fill = NA) +
  geom_polygon(data = provinces, color = "black", fill = NA) +
  scale_fill_gradientn(name = "Amount (in)", colors = c("#f7fbff", "#6baed6", "#08306b"))+#, limits = c(45, 60)) +
  xlab("Longitude") + ylab("Latitude") +
  theme(legend.position = c(0.90, 0.80),
        legend.background = element_rect(color = "black"),
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)) +
  coord_quickmap(xlim = c(-106, -90), ylim = c(41, 50))

# Plot JJAS mean
ggplot(clim_div_df_PPR_JJAS_mean) + 
  aes(long, lat, group = group) + 
  geom_polygon(aes(fill = JJAS_mean)) +
  geom_path(color = "black") +
  geom_polygon(data = states, color = "black", fill = NA) +
  geom_polygon(data = provinces, color = "black", fill = NA) +
  scale_fill_gradientn(name = "Amount (in)", colors = c("#f7fbff", "#6baed6", "#08306b"))+#, limits = c(45, 60)) +
  xlab("Longitude") + ylab("Latitude") +
  theme(legend.position = c(0.90, 0.80),
        legend.background = element_rect(color = "black"),
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)) +
  coord_quickmap(xlim = c(-106, -90), ylim = c(41, 50))
