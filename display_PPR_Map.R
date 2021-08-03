#Display PPR Map
library(tidyverse)
library(rgdal)

###Import raster maps
#US State Boundaries
states <- readOGR("../Shapefiles/US_states", "cb_2017_us_state_500k")

states_CONUS <- subset(states, states$NAME != "Hawaii" &
                         states$NAME != "Puerto Rico" &
                         states$NAME != "United States Virgin Islands" &
                         states$NAME != "Guam" &
                         states$NAME != "American Samoa" &
                         states$NAME != "Commonwealth of the Northern Mariana Islands")

projection <- proj4string(states)

#Canada Provinces
provinces <- readOGR("../Shapefiles/Canada_provinces", "Canada")
provinces <- spTransform(provinces, projection)

provinces_PPR <- subset(provinces, provinces$NAME == "Alberta" |
                          provinces$NAME == "Saskatchewan" |
                          provinces$NAME == "Manitoba")

#NA Boundaries
n_amer <- readOGR("../Shapefiles/NAmer", "North_America")

n_amer <- spTransform(n_amer, projection)

#PPR Millett restricted to southeast PPR and with modified left leg to closely match Ballard
PPR_Millett_mod <- readOGR("../Shapefiles/PPR_Shapefiles/PPR_left_leg", "PPR_left_leg_integrated")
#convert coordinates by changing the projection
PPR_Millett_mod <- spTransform(PPR_Millett_mod, projection)

#PPR Millett
PPR_Millett_full <- readOGR("../Shapefiles/PPR_Shapefiles/PPR_Millett", "PPR")
#convert coordinates by changing the projection
PPR_Millett_full <- spTransform(PPR_Millett_full, projection)


#USFWS Survey Strata
USFWS_strata <- readOGR("../Shapefiles/WBPHS_strata_boundaries", "WBPHS_strata_boundaries_2019")
#convert coordinates by changing the projection
USFWS_strata <- spTransform(USFWS_strata, projection)

USFWS_strata_USPPR <- subset(USFWS_strata, USFWS_strata$STRAT == 43| USFWS_strata$STRAT == 44|
                               USFWS_strata$STRAT == 45| USFWS_strata$STRAT == 46|
                               USFWS_strata$STRAT == 47| USFWS_strata$STRAT == 48|
                               USFWS_strata$STRAT == 49)

#Climate Divisions
clim_div <- readOGR("../Shapefiles/CONUS_CD_Shapefiles", "GIS_OFFICIAL_CLIM_DIVISIONS")

clim_div <- spTransform(clim_div, projection)

clim_div_PPR <- subset(clim_div, clim_div$CLIMDIV == 1302| clim_div$CLIMDIV == 1305|
                         clim_div$CLIMDIV == 2101| clim_div$CLIMDIV == 2104|
                         clim_div$CLIMDIV == 2105| clim_div$CLIMDIV == 2107|
                         clim_div$CLIMDIV == 2108| clim_div$CLIMDIV == 2503|
                         clim_div$CLIMDIV == 3201| clim_div$CLIMDIV == 3202|
                         clim_div$CLIMDIV == 3203| clim_div$CLIMDIV == 3204|
                         clim_div$CLIMDIV == 3205| clim_div$CLIMDIV == 3206|
                         clim_div$CLIMDIV == 3208| clim_div$CLIMDIV == 3209|
                         clim_div$CLIMDIV == 3902| clim_div$CLIMDIV == 3903|
                         clim_div$CLIMDIV == 3906| clim_div$CLIMDIV == 3907|
                         clim_div$CLIMDIV == 3908| clim_div$CLIMDIV == 3909)

#IA_CD2 = 1302, IA_CD5 = 1305
#MN_CD1 = 2101, MN_CD4 = 2104, MN_CD5 = 2105, MN_CD7 = 2107, MN_CD8 = 2108
#NE_CD3 = 2503
#ND_CD1 = 3201, ND_CD2 = 3202, ND_CD3 = 3203, ND_CD4 = 3204, ND_CD5 = 3205, ND_CD6 = 3206, ND_CD8 = 3208, ND_CD9 = 3209
#SD_CD2 = 3902, SD_CD3 = 3903, SD_CD6 = 3906, SD_CD7 = 3907, SD_CD8 = 3908, SD_CD9 = 3909

###Plots
#to add the stratum numbers to the plot, provide lat, lon locations for numbers
stratum_num <- tibble(
  long = c(-102.5, -102.5, -99.75, -98.95, -97.65, -98.9, -97.9),
  lat = c(47, 45, 48.25, 46.75, 47.5, 45, 43.3),
  names = c("43", "44", "45", "46", "47", "48", "49"),
  stringsAsFactors = FALSE
)

#plot with sePPR and USFWS survey strata used in dissertation
PPR_survey_strata <- ggplot() + 
  aes(long, lat, group = group) +
  geom_polygon(data = USFWS_strata_USPPR, color = "#00a884", fill = "#d7d79e", size = 2) +
  geom_polygon(data = states_CONUS, color = "black", fill = NA) +
  geom_polygon(data = provinces, color = "black", fill = NA) +
  geom_polygon(data = PPR_Millett_mod, color = "gray20", size = 1.05, fill = NA, linetype = "longdash") +
  geom_label(data = stratum_num, aes(x = long, y = lat, label = names),
            hjust = 0, #nudge_x = 0.2, nudge_y = 0.05,
            size = 6, fontface = "bold", inherit.aes = F) +
  #labs(title = "Climate Divisions Coincident with PPR") +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_quickmap(xlim = c(-106, -90), ylim = c(41, 50)) +
  theme(legend.title = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18))

PPR_survey_strata

#redefine locations slightly for next plot
stratum_num <- tibble(
  long = c(-102.75, -102.75, -100.25, -99.25, -97.65, -99.25, -97.9),
  lat = c(47, 45, 48.25, 46.75, 47.5, 45, 43.3),
  names = c("43", "44", "45", "46", "47", "48", "49"),
  stringsAsFactors = FALSE
)


#plot with full PPR and sePPR filled in or plot showing PPR place in NA
PPR_full <- ggplot() + 
  aes(long, lat, group = group) +
  #geom_polygon(data = PPR_Millett_full, color = "black", size = 1.05, fill = NA, linetype = "longdash") +
  #geom_polygon(data = PPR_Millett_full, color = NA, fill = "#6baed6") +
  #geom_polygon(data = PPR_Millett_mod, color = "gray20", size = 1.05, fill = "#6baed6", linetype = "longdash") +
  geom_polygon(data = PPR_Millett_mod, color = NA, fill = "#4393c3") +
  #geom_polygon(data = USFWS_strata_USPPR, color = "#006837", fill = NA, size = 2) +
  geom_polygon(data = n_amer, color = "black", fill = NA) +
  #geom_polygon(data = states_CONUS, color = "black", fill = NA) +
  #geom_polygon(data = provinces, color = "black", fill = NA) +
  #geom_label(data = stratum_num, aes(x = long, y = lat, label = names),
  #           hjust = 0, #nudge_x = 0.2, nudge_y = 0.05,
  #           size = 4.5, fontface = "bold", inherit.aes = F) +
  #labs(title = "Climate Divisions Coincident with PPR") +
  xlab("Longitude") + ylab("Latitude") +
  #coord_quickmap(xlim = c(-115, -89), ylim = c(41, 54.5)) +
  coord_quickmap(xlim = c(-145, -55), ylim = c(10, 70)) +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

PPR_full


#plot with sePPR, CDs, and USFWS stata
PPR_CDs_survey <- ggplot() + 
  aes(long, lat, group = group) +
  geom_polygon(data = PPR_Millett_mod, color = NA, fill = "#4393c3") +
  #geom_polygon(data = USFWS_strata_USPPR, color = "forest green", fill = NA, size = 3) +
  geom_polygon(data = states_CONUS, color = "black", fill = NA) +
  geom_polygon(data = provinces, color = "black", fill = NA) +
  geom_polygon(data = clim_div_PPR, color = "gray20", size = 1.1, fill = NA) +
  #geom_label(data = stratum_num, aes(x = long, y = lat, label = names),
  #           hjust = 0, #nudge_x = 0.2, nudge_y = 0.05,
  #           size = 6, fontface = "bold", inherit.aes = F) +
  #labs(title = "Climate Divisions Coincident with PPR") +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_quickmap(xlim = c(-106, -90), ylim = c(41, 50)) +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18))

PPR_CDs_survey

#plot map with an inset
ggplot() +
  coord_equal(xlim = c(0, 3.0), ylim = c(0, 2), expand = FALSE) +
  annotation_custom(ggplotGrob(PPR_CDs_survey), xmin = 0, xmax = 3.0, ymin = 0, 
                    ymax = 2) +
  annotation_custom(ggplotGrob(PPR_full), xmin = 2.1, xmax = 2.9, ymin = 1.2, 
                    ymax = 1.95) +
  theme_void()

#"#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#FFFFFF", "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f"