#Create climatology (1980-2010) of precipitation in SEPPR
library(tidyverse)

#Import large dataset
precip_CD_1895_2017 <-
  read_csv("./NCEI_CAG_Data/Climate_Divisions/NCEI_precip_allCD.csv")

#Filter to just PPR CDs
PPR_precip_CD_1895_2017 <- precip_CD_1895_2017 %>%
  filter(CD == 1302 | CD == 1305 | CD == 2101 | CD == 2104 |
           CD == 2105 | CD == 2107 | CD == 2108 | CD == 2503 |
           CD == 3201 | CD == 3202 | CD == 3203 | CD == 3204 |
           CD == 3205 | CD == 3206 | CD == 3208 | CD == 3209 | 
           CD == 3902 | CD == 3903 | CD == 3906 | CD == 3907 |
           CD == 3908 | CD == 3909)

#1980-2010 climatology
PPR_precip_CD_1980_2010 <- PPR_precip_CD_1895_2017 %>%
  filter(year > 1980 & year <= 2010)

#by CD
PPR_precip_CD_climatology <- PPR_precip_CD_1980_2010 %>%
  group_by(month, CD) %>% summarise(mean_precip = mean(precip))

#for all SEPPR
PPR_precip_all_climatology <- PPR_precip_CD_1980_2010 %>%
  group_by(month) %>% summarise(mean_precip = mean(precip))

#make months factors to specify order for plotting
PPR_precip_CD_climatology$month <- factor(PPR_precip_CD_climatology$month, levels = PPR_precip_CD_1895_2017$month[1:12])
PPR_precip_all_climatology$month <- factor(PPR_precip_all_climatology$month, levels = PPR_precip_CD_1895_2017$month[1:12])

#plot
ggplot(PPR_precip_CD_climatology) +
  geom_line(aes(x = month, y = mean_precip, color = factor(CD), group = CD), color = "dark grey") +
  geom_line(data = PPR_precip_all_climatology,
            aes(x = month, y = mean_precip, group = 1), color = "black", size = 1) +
  labs(x = "Month", y = "Mean Precipitation (in)") +
  theme_bw() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )
