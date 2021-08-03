#Plot PC time series data
library(tidyverse)
library(locfit)

#Choose PC for analysis
PC_num <- 1

#PCts <- ts(PCs[ ,PC_num], start = 1895, frequency=1)
PCts <- ts(PC_scores[ ,PC_num], start = 1895, frequency=1) 
plot(PCts, plot.type = "single", xlab = "Year", ylab = "PC value") 
#axis(3, at=years)
years <- c(1895:2017)
abline(lm(PC_scores[ ,PC_num] ~ years), col=2)
#lines(locfit(PCs[ ,PC_num] ~ years), col=2)

#Find high/low PC years (or dry/wet years)

###
# Note there is a negative correlation between PC1 and the precip data when using svd
# This indicates that high PC values are actually low precip values (dry) and vice versa
# When using the built in PCA function, the correlation is positive and thus
#  high PC values are wet and vice versa
###

PC <- PC_scores[ ,PC_num]
PC %>%
  quantile(0.85) #PC1: 3.546615, PC2: 1.574448
PC_high_years <- years[PC > 1.574448]

PC %>%
  quantile(0.15) #PC1: -2.932404, PC2: -1.799276
PC_low_years <- years[PC < -1.799276]

#PC1
#High (Wet) (19): 1900 1901 1905 1914 1915 1928 1941 1944 1957 1962
# 1965 1968 1977 1986 1993 2005 2010 2014 2016
#Low (Dry) (19): 1898 1910 1917 1922 1929 1930 1931 1933 1934 1936
# 1958 1967 1970 1974 1976 1988 1989 2003 2012

#PC2
#High (wet): 1902 1906 1908 1915 1917 1919 1923 1929 1933 1934 1951 1960 1967 1979 1984 1992 2006 2010 2015
#Low (dry): 1904 1912 1916 1927 1928 1931 1935 1937 1941 1947 1954 1964 1971 1975 1980 1991 2000 2002 2012

###
## Plot PC with raw data
# Use Analysis_tot_precip.R to get raw data time series (aggregate SEPPR)
#Scale two ts
PC_scaled <- scale(PCts)
raw_tot_scaled <- scale(PPR_precip_JJAS_by_year$tot_precip)

#Find scaled threshholds
PC_scaled %>% quantile(0.85) #PC1: 1.025873
PC_scaled %>% quantile(0.15) #PC1: -0.84821

#build tibble for plotting
ts_data <- tibble(
  year = years,
  PC_ts_scaled = PC_scaled,
  raw_ts_scaled = raw_tot_scaled
)

# color scale use for other plots in paper:
#"#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#FFFFFF", "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f"

#plot
ggplot(ts_data) +
  geom_point(aes(year, PC_ts_scaled), shape = 1, size = 2, color = "#2166ac") +
  geom_point(aes(year, raw_ts_scaled), size = 1, color = "#b2182b") +
  geom_hline(yintercept = 1.025873, linetype = "dashed") +
  geom_hline(yintercept = -0.84821, linetype = "dashed") +
  xlab("Year") + ylab("Scaled PC1 and SEPPR JJAS Precipitation")
