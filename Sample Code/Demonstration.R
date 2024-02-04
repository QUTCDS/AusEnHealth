## ########################################################################## ##
##         ___               _____         _   _            _ _   _           ##
##        / _ \             |  ___|       | | | |          | | | | |          ##
##       / /_\ \_   _ ___   | |__ _ __    | |_| | ___  __ _| | |_| |__        ##
##       |  _  | | | / __|  |  __| '_ \   |  _  |/ _ \/ _` | | __| '_ \       ##
##       | | | | |_| \__ \  | |__| | | |  | | | |  __/ (_| | | |_| | | |      ##
##       \_| |_/\__,_|___/  \____/_| |_|  \_| |_/\___|\__,_|_|\__|_| |_|      ##
##                                                                            ##
## ########################################################################## ##



# Libraries --------------------------------------------------------------------
library(tidyverse)
library(sf)
library(rmapshaper)
library(corrplot)



# Load Data --------------------------------------------------------------------
# Yearly Vulnerability Index, Local Government Areas, 2011-2019.
dfYr <- read_csv(paste0("../Data/Vulnerability_Yearly_2011_2019.csv"),
               guess_max = 400000,
               show_col_types = FALSE)

# Monthly Vulnerability Index, Local Government Areas, 2011-2019.
dfMth <- read_csv(paste0("../Data/Vulnerability_Monthly_2011_2019.csv"),
                guess_max = 400000,
                show_col_types = FALSE)

# Mortality, Local Government Areas, 2015-2019 Average ASR per 100,000.
dfMort <- read_csv(paste0("../Data/Mortalty_Causes_2015_2019.csv"),
                guess_max = 400000,
                show_col_types = FALSE)



# Load Shapefiles --------------------------------------------------------------
# Local Government Area 2016 shape file, from the Australian Bureau of Statistics.
LGA_SHP_Full <- read_sf("../Data/Shape Files/Full LGA","LGA_2016_AUST") %>%
  filter(!st_is_empty(.)) %>%
  filter(STE_CODE16 != "9")

# Local Government Area 2016 shape file, quality reduced for faster plotting.
LGA_SHP_Simple <- read_sf("../Data/Shape Files/Simplified LGA","rLGA") %>%
  filter(!st_is_empty(.)) %>%
  filter(STE_CODE16 != "9")

# States 2016 shape file, from the Australian Bureau of Statistics.
STE_SHP_Full <- read_sf("../Data/Shape Files/Full STE","STE_2016_AUST") %>%
  filter(!st_is_empty(.)) %>%
  filter(STE_CODE16 != "9") %>%
  ms_simplify(0.03)

# National 2016 shape file, from the Australian Bureau of Statistics.
AUS_SHP_Full <- read_sf("../Data/Shape Files/Full AUS","AUS_2016_AUST") %>%
  filter(!st_is_empty(.)) %>%
  ms_simplify(0.03)



# Averaging Yearly Data to Match Mortality -------------------------------------
df_2015_2019 <- dfYr %>%
  filter(Year %in% (2015:2019)) %>%
  select("LGA_CODE16",
         "Heat_Vulnerability_Index",
         "Cold_Vulnerability_Index",
         "Air_Quality_Vulnerability_Index"
         )
df_2015_2019 <- aggregate(df_2015_2019,
                          by = list(df_2015_2019$LGA_CODE16),
                          FUN = mean, na.rm=T)[,-1]



# Sample Plots -----------------------------------------------------------------
# Histogram of 2019 Air Quality Vulnerability Index
df_eg1 <- filter(dfYr, Year == 2019)
ggplot(df_eg1, aes(x=Air_Quality_Vulnerability_Index)) +
  geom_histogram(bins = 20, color="black", fill = "lightblue") +
  theme_bw() +
  labs(title = "Histogram, 2019 AQVI")

# Choropleth plot of 2015 Cold Vulnerability Index
df_eg2 <- merge(LGA_SHP_Simple, filter(dfYr, Year == 2015))
ggplot() +
  theme_void() +
  geom_sf(data = df_eg2,
          aes(fill = Cold_Vulnerability_Index),
          color = NA) +
  geom_sf(data = AUS_SHP_Full,
          fill = NA,
          color = "black",
          linewidth = 0.7)

# Choropleth plot of 2017 Heat Vulnerability Index in Victoria.
df_eg3 <- filter(merge(LGA_SHP_Simple, filter(dfYr, Year == 2017)), STE_NAME16 == "Victoria")
STE_SHP_Full <- filter(STE_SHP_Full, STE_NAME16 == "Victoria")
ggplot() +
  theme_void() +
  geom_sf(data = df_eg3,
          aes(fill = Heat_Vulnerability_Index),
          color = NA) +
  geom_sf(data = STE_SHP_Full,
          fill = NA,
          color = "black",
          linewidth = 1)

# Correlation plot of 2015-2019 vulnerability indices and mortality.
df_eg4 <- cor(merge(dfMort,df_2015_2019)[,-1], method="kendall", use="complete.obs")
corrplot(df_eg4, method = 'circle', type = 'upper')

# Scatter plot of 2015-2019 air quality vulnerability index and chronic obstructive pulmonary disease.
df_eg5 <- merge(dfMort,df_2015_2019)
ggplot(data = df_eg5,
       aes(x = Air_Quality_Vulnerability_Index, y = All_Causes_ASR)) +
  geom_point(shape = 21,
             colour = "black",
             fill = "red") +
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE, na.rm = T)

# Line plot showing changes in yearly vulnerability indices over time for Brisbane.
df_eg6 <- filter(merge(dfYr, LGA_SHP_Full), LGA_NAME16 == "Brisbane (C)")
ggplot(df_eg6, aes(x = Year)) + 
  geom_line(aes(y = Heat_Vulnerability_Index, colour = "Heat Vulnerability")) +
  geom_line(aes(y = Cold_Vulnerability_Index, colour = "Cold Vulnerability")) +
  geom_line(aes(y = Air_Quality_Vulnerability_Index, colour = "Air Quality Vulnerability")) +
  theme_bw() +
  labs(colour = "Index Type") +
  theme(legend.position = "bottom") +
  ggtitle("Vulnerability Index Over Time")

# Line plot showing changes in yearly vulnerability indices over time for Brisbane.
df_eg7 <- filter(merge(dfMth, LGA_SHP_Full), LGA_NAME16 == "Brisbane (C)")
df_eg7$Date <- as.Date(with(df_eg7, paste(Year, Month, "01", sep="-")), "%Y-%m-%d")
ggplot(df_eg7, aes(x = Date)) + 
  geom_line(aes(y = Heat_Vulnerability_Index, colour = "Heat Vulnerability")) +
  geom_line(aes(y = Cold_Vulnerability_Index, colour = "Cold Vulnerability")) +
  geom_line(aes(y = Air_Quality_Vulnerability_Index, colour = "Air Quality Vulnerability")) +
  theme_bw() +
  labs(colour = "Index Type") +
  theme(legend.position = "bottom") +
  ggtitle("Vulnerability Index Over Time")

# A bit messy, so plot using "facet_wrap".
df_eg8 <- gather(df_eg7, Index, Value, Heat_Vulnerability_Index:Air_Quality_Vulnerability_Index)
ggplot(df_eg8, aes(x = Date, y = Value, group = Index, colour = Index)) + 
  geom_line() +
  facet_wrap(~Index, nrow=3) +
  theme_bw() +
  labs(colour = "Index Type") +
  theme(legend.position = "bottom") +
  ggtitle("Vulnerability Index Over Time by Index")



## ########################################################################## ##