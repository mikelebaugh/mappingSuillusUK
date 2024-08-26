
install.packages(c("cowplot", "readxl", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth",
                   "rnaturalearthdata", "viridis", "ggthemes", "gtsummary"))


library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("readxl")
library(CoordinateCleaner)
library(dplyr)
library(viridis)
library("ggthemes")
library(gtsummary)



world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)



# BMS Database

BMS_data <- suillusFRDBI2

UK_sites <- data.frame(name = BMS_data$`Current name`, longitude = BMS_data$Lon, 
                    latitude = BMS_data$Lat, country = BMS_data$Country, 
                    Year = BMS_data$`Start Year`, End_Year = BMS_data$`End Year`)
sites <- UK_sites[UK_sites$country != "Eire", ]

observationsPerSpecies <- as.data.frame(table(sites$name))

View(observationsPerSpecies)
### Species with zero BMS observations: mediterraneesis and subluteus

sites_50s_to_2023 <- sites[sites$Year >= 1950, ]

sites_90s_to_2023 <- sites[sites$Year >= 1990, ]

sites_2000_to_2023 <- sites[sites$Year >= 2000, ]

observationsPerSpecies2000 <- as.data.frame(table(sites_2000_to_2023$name))

suillusGenusRegression <- lm(sites_2000_to_2023$Year ~ sites_2000_to_2023$latitude)
suillusGenusRsquared <- cor(sites_2000_to_2023$Year, sites_2000_to_2023$latitude)

suillusGenusRegression  %>%
  tbl_regression()


## BMS Data: Genus Map

ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites, aes(x = longitude, y = latitude, color = Year), 
             size = 1, 
             shape = 20
             ) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE
           ) +
  scale_color_viridis(option = "plasma") +
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of the Genus Suillus')

ggplot(data = sites, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Genus Suillus')

ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites_50s_to_2023, aes(x = longitude, y = latitude, color = Year), 
             size = 1, 
             shape = 20
             ) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE
           ) +
  scale_color_viridis() +
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of the Genus Suillus: 1950-2023')

ggplot(data = sites_50s_to_2023, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Genus Suillus: 1950-2023')

ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites_90s_to_2023, aes(x = longitude, y = latitude, color = Year), 
             size = 1, 
             shape = 20
  ) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE
  ) +
  scale_color_viridis() +
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of the Genus Suillus: 1990-2023')

ggplot(data = sites_90s_to_2023, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Genus Suillus: 1990-2023')

ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites_2000_to_2023, aes(x = longitude, y = latitude, color = Year), 
             size = 1, 
             shape = 20
  ) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE
  ) +
  scale_color_viridis() +
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of the Genus Suillus: 2000-2023')

ggplot(data = sites_2000_to_2023, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Genus Suillus: 2000-2023')




## BMS Data: Individual Species


bovinus <- sites[sites$name == "Suillus bovinus", ]

bovinus_50s_to_2023 <- bovinus[bovinus$Year >= 1950, ]

bovinus_90s_to_2023 <- bovinus[bovinus$Year >= 1990, ]

bovinus_2000_to_2023 <- bovinus[bovinus$Year >= 2000, ]

bovinusRegression <- lm(bovinus_2000_to_2023$Year ~ bovinus_2000_to_2023$latitude)
bovinusRsquared <- cor(bovinus_2000_to_2023$Year, bovinus_2000_to_2023$latitude)

bovinusRegression  %>%
  tbl_regression()

ggplot(data = world) +
  geom_sf() +
  geom_point(data = bovinus, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis(option = "plasma") +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus bovinus')

ggplot(data = bovinus_50s_to_2023, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus bovinus: 1950-2023')

ggplot(data = world) +
  geom_sf() +
  geom_point(data = bovinus_90s_to_2023, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis() +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus bovinus: 1990-2023')

ggplot(data = bovinus_90s_to_2023, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus bovinus: 1990-2023')

ggplot(data = world) +
  geom_sf() +
  geom_point(data = bovinus_2000_to_2023, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis() +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus bovinus: 2000-2023')

ggplot(data = bovinus_2000_to_2023, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus bovinus: 2000-2023')



bresadolae <- sites[sites$name == "Suillus bresadolae var. flavogriseus", ]

bresadolae_50s_to_2023 <- bresadolae[bresadolae$Year >= 1950, ]

bresadolae_90s_to_2023 <- bresadolae[bresadolae$Year >= 1990, ]

bresadolae_2000_to_2023 <- bresadolae[bresadolae$Year >= 2000, ]

bresadolaeRegression <- lm(bresadolae_2000_to_2023$Year ~ bresadolae_2000_to_2023$latitude)
bresadolaeRsquared <- cor(bresadolae_2000_to_2023$Year, bresadolae_2000_to_2023$latitude)

bresadolaeRegression  %>%
  tbl_regression()

ggplot(data = world) +
  geom_sf() +
  geom_point(data = bresadolae, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis(option = "plasma") +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle('BMS Observations of Suillus bresadolae var. flavogriseus')

ggplot(data = bresadolae, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus bresadolae var. flavogriseus')

ggplot(data = world) +
  geom_sf() +
  geom_point(data = bresadolae_2000_to_2023, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis() +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle('BMS Observations of Suillus bresadolae: 2000-2023')

ggplot(data = bresadolae_2000_to_2023, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus bresadolae var. flavogriseus: 2000-2023')


cavipes <- sites[sites$name == "Suillus cavipes", ]

cavipes_50s_to_2023 <- cavipes[cavipes$Year >= 1950, ]

cavipes_90s_to_2023 <- cavipes[cavipes$Year >= 1990, ]

cavipes_2000_to_2023 <- cavipes[cavipes$Year >= 2000, ]

cavipesRegression <- lm(cavipes_2000_to_2023$Year ~ cavipes_2000_to_2023$latitude)
cavipesRsquared <- cor(cavipes_2000_to_2023$Year, cavipes_2000_to_2023$latitude)

cavipesRegression  %>%
  tbl_regression()

ggplot(data = world) +
  geom_sf() +
  geom_point(data = cavipes, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis(option = "plasma") +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle('BMS Observations of Suillus cavipes')

ggplot(data = cavipes, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus cavipes')

ggplot(data = world) +
  geom_sf() +
  geom_point(data = cavipes_2000_to_2023, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis() +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus cavipes: 2000-2023')

ggplot(data = cavipes_2000_to_2023, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus cavipes: 2000-2023')


collinitus <- sites[sites$name == "Suillus collinitus", ]

collinitus_50s_to_2023 <- collinitus[collinitus$Year >= 1950, ]

collinitus_90s_to_2023 <- collinitus[collinitus$Year >= 1990, ]

collinitus_2000_to_2023 <- collinitus[collinitus$Year >= 2000, ]

collinitusRegression <- lm(collinitus_2000_to_2023$Year ~ collinitus_2000_to_2023$latitude)
collinitusRsquared <- cor(collinitus_2000_to_2023$Year, collinitus_2000_to_2023$latitude)

collinitusRegression  %>%
  tbl_regression()

ggplot(data = world) +
  geom_sf() +
  geom_point(data = collinitus, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis(option = "plasma") +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle('BMS Observations of Suillus collinitus')

ggplot(data = collinitus, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus collinitus')

ggplot(data = world) +
  geom_sf() +
  geom_point(data = collinitus_2000_to_2023, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis() +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle('BMS Observations of Suillus collinitus: 2000-2023')

ggplot(data = collinitus_2000_to_2023, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus collinitus: 2000-2023')


flavidus <- sites[sites$name == "Suillus flavidus", ]

flavidus_50s_to_2023 <- flavidus[flavidus$Year >= 1950, ]

flavidus_90s_to_2023 <- flavidus[flavidus$Year >= 1990, ]

flavidus_2000_to_2023 <- flavidus[flavidus$Year >= 2000, ]

flavidusRegression <- lm(flavidus_2000_to_2023$Year ~ flavidus_2000_to_2023$latitude)
flavidusRsquared <- cor(flavidus_2000_to_2023$Year, flavidus_2000_to_2023$latitude)

flavidusRegression  %>%
  tbl_regression()

ggplot(data = world) +
  geom_sf() +
  geom_point(data = flavidus, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis(option = "plasma") +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle('BMS Observations of Suillus flavidus')

ggplot(data = flavidus, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus flavidus')

ggplot(data = world) +
  geom_sf() +
  geom_point(data = flavidus_2000_to_2023, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis() +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle('BMS Observations of Suillus flavidus: 2000-2023')

ggplot(data = flavidus_2000_to_2023, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus flavidus: 2000-2023')


granulatus <- sites[sites$name == "Suillus granulatus", ]

granulatus_50s_to_2023 <- granulatus[granulatus$Year >= 1950, ]

granulatus_90s_to_2023 <- granulatus[granulatus$Year >= 1990, ]

granulatus_2000_to_2023 <- granulatus[granulatus$Year >= 2000, ]

granulatusRegression <- lm(granulatus_2000_to_2023$Year ~ granulatus_2000_to_2023$latitude)
granulatusRsquared <- cor(granulatus_2000_to_2023$Year, granulatus_2000_to_2023$latitude)

granulatusRegression  %>%
  tbl_regression()

ggplot(data = world) +
  geom_sf() +
  geom_point(data = granulatus, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis(option = "plasma") +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle('BMS Observations of Suillus granulatus')

ggplot(data = granulatus, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus granulatus')

ggplot(data = world) +
  geom_sf() +
  geom_point(data = granulatus_90s_to_2023, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis() +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle('BMS Observations of Suillus granulatus: 1990-2023')

ggplot(data = granulatus_90s_to_2023, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus granulatus: 1990-2023')

ggplot(data = world) +
  geom_sf() +
  geom_point(data = granulatus_2000_to_2023, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis() +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle('BMS Observations of Suillus granulatus: 2000-2023')

ggplot(data = granulatus_2000_to_2023, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus granulatus: 2000-2023')


grevillei <- sites[sites$name == "Suillus grevillei", ]

grevillei_50s_to_2023 <- grevillei[grevillei$Year >= 1950, ]

grevillei_90s_to_2023 <- grevillei[grevillei$Year >= 1990, ]

grevillei_2000_to_2023 <- grevillei[grevillei$Year >= 2000, ]

grevilleiRegression <- lm(grevillei_2000_to_2023$Year ~ grevillei_2000_to_2023$latitude)
grevilleiRsquared <- cor(grevillei_2000_to_2023$Year, grevillei_2000_to_2023$latitude)

grevilleiRegression  %>%
  tbl_regression()

ggplot(data = world) +
  geom_sf() +
  geom_point(data = grevillei, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis(option = "plasma") +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle('BMS Observations of Suillus grevillei')

ggplot(data = grevillei, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus grevillei')

ggplot(data = world) +
  geom_sf() +
  geom_point(data = grevillei_90s_to_2023, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis() +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle('BMS Observations of Suillus grevillei: 1990-2023')

ggplot(data = grevillei_90s_to_2023, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus grevillei: 1990-2023')

ggplot(data = world) +
  geom_sf() +
  geom_point(data = grevillei_2000_to_2023, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis() +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle('BMS Observations of Suillus grevillei: 2000-2023')

ggplot(data = grevillei_2000_to_2023, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus grevillei: 2000-2023')


luteus <- sites[sites$name == "Suillus luteus", ]

luteus_50s_to_2023 <- luteus[luteus$Year >= 1950, ]

luteus_90s_to_2023 <- luteus[luteus$Year >= 1990, ]

luteus_2000_to_2023 <- luteus[luteus$Year >= 2000, ]

luteusRegression <- lm(luteus_2000_to_2023$Year ~ luteus_2000_to_2023$latitude)
luteusRsquared <- cor(luteus_2000_to_2023$Year, luteus_2000_to_2023$latitude)

luteusRegression  %>%
  tbl_regression()

ggplot(data = world) +
  geom_sf() +
  geom_point(data = luteus, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis(option = "plasma") +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle('BMS Observations of Suillus luteus')

ggplot(data = luteus, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus luteus')

ggplot(data = world) +
  geom_sf() +
  geom_point(data = luteus_90s_to_2023, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis() +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle('BMS Observations of Suillus luteus: 1990-2023')

ggplot(data = luteus_90s_to_2023, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus luteus: 1990-2023')

ggplot(data = world) +
  geom_sf() +
  geom_point(data = luteus_2000_to_2023, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis() +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle('BMS Observations of Suillus luteus: 2000-2023')

ggplot(data = luteus_2000_to_2023, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus luteus: 2000-2023')


placidus <- sites[sites$name == "Suillus placidus", ]

placidus_50s_to_2023 <- placidus[placidus$Year >= 1950, ]

placidus_90s_to_2023 <- placidus[placidus$Year >= 1990, ]

#### No observations of Suillus placidus between 1990-2023

#placidus_2000_to_2023 <- placidus[placidus$Year >= 2000, ]

#placidusRegression <- lm(placidus_2000_to_2023$Year ~ placidus_2000_to_2023$latitude)
#placidusRsquared <- cor(placidus_2000_to_2023$Year, placidus_2000_to_2023$latitude)

ggplot(data = world) +
  geom_sf() +
  geom_point(data = placidus, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis(option = "plasma") +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle('BMS Observations of Suillus placidus')

ggplot(data = placidus, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus placidus')



tridentinus <- sites[sites$name == "Suillus tridentinus", ]

tridentinus_50s_to_2023 <- tridentinus[tridentinus$Year >= 1950, ]

tridentinus_90s_to_2023 <- tridentinus[tridentinus$Year >= 1990, ]

tridentinus_2000_to_2023 <- tridentinus[tridentinus$Year >= 2000, ]

tridentinusRegression <- lm(tridentinus_2000_to_2023$Year ~ 
                              tridentinus_2000_to_2023$latitude)
tridentinusRsquared <- cor(tridentinus_2000_to_2023$Year, tridentinus_2000_to_2023$latitude)

tridentinusRegression  %>%
  tbl_regression()

ggplot(data = world) +
  geom_sf() +
  geom_point(data = tridentinus, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis(option = "plasma") +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle('BMS Observations of Suillus tridentinus')

ggplot(data = tridentinus, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus tridentinus')

ggplot(data = world) +
  geom_sf() +
  geom_point(data = tridentinus_2000_to_2023, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis() +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle('BMS Observations of Suillus tridentinus: 2000-2023')

ggplot(data = tridentinus_2000_to_2023, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus tridentinus: 2000-2023')

tridentinus_2000_to_2023NoOutlier <- tridentinus_2000_to_2023[tridentinus_2000_to_2023$latitude <= 55, ]

ggplot(data = tridentinus_2000_to_2023NoOutlier, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude")

tridentinusNoOutlierRegression <- lm(tridentinus_2000_to_2023NoOutlier$Year ~ 
                              tridentinus_2000_to_2023NoOutlier$latitude)

tridentinusNoOutlierRegression  %>%
  tbl_regression()

tridentinusNoOutlierRsquared <- cor(tridentinus_2000_to_2023NoOutlier$Year, tridentinus_2000_to_2023NoOutlier$latitude)


variegatus <- sites[sites$name == "Suillus variegatus", ]

variegatus_50s_to_2023 <- variegatus[variegatus$Year >= 1950, ]

variegatus_90s_to_2023 <- variegatus[variegatus$Year >= 1990, ]

variegatus_2000_to_2023 <- variegatus[variegatus$Year >= 2000, ]

variegatusRegression <- lm(variegatus_2000_to_2023$Year ~ variegatus_2000_to_2023$latitude)
variegatusRsquared <- cor(variegatus_2000_to_2023$Year, variegatus_2000_to_2023$latitude)

variegatusRegression  %>%
  tbl_regression()

ggplot(data = world) +
  geom_sf() +
  geom_point(data = variegatus, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis(option = "plasma") +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle('BMS Observations of Suillus variegatus')

ggplot(data = variegatus, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus variegatus')

ggplot(data = world) +
  geom_sf() +
  geom_point(data = variegatus_90s_to_2023, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis() +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle('BMS Observations of Suillus variegatus: 1990-2023')

ggplot(data = variegatus_90s_to_2023, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus variegatus: 1990-2023')

ggplot(data = world) +
  geom_sf() +
  geom_point(data = variegatus_2000_to_2023, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis() +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle('BMS Observations of Suillus variegatus: 2000-2023')

ggplot(data = variegatus_2000_to_2023, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus variegatus: 2000-2023')


viscidus <- sites[sites$name == "Suillus viscidus", ]

viscidus_50s_to_2023 <- viscidus[viscidus$Year >= 1950, ]

viscidus_90s_to_2023 <- viscidus[viscidus$Year >= 1990, ]

viscidus_2000_to_2023 <- viscidus[viscidus$Year >= 2000, ]

viscidusRegression <- lm(viscidus_2000_to_2023$Year ~ viscidus_2000_to_2023$latitude)
viscidusRsquared <- cor(viscidus_2000_to_2023$Year, viscidus_2000_to_2023$latitude)

viscidusRegression  %>%
  tbl_regression()

ggplot(data = world) +
  geom_sf() +
  geom_point(data = viscidus, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis(option = "plasma") +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle('BMS Observations of Suillus viscidus')

ggplot(data = viscidus, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus viscidus')

ggplot(data = world) +
  geom_sf() +
  geom_point(data = viscidus_90s_to_2023, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis() +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle('BMS Observations of Suillus viscidus: 1990-2023')

ggplot(data = viscidus_90s_to_2023, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus viscidus: 1990-2023')

ggplot(data = world) +
  geom_sf() +
  geom_point(data = viscidus_2000_to_2023, 
             aes(x = longitude, 
                 y = latitude, 
                 color = Year), 
             size = 1, 
             shape = 20) +
  coord_sf(xlim = c(-8.25, 2), 
           ylim = c(49.75, 59.5), 
           expand = FALSE) +
  scale_color_viridis() +
  theme(legend.position = "right") +
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle('BMS Observations of Suillus viscidus: 2000-2023')

ggplot(data = viscidus_2000_to_2023, 
       aes(x = Year, 
           y = latitude)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              color = "red") +
  xlab("Year") + 
  ylab("Latitude") +
  ggtitle('BMS Observations of Suillus viscidus: 2000-2023')

