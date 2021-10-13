#AustinBoulderScratch


#Libraries & Environment
library(tidyverse)
library(sf)
library(spdep)
library(caret)
library(ckanr)
library(FNN)
library(grid)
library(gridExtra)
library(ggcorrplot)
library(kableExtra)
library(jtools)     # for regression model plots
library(ggstance) # to support jtools plots
library(Rcpp)
library(mapview)
library(dplyr)

root.dir = ("C:/Users/austi/Desktop/Penn/Classes/PublicPolicy/Assignments/")
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

# color set up - yellow to green
palette1 <- c('#f2d6a2', '#dfd688', '#ccd270', '#b8ca58', '#a4bf41')  # lighter one
palette2 <- c('#ffd358', '#e1cf5b', '#b9bc4e', '#879a32', '#4f6d00')   # stronger one
palette3 <- c('#f2d6a2', '#e9d596', '#dfd38a', '#d6d17e', '#ccce72', '#c2cb66', '#b8c85a', '#aec44d', '#a4bf41')  # lighter one with more gradient

#studentData
physical_data <- st_read("C:/Users/austi/Desktop/Penn/Classes/PublicPolicy/Assignments/Midterm/studentData.geojson") %>% 
  st_set_crs('ESRI:102254')
st_crs(physical_data)
mapview(physical_data)

#City of Boulder Boundary
city_boundary <- st_read("https://opendata.arcgis.com/datasets/955e7a0f52474b60a9866950daf10acb_0.geojson") %>% 
  st_transform('ESRI:102254') %>% 
  st_union() %>% 
  st_sf()
mapview(city_boundary)

#Boulder County Outline
county_boundary <- st_read("https://opendata.arcgis.com/datasets/964b8f3b3dbe401bb28d49ac93d29dc4_0.geojson") %>% 
  st_transform('ESRI:102254') %>% 
  st_union() %>% 
  st_sf()
mapview(county_boundary)

# % Sales within City
city_phy_data <- physical_data[city_boundary,]
mapview(city_phy_data)
proportion <- nrow(distinct(city_phy_data))/nrow(distinct(physical_data))

#City of Boulder Historic Districts
histDists <- st_read("https://opendata.arcgis.com/datasets/643848efa2e24bdf8a8eafba67ec3d43_1.geojson") %>%
  st_transform('ESRI:102254')
mapview(histDists)

physical_data[histDists,] %>%
  dplyr::mutate(Historic = "Yes")


# County Parks
countyParks <- st_read("https://opendata.arcgis.com/datasets/61728921abcb481fa98b9b07cfd7c95d_0.geojson") %>%
  st_transform('ESRI:102254') %>%
  filter(PARK_GROUP != "NA") %>%
  select("PROP_NAME")
mapview(countyParks)

#crime

crime <- read.csv("C:/Users/austi/Desktop/Penn/Classes/PublicPolicy/Assignments/Midterm/Crime_Data.csv")

group_by(crime,CAD.Problem) %>%
  summarize(count = n()) %>%
  arrange(-count) %>% top_n(5)

crime.sf <-
  crime %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102254')

crime.sf <-
  crime %>%
  filter(CAD.Problem == "ASSAUS-Assault",
         Latitude > -1) %>%
  dplyr::select(Latitude, Longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102254') %>%
  distinct()

ggplot() + geom_sf(data = county_boundary, fill = "grey40") +
  stat_density2d(data = data.frame(st_coordinates(crime.sf)), 
                 aes(X, Y, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 40, geom = 'polygon') +
  scale_fill_gradient(low = "#25CB10", high = "#FA7800", 
                      breaks=c(0.000000003,0.00000003),
                      labels=c("Minimum","Maximum"), name = "Density") +
  scale_alpha(range = c(0.00, 0.35), guide = FALSE) +
  labs(title = "Density of Aggravated Assaults, Boulder County, CO") +
  mapTheme()

#City Parks
cityPark <- st_read("https://opendata.arcgis.com/datasets/fdfc0410e75c48b6b9e53cf94bdbe224_1.geojson") %>%
  st_transform('ESRI:102254') %>%
  filter(PARKTYPE != "Undeveloped") %>%
  dplyr::rename(PROP_NAME=NAME)

cityPark_cleaned <- cityPark[ -c(4:38) ]
cityPark_scrubbed <- cityPark_cleaned[ -c(1:2, 4:6)]
mapview(cityPark_scrubbed)

#County + City Parks
allParks <- rbind(countyParks,cityPark_scrubbed)
mapview(allParks)
#buffer on each house, how many parks it intersects


#Trailheads
trailheads <- st_read("https://opendata.arcgis.com/datasets/3a950053bbef46c6a3c2abe3aceee3de_0.geojson") %>%
  st_transform('ESRI:102254')
mapview(trailheads)
#buffer on each house, how many points fall within

#####FEATURE ENGINEERING#####

sale2 <- physical_data

#FE - Parks#

allParks <- allParks %>% 
  mutate(allParks, counter=1)
as.numeric(allParks$counter)

sale2$parkCount =
  st_buffer(sale2, 660) %>%
  aggregate(dplyr::select(allParks,counter),.,sum) %>%
  pull(counter)

sale2$parkCount[ is.na(sale2$parkCount)] <- 0

#FE Crime

crime.sf$crimes.Buffer =
  st_buffer(crime.sf, 660) %>%
  aggregate(mutate(crime.sf, counter = 1),., sum) %>%
  pull(counter)

st_c <- st_coordinates

sale2 <-
  sale2 %>% 
  mutate(
    crime_nn1 = nn_function(st_c(sale2), st_c(crime.sf), 1),
    crime_nn2 = nn_function(st_c(sale2), st_c(crime.sf), 2), 
    crime_nn3 = nn_function(st_c(sale2), st_c(crime.sf), 3), 
    crime_nn4 = nn_function(st_c(sale2), st_c(crime.sf), 4), 
    crime_nn5 = nn_function(st_c(sale2), st_c(crime.sf), 5)) 
#FE Historic Districts

sale2 <-  st_join(
  x=sale2, 
  y=histDists, 
  join = st_within, 
  left = TRUE
)

sale2.inhist <- sale2 %>% 
  filter(!is.na(OBJECTID)) %>% 
  mutate(in_hist = "Yes") 
sale2.nohist <- sale2 %>% 
  filter(is.na(OBJECTID)) %>% 
  mutate(in_hist = "No") 
sale2 <- rbind(sale2.nohist,sale2.inhist)

sale2 <- sale2 %>%
  select(-OBJECTID,-NAME,-DESIGNATION,-SHAPESTArea,-SHAPESTLength)

#FE - Trailheads

trailheads <- trailheads %>% 
  mutate(trailheads, counter=1)
as.numeric(trailheads$counter)

sale2$trailheadCount =
  st_buffer(sale2, 660) %>%
  aggregate(dplyr::select(trailheads,counter),.,sum) %>%
  pull(counter)

sale2$trailheadCount[ is.na(sale2$trailheadCount)] <- 0