library(readr)
library(dplyr)
library(fuzzyjoin)
library(ggplot2)
library(sf)
BCPopulation <- read_csv("BCPopulation1971-2021.csv")
boundaries <- st_read("basemap/REGIONAL_DISTRICTS.geojson")
print(BCPopulation)
pop_polygons <- merge(boundaries, BCPopulation)


st_crs(pop_polygons)
plot(pop_polygons['1971'])


