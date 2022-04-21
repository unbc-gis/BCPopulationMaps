library(readr)
library(dplyr)
library(fuzzyjoin)
library(ggplot2)
library(sf)

BCPopulation <- read_csv("BCPopulation1971-2021.csv") # Import Population From CSV
years = names(BCPopulation)[3:length(names(BCPopulation))] # Ignore Names from CSV


boundaries <- st_read("basemap/REGIONAL_DISTRICTS.geojson") # Open Regional Districts

pop_polygons <- merge(boundaries, BCPopulation) # Add pop data to polygons


st_crs(pop_polygons) # Test that data has projection



for (i in length(years):2){
  for (j in (i-1):1){
    diff_name = paste(years[j], "_", years[i], sep="")
    print(diff_name)
    pop_polygons[paste("change_", diff_name, sep = "")] = BCPopulation[years[i]] - BCPopulation[years[j]]
    pop_polygons[paste("pct_", diff_name, sep = "")] = ((1 / BCPopulation[years[j]]) * BCPopulation[years[i]]) - 1
  }
  #diff_name = paste(years[n], "-", years[n-1], sep="")
  #pop_polygons[diff_name] = BCPopulation[years[n]] - BCPopulation[years[n-1]]
  #pop_polygons[paste(diff_name, "_pct", sep = "")] = ((1 / BCPopulation[years[n-1]]) * BCPopulation[years[n]]) - 1
  #print(diff_name)
}

col.range=c(1,8)

plot(pop_polygons['1971_1981_pct'], pal = colorRampPalette(c("red", "white", "blue")), limits = col.range)

map <- ggplot() + geom_sf(data = pop_polygons, aes(fill = pct_1971_1981)) +
  scale_colour_steps()
map
#pop_polygons$'test' = BCPopulation[years[2]] - BCPopulation[years[2]]
#years
#pop_polygons$'test'

#pop_polygons[years[2]]
