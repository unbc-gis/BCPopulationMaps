library(readr)
library(dplyr)
library(ggplot2)
library(sf)

BCPopulation <- read_csv("BCPopulation1971-2021.csv") # Import Population From CSV
years = names(BCPopulation)[3:length(names(BCPopulation))] # Ignore Names from CSV


boundaries <- st_read("basemap/REGIONAL_DISTRICTS.geojson") # Open Regional Districts

pop_polygons <- merge(boundaries, BCPopulation) # Add pop data to polygons


st_crs(pop_polygons) # Test that data has projection



for (i in length(years):2){
  for (j in (i-1):1){
    diff_name = paste0(years[j], "_", years[i])
    print(diff_name)
    pop_polygons[paste0("change_", diff_name)] = BCPopulation[years[i]] - BCPopulation[years[j]]
    pop_polygons[paste0("pct_", diff_name)] = ((1 / BCPopulation[years[j]]) * BCPopulation[years[i]]) - 1
  }
}

col.range=c(1,8)

i = '1971'
j = '1981'
target_column = paste0("pct_", i, "_", j)
map <- ggplot() + geom_sf(data = pop_polygons, aes(fill = get(target_column))) +
  scale_fill_stepsn(colors = c("red", "white", "blue"),
                   space = "Lab",
                   na.value = "grey50",
                   n.breaks = 3,
                   limits = c(-0.4,.4),
                   guide = "coloursteps",
                   aesthetics = "fill")+ guides(fill=guide_legend(title=paste0("British Columbia\npop. change ", i, "-", j)))

map + theme_void() + theme(legend.background = element_rect(linetype = 1, size = 0.25, colour = 1), 
                           legend.margin = margin(.25, 0.25, 0.25, 0.25, unit='cm'),
                           legend.box.margin = margin(-8, 0, 0, -5, unit='cm'),
                           legend.title.align = 0.5)
                           

                           