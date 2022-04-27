library(readr)
library(dplyr)
library(ggplot2)
library(sf)

low_pct = -0.4
high_pct = 0.4

low_colour = 'red'
mid_colour = 'white'
high_colour = 'blue'



BCPopulation <- read_csv("BCPopulation1971-2021.csv") # Import Population From CSV
years = names(BCPopulation)[3:length(names(BCPopulation))] # Ignore Names from CSV


boundaries <- st_read("basemap/REGIONAL_DISTRICTS.geojson") # Open Regional Districts

pop_polygons <- merge(boundaries, BCPopulation) # Add pop data to polygons


st_crs(pop_polygons) # Test that data has projection

for (i in length(years):2){
  for (j in (i-1):1){
    diff_name = paste0(years[j], "_", years[i])
    print(diff_name)
    pop_polygons[paste0("change_", diff_name)] = 
      BCPopulation[years[i]] - BCPopulation[years[j]]
    pct = ((1 / BCPopulation[years[j]]) * BCPopulation[years[i]]) - 1
    pop_polygons[paste0("pct_", diff_name)] = pct

    pop_polygons[paste0("colour_", diff_name)] = 
      ifelse(pct <= low_pct, 'red', 
             ifelse(pct < high_pct, 'white', 'blue'))
  }
}

i = '1976'
j = '2021'

target_column = paste0("pct_", i, "_", j)
map <- ggplot() + geom_sf(data = pop_polygons, aes(fill = get(target_column))) +
    scale_fill_binned(breaks = c(-0.4, 0.1, 0.2, 0.4),
                      show.limits = TRUE) +
  guides(fill=guide_legend(title=paste0("British Columbia\n%pop. change ", i, "-", j)))

map + theme_void() + theme(legend.background = element_rect(linetype = 1, size = 0.25, colour = 1), 
                           legend.margin = margin(.25, 0.25, 0.25, 0.25, unit='cm'),
                           legend.box.margin = margin(-7, 0, 0, -4, unit='cm'),
                           legend.title.align = 0.5)

map <- ggplot() + geom_sf(data = pop_polygons, aes(fill = get(target_column))) +
  scale_fill_gradient2(  limits = c(-1, 1),
                        low = low_colour,
                        mid = mid_colour,
                        midpoint = 0,
                        high = high_colour,
                        guide = "colourbar",
                        aesthetics = "fill") +
  guides(fill=guide_legend(title=paste0("British Columbia\n%pop. change ", i, "-", j)))

map + theme_void() + theme(legend.background = element_rect(linetype = 1, size = 0.25, colour = 1), 
                           legend.margin = margin(.25, 0.25, 0.25, 0.25, unit='cm'),
                           legend.box.margin = margin(-7, 0, 0, -4, unit='cm'),
                           legend.title.align = 0.5)

                           

                           
                           