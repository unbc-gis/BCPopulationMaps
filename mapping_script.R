library(readr)
library(dplyr)
library(ggplot2)
library(sf)

low_pct = -0.4
high_pct = 0.4

lim = 1 #Distance for 0 where colour is maximum saturation

low_colour = 'red'
mid_colour = 'white'
high_colour = 'blue'

low_label = paste0("< ", toString(low_pct), "%")
high_label = paste0("> ", toString(high_pct), "%")

BCPopulation <- read_csv("BCPopulation1971-2021.csv") # Import Population From CSV
years = names(BCPopulation)[3:length(names(BCPopulation))] # Ignore Names from CSV


boundaries <- st_read("basemap/REGIONAL_DISTRICTS.geojson") # Open Regional Districts

pop_polygons <- merge(boundaries, BCPopulation) # Add pop data to polygons


st_crs(pop_polygons) # Test that data has projection

for (i in length(years):2) {
  for (j in (i-1):1) {
    diff_name = paste0(years[j], "_", years[i])
    print(diff_name)
    pop_polygons[paste0("change_", diff_name)] = 
      BCPopulation[years[i]] - BCPopulation[years[j]]
    pct = ((1 / BCPopulation[years[j]]) * BCPopulation[years[i]]) - 1
    pop_polygons[paste0("pct_", diff_name)] = pct

    pop_polygons[paste0("colour_", diff_name)] = 
      ifelse(pct <= low_pct, 'low', 
             ifelse(pct < high_pct, 'mid', 'high'))
  }
}

for (i in length(years):2) {
  for (j in (i-1):1) {
    pct_column = paste0("pct_", years[j], "_", years[i])
    colour_column = paste0("colour_", years[j], "_", years[i])
    
    gradiant_out = paste0("output/gradient/", pct_column)
    
    pct_map <- ggplot() + geom_sf(data = pop_polygons, color = 'black', aes(fill = get(pct_column))) +
      scale_fill_gradient2(low = low_colour, mid = mid_colour, midpoint = 0,
                           high = high_colour, limits = c(-lim, lim), n.breaks = 3,
                           oob = scales::squish, labels = c(paste0("< -", lim, "%"), "0%", paste0("> ", lim, "%"))) +
      guides(fill=guide_colorbar(title=paste0("British Columbia\n%pop. change ", years[j], "-", years[i]))) +
      theme_void() + theme(legend.background = element_rect(linetype = 1, size = 0.25, colour = 1), 
                           legend.margin = margin(.25, 0.25, 0.25, 0.25, unit='cm'),
                           legend.box.margin = margin(-7.8, 0, 0, -5.6, unit='cm'),
                           legend.title.align = 0.5)
    
    #ggsave(plot = pct_map, width = 7, height = 6, dpi = 1200, filename = paste0(gradiant_out, ".jpg"))
    ggsave(plot = pct_map, width = 7, height = 6, dpi = 1200, filename = paste0(gradiant_out, ".png"))
    
    colour_column = paste0("colour_", years[j], "_", years[i])
    classified_out = paste0("output/classified/", colour_column)
    
    colour_map <- ggplot() + geom_sf(data = pop_polygons, color = 'black', aes(fill = factor(get(colour_column)))) +
      scale_fill_manual(na.value = "grey50", values = c(low = low_colour, mid = mid_colour, high = high_colour),
                        labels = c(low_label, "", high_label)) +
      guides(fill=guide_legend(title=paste0("British Columbia\n%pop. change ", years[j], "-", years[i]), reverse = TRUE)) +
      theme_void() + theme(legend.background = element_rect(linetype = 1, size = 0.25, colour = 1), 
                           legend.margin = margin(.25, 0.25, 0.25, 0.25, unit='cm'),
                           legend.box.margin = margin(-7.8, 0, 0, -5.6, unit='cm'),
                           legend.title.align = 0.5)
    
    #ggsave(plot = colour_map, width = 7, height = 6, dpi = 1200, filename = paste0(classified_out, ".jpg"))
    ggsave(plot = colour_map, width = 7, height = 6, dpi = 1200, filename = paste0(classified_out, ".png"))
  }
}

