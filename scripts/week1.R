##------------------------------------------------------------------------------
## Spatial Analysis in Epidemiology 
## Week 1 practical exercises
##------------------------------------------------------------------------------

# Load packages
library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial) #scale bar & arrow, annotation_scale() & annotation_north_arrow()

## (1) SHAPE FILES -------------------------------------------------------------

# Load spatial data
cheshire_map <- st_read("maps/cheshire_map.shp")  #Cheshire map shape file
cheshire_fmd <- st_read("maps/cheshire_fmd.shp")  #Cheshire FMD shape file

# Check coordinate reference system (CRS) of data
st_crs(cheshire_map)  #OSGB36 / British National Grid

# Set the CRS for the point data to be same as the map
cheshire_fmd <- st_set_crs(cheshire_fmd, st_crs(cheshire_map))

# Plot the map of Cheshire & distribution of FMD cases
ggplot() +  
  geom_sf(data = cheshire_map, fill="lightgreen") + 
  geom_point(data = cheshire_fmd, aes(XCOORD, YCOORD), shape=16, col="darkred") + 
  theme_void() + 
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "br", which_north = "true", style = north_arrow_nautical)
  

# How many parishes make up Cheshire?
cheshire_map %>%
  select(PARISHN) %>%
  unique() %>%
  nrow()

# How many farm holdings are there in Cheshire?
cheshire_fmd %>% 
  nrow()

## (2) RASTER MAPS -------------------------------------------------------------
