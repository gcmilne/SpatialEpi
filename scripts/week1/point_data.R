# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Spatial Analysis in Epidemiology
#
# Week 1 practical exercises:
# Visualising point data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load packages
library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial) #scale bar & arrow, annotation_scale() & annotation_north_arrow()
library(incidence) #as.incidence() for plotting epi curve

# Load & format spatial data ---------------------------------------------------
cheshire_map <- st_read("maps/cheshire_map.shp")  #Cheshire map shape file
cheshire_fmd <- st_read("maps/cheshire_fmd.shp")  #Cheshire FMD shape file

# Check coordinate reference system (CRS) of data
st_crs(cheshire_map)  #OSGB36 / British National Grid

# Set the CRS for the point data to be same as the map
cheshire_fmd <- st_set_crs(cheshire_fmd, st_crs(cheshire_map))

# Rename FMD factor levels
cheshire_fmd <- cheshire_fmd %>%
  mutate(STATUS = case_when(
    STATUS == 0 ~ "Negative",
    STATUS == 1 ~ "Positive"
  ))

# Map the distribution of FMD cases in Cheshire --------------------------------
ggplot() +  
  # cheshire map
  geom_sf(data = cheshire_map, fill="lightgreen", alpha=.5) + 
  # FMD points, coloured by positive status
  geom_point(data = cheshire_fmd, aes(XCOORD, YCOORD, col=as.factor(STATUS)), shape=16, alpha=.7) + 
  theme_void() + 
  # scale bar
  annotation_scale(location = "bl", width_hint = 0.5) + 
  # north arrow
  annotation_north_arrow(location = "br", which_north = "true", height=unit(1, "cm"), 
                         width=unit(1, "cm")) + 
  # set manual colours
  scale_color_manual(values = RColorBrewer::brewer.pal(2, "Set1")[c(2,1)]) + 
  labs(col="FMD status")

# Save map
ggsave("plots/cheshire_fmd_points.pdf", device = cairo_pdf, height = 6, width = 6, 
       units = "in")

# How many parishes make up Cheshire?
cheshire_map %>%
  select(PARISHN) %>%
  unique() %>%
  nrow()

# How many farm holdings are there in Cheshire?
cheshire_fmd %>% 
  nrow()

# First & last date of infection
range(cheshire_fmd$DATE, na.rm = TRUE)

## Visualise the temporal evolution of cases -----------------------------------

# create case count data 
count_data <- cheshire_fmd %>%
  group_by(DATE) %>%
  summarize(n_cases = dplyr::n()) %>%
  ungroup() %>%
  na.omit()

# plot epicurve
epi_curve <- as.incidence(count_data$n_cases, count_data$DATE, interval = 1)

plot(epi_curve) + 
  labs(x="Date", y="Number of cases") + 
  theme_minimal()
