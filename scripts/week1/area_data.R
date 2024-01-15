# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Spatial Analysis in Epidemiology
#
# Week 1 practical exercises:
# Visualising area data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load packages ----------------------------------------------------------------
library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial) #scale bar & arrow, annotation_scale() & annotation_north_arrow()


# Load & format spatial data ---------------------------------------------------

# Lip cancer rates in Scotland, 1975-1980
cancer_dat <- read.csv("data/sc_data.csv")
# head(cancer_dat)

# Scotland shape file
scot_map <- st_read("maps/SC_MAP.shp") %>%
  st_set_crs("EPSG:2397")  #set CRS

# Join the shapefile with the cancer data based on common variables
joined_dat <- left_join(scot_map, cancer_dat)

# check distribution of data
hist(joined_dat$PROP_AG)
range(joined_dat$PROP_AG)


# Plot map of proportion in agricultural industry ------------------------------

# Create levels for proportion in agricultural industry
ag_factor_levels <- c("0", "<2", "2-7", "8-11", "11-24")

agri_map <- joined_dat %>%
  # group according to factor levels
  mutate(
    tidy_propag = case_when(
      PROP_AG == 0                ~ ag_factor_levels[1],
      PROP_AG > 0 & PROP_AG < 2   ~ ag_factor_levels[2],
      PROP_AG >= 2 & PROP_AG < 7  ~ ag_factor_levels[3],
      PROP_AG >= 7 & PROP_AG < 11 ~ ag_factor_levels[4],
      PROP_AG >= 11               ~ ag_factor_levels[5],
    ), 
    # ensure correct ordering of factor
    tidy_propag = factor(tidy_propag, levels = ag_factor_levels)
    )%>%
  # Plot tidied data as chloropleth map
  ggplot() + 
  geom_sf() + 
  geom_sf(aes(fill=tidy_propag)) + 
  theme_void() +
  # north arrow
  annotation_north_arrow(location = "br", which_north = "true", height=unit(1, "cm"), 
                         width=unit(1, "cm")) + 
  # annotation_scale(location = "bl", width_hint = 0.5, plot_unit = "km") +
  labs(fill="Proportion in agricultural industry (%)") + 
  scale_fill_manual(values=RColorBrewer::brewer.pal(4, "Greens"))


# Plot map of case numbers -----------------------------------------------------

# create labels for factor levels
factor_levels = c("0-3", ">3-7", ">7-9", ">9-15", ">15-39")

cancer_map <- joined_dat %>%
  # group according to factor levels
  mutate(
   grouped_cases = cut(CASES, breaks=quantile(CASES, probs=seq(0,1,0.2)), include.lowest=TRUE),
   grouped_cases_labels = case_when(
     grouped_cases == levels(grouped_cases)[1] ~ factor_levels[1],
     grouped_cases == levels(grouped_cases)[2] ~ factor_levels[2],
     grouped_cases == levels(grouped_cases)[3] ~ factor_levels[3],
     grouped_cases == levels(grouped_cases)[4] ~ factor_levels[4],
     grouped_cases == levels(grouped_cases)[5] ~ factor_levels[5]
   ), 
   # ensure correct ordering of factor
   grouped_cases_labels = factor(grouped_cases_labels, levels = factor_levels)
  ) %>%
  # Plot tidied data as chloropleth map
  ggplot() + 
  geom_sf() + 
  geom_sf(aes(fill=grouped_cases_labels)) + 
  theme_void() +
  # north arrow
  annotation_north_arrow(location = "br", which_north = "true", height=unit(1, "cm"), 
                         width=unit(1, "cm")) + 
  # annotation_scale(location = "bl", width_hint = 0.5, plot_unit = "km") +
  labs(fill="No. lip cancer cases, 1975-1980") +
  scale_fill_manual(values=RColorBrewer::brewer.pal(5, "Reds"))


# Save maps --------------------------------------------------------------------

# agricultural map
ggsave("plots/scotland_agricultural_industry_map.pdf", plot=agri_map, device=cairo_pdf, 
       height=6, width=6, units="in")

# cancer cases map
ggsave("plots/scotland_cancer_map.pdf", plot=cancer_map, device=cairo_pdf, 
       height=6, width=6, units="in")
