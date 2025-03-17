
# day -1 point - Military Airport
# https://gis.data.ca.gov/datasets/1927f89a86754849a430c5d9584bf8dd_0/explore

setwd("/Users/jiatong/Desktop/30daysMap/day_1_point")

library(sf)
library(ggplot2)
library(tigris)
library(sf)
library(ggimage)  # For using PNG images as symbols

# Load necessary libraries
library(tigris)
library(dplyr)
library(ggspatial)  # For scale bar and north arrow
library(ggrepel)  # For auto-adjusting text labels



CA_airplot <- st_read("https://caltrans-gis.dot.ca.gov/arcgis/rest/services/CHaviation/Public_Airport/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

CA_boundary <- st_read("https://services3.arcgis.com/fdvHcZVgB2QSRNkL/arcgis/rest/services/State_Boundary/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")


Primary_airports <- CA_airplot %>%
  filter(FNCTNLCLSS == "Commercial/Primary")



# Extract coordinates from the Primary_airports sf object
Primary_airports_coords <- st_coordinates(Primary_airports) %>%
  as.data.frame()

# Check the structure of the coordinates data frame
print(head(Primary_airports_coords))

# Check the names of the Primary_airports data frame
print(names(Primary_airports))

# Add AIRPORTID as a column to the coordinates data frame
Primary_airports_coords <- cbind(Primary_airports_coords, AIRPORTID = Primary_airports$AIRPORTID)

# Rename the coordinate columns to be more descriptive
colnames(Primary_airports_coords) <- c("longitude", "latitude", "AIRPORTID")

# Path to your airplane PNG file
airplane_icon_path <- "/Users/jiatong/Desktop/30daysMap/day_1_point/icon/211875_plane_icon.png"  # Replace with your actual PNG file path


# Transform spatial data to WGS84 (optional for better accuracy)
CA_boundary <- st_transform(CA_boundary, crs = 4326)
Primary_airports <- st_transform(Primary_airports, crs = 4326)


Primary_airports <- Primary_airports %>%
  mutate(icon = airplane_icon_path)  # Add the icon path as a column


# Plot with scale bar and compass
final_map <- ggplot() +

  
  # Plot California boundaries
  geom_sf(data = CA_boundary, fill = "orange", color = "orange") +

  # Use geom_image() to add the PNG icon for each airport
  geom_image(data = Primary_airports,
             aes(x = st_coordinates(geometry)[, 1], 
                 y = st_coordinates(geometry)[, 2], 
                 image = icon), 
             size = 0.05) +  # Adjust size as needed
  
  # Add dynamically adjusted labels with geom_text_repel
  geom_text_repel(
    data = Primary_airports,
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = AIRPORTID),
    color = "black", size = 4.5,
    min.segment.length = 0,  # Ensure line is always drawn
    box.padding = 1,       # Space around label
    point.padding = 0.5,     # Space between label and point
    segment.color = "darkgrey"  # Color of the line connecting label to point
  ) +
  
  # Add scale bar with white colors for visibility
  annotation_scale(location = "bl", 
                   style = "bar",              # Use "bar" style for the scale bar
                   bar_cols = c("white", "orange"),  # Change colors to white and grey
                   text_col = "black") +      # Change text color to white
  
  # Add compass with white styling
  annotation_north_arrow(location = "tr", 
                         style = north_arrow_fancy_orienteering(text_size = 5, 
                                                                fill = c("orange", "grey"))) +  # Set north arrow colors to white and grey
  
  # Title and theme
  labs(
    title = "Day 1 - points : California Airports",
    subtitle = "Primary and commercial airports in California",
    caption = "30 day map challenge | Day1 - Point\nAuthor: Jiatong Su\nData Source: California State Geoportal"
  ) +
  
  theme_void() +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(color = "black", size = 28),
    plot.subtitle = element_text(color = "black", size = 20),
    plot.caption = element_text(color = "black", size = 10, hjust = 0))

final_map

ggsave("final_map_day_1.png", plot = final_map)
