# Load required libraries
library(ggplot2)
library(dplyr)
library(maps)
library(mapdata)

# Create a world map data frame
world_map <- map_data("world")

# Define the countries to highlight
priority_countries <- c("Brazil", "Nepal", "Indonesia")

# Create the map plot
priority_map <- ggplot() +
  # Plot all countries in light gray
  geom_polygon(data = world_map, 
               aes(x = long, y = lat, group = group),
               fill = "lightgray", color = "white", linewidth = 0.1) +
  
  # Highlight priority countries
  geom_polygon(data = world_map %>% filter(region %in% priority_countries),
               aes(x = long, y = lat, group = group),
               fill = "#2E8B57", color = "white", linewidth = 0.2) +
  
  # Add labels for priority countries
  geom_text(data = data.frame(
    country = priority_countries,
    long = c(-55, 84, 120),
    lat = c(-10, 28, -5)
  ), aes(x = long, y = lat, label = country),
  color = "darkgreen", fontface = "bold", size = 4) +
  
  # Customize the theme
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  
  # Add title and subtitle
  labs(
    title = "Priority Regions for Ethnoforestry Research",
    subtitle = "Brazil, Nepal, and Indonesia"
  ) +
  
  # Set aspect ratio
  coord_fixed(ratio = 1.3)

# Display the plot
print(priority_map)

# Save the plot
ggsave("priority_regions_map.png", 
       plot = priority_map, 
       width = 10, 
       height = 6, 
       dpi = 300)

# Alternative version with different colors and styling
priority_map_alt <- ggplot() +
  geom_polygon(data = world_map, 
               aes(x = long, y = lat, group = group),
               fill = "#f0f0f0", color = "darkgray", linewidth = 0.1) +
  
  geom_polygon(data = world_map %>% filter(region %in% priority_countries),
               aes(x = long, y = lat, group = group),
               fill = "#E74C3C", color = "darkred", linewidth = 0.3, alpha = 0.7) +
  
  geom_point(data = data.frame(
    country = priority_countries,
    long = c(-53, 84, 115),
    lat = c(-15, 28, -5)
  ), aes(x = long, y = lat),
  color = "darkred", size = 3, shape = 18) +
  
  geom_text(data = data.frame(
    country = priority_countries,
    long = c(-53, 84, 115),
    lat = c(-22, 23, -12)
  ), aes(x = long, y = lat, label = country),
  color = "darkred", fontface = "bold", size = 4) +
  
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18, color = "#2C3E50"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, color = "#34495E"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  
  labs(
    title = "Ethnoforestry Research Priority Regions",
    subtitle = "Focus areas for pilot interventions"
  ) +
  
  coord_fixed(ratio = 1.3)

# Display alternative version
print(priority_map_alt)

# Save alternative version
ggsave("figures/map_priority_regions_alt.png", 
       plot = priority_map_alt, 
       width = 10, 
       height = 6, 
       dpi = 300)

