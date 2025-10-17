# Simple version - minimal dependencies
library(ggplot2)
library(maps)

# Get world data
world_data <- map_data("world")

# Countries to highlight
highlight_countries <- c("Brazil", "Nepal", "Indonesia")

# Create simple map
simple_map <- ggplot() +
  geom_polygon(data = world_data, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white") +
  geom_polygon(data = subset(world_data, region %in% highlight_countries),
               aes(x = long, y = lat, group = group),
               fill = "steelblue", color = "white") +
  theme_void() +
  labs(title = "Priority Research Regions") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(simple_map)
ggsave("figures/map_simple_priority.png", simple_map, width = 8, height = 5, dpi = 300)