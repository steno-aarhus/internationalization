# Load necessary libraries
library(ggplot2)
library(sf)
library(dplyr)

# Define coordinates for Denmark and Latin American countries
denmark_coords <- data.frame(
  country = "Denmark",
  lon = 9.5018,
  lat = 56.2639
)

latin_america_coords <- data.frame(
  country = c("Mexico", "Brazil", "Argentina", "Colombia", "Chile", "Peru", "Cuba", "Costa Rica"),
  lon = c(-102.5528, -51.9253, -63.6167, -74.2973, -71.5429, -75.0152, -77.7812, -83.7534),
  lat = c(23.6345, -14.2350, -38.4161, 4.5709, -35.6751, -9.1899, 21.5218, 9.7489)
)

# Combine Denmark and Latin America coordinates
all_coords <- rbind(denmark_coords, latin_america_coords)

# Create connection lines between Denmark and Latin America
connections <- do.call(rbind, lapply(1:nrow(latin_america_coords), function(i) {
  data.frame(
    from_lon = denmark_coords$lon,
    from_lat = denmark_coords$lat,
    to_lon = latin_america_coords$lon[i],
    to_lat = latin_america_coords$lat[i]
  )
}))

# Load high-resolution world map
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Color-blind friendly palette (Okabe-Ito)
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Plot the map with enhanced quality and accessibility
ggplot(data = world) +
  geom_sf(fill = "lightgrey", color = "white") +  # Base map
  #geom_point(data = all_coords, aes(x = lon, y = lat), color = okabe_ito[2], size = 3) +  # Nodes
  geom_segment(data = connections, aes(x = from_lon, y = from_lat, xend = to_lon, yend = to_lat),
               color = okabe_ito[3], size = 0.6, alpha = 0.8) +  # Connections
  coord_sf(xlim = c(-130, 20), ylim = c(-60, 70), expand = FALSE) +  # Focused extent
  theme_minimal() +
  # labs(title = "Denmark and Latin America Collaboration Network",
  #      x = "Longitude", y = "Latitude") +
  theme(
    panel.background = element_rect(fill = "lightblue"),
    panel.grid.major = element_line(color = "white", size = 0.2),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(file= here::here("images/dk-latam_map.svg"), width = 8, height = 9, device="svg")