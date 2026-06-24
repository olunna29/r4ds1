# Load required packages
library(ggplot2)
library(maps)

# Read the music dataset
music <- read.csv("data/music.csv", stringsAsFactors = FALSE)

# Convert coordinate and hotness columns to numeric
music$artist.latitude <- as.numeric(as.character(music$artist.latitude))
music$artist.longitude <- as.numeric(as.character(music$artist.longitude))
music$artist.hotttnesss <- as.numeric(as.character(music$artist.hotttnesss))

# Filter to usable coordinates and hotness values greater than 0
usable_artists <- subset(music,
                         !(artist.latitude == 0 & artist.longitude == 0) &
                         !is.na(artist.latitude) & !is.na(artist.longitude) &
                         !is.na(artist.hotttnesss) & artist.hotttnesss > 0)

# Load world map data from maps package
world_map <- map_data("world")

# Build the plot
gg <- ggplot() +
  geom_polygon(
    data = world_map,
    aes(x = long, y = lat, group = group),
    fill = "gray95",
    color = "gray70",
    linewidth = 0.2
  ) +
  geom_point(
    data = usable_artists,
    aes(x = artist.longitude, y = artist.latitude, color = artist.hotttnesss),
    alpha = 0.75,
    size = 1.8
  ) +
  scale_color_viridis_c(option = "plasma", name = "Hotttnesss") +
  coord_fixed(1.3, xlim = c(-180, 180), ylim = c(-60, 85)) +
  labs(
    title = "Artist Locations Colored by Hotttnesss",
    subtitle = "Shows artists with usable coordinates and hotttnesss above 0; coverage is limited to geocoded artists.",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#e6f0f6"),
    panel.grid = element_line(color = "gray90"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = "right"
  )

# Save plot to file
ggsave("artist_world_map.png", gg, width = 12, height = 6, dpi = 300)
message("Saved world map to artist_world_map.png")
