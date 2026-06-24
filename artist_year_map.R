# Load required packages
library(ggplot2)
library(maps)

# Read the music dataset
music <- read.csv("data/music.csv", stringsAsFactors = FALSE)

# Convert song.year and coordinate columns to numeric
music$song.year <- as.numeric(as.character(music$song.year))
music$artist.latitude <- as.numeric(as.character(music$artist.latitude))
music$artist.longitude <- as.numeric(as.character(music$artist.longitude))

# Filter to artists with usable coordinates and nonzero song.year
year_artists <- subset(music,
                       song.year != 0 & !is.na(song.year) &
                       !(artist.latitude == 0 & artist.longitude == 0) &
                       !is.na(artist.latitude) & !is.na(artist.longitude))

# Load world map data
world_map <- map_data("world")

# Build the plot
gg <- ggplot() +
  geom_polygon(
    data = world_map,
    aes(x = long, y = lat, group = group),
    fill = "gray95",
    color = "gray70",
    size = 0.2
  ) +
  geom_point(
    data = year_artists,
    aes(x = artist.longitude, y = artist.latitude, color = song.year),
    alpha = 0.75,
    size = 1.8
  ) +
  scale_color_viridis_c(option = "plasma", name = "Song Year") +
  coord_fixed(1.3, xlim = c(-180, 180), ylim = c(-60, 85)) +
  labs(
    title = "Artist Locations Colored by Song Year",
    subtitle = "Shows artists with usable coordinates and nonzero song year; geographic coverage is limited to available geocoded artists.",
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

# Save the plot to a PNG file
ggsave("artist_year_map.png", gg, width = 12, height = 6, dpi = 300)
message("Saved year map to artist_year_map.png")
