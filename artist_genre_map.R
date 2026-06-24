# Load required packages
library(ggplot2)
library(maps)

# Read the music dataset
music <- read.csv("data/music.csv", stringsAsFactors = FALSE)

# Convert coordinate columns to numeric
music$artist.latitude <- as.numeric(as.character(music$artist.latitude))
music$artist.longitude <- as.numeric(as.character(music$artist.longitude))

# Normalize artist terms and assign genres by priority
music$artist.terms <- tolower(trimws(as.character(music$artist.terms)))
assign_genre <- function(terms) {
  if (is.na(terms) || terms == "") {
    return(NA_character_)
  }
  if (grepl("\\brock\\b", terms, ignore.case = TRUE)) {
    return("rock")
  }
  if (grepl("\\bpop\\b", terms, ignore.case = TRUE)) {
    return("pop")
  }
  if (grepl("\\bblues\\b", terms, ignore.case = TRUE)) {
    return("blues")
  }
  if (grepl("\\bjazz\\b", terms, ignore.case = TRUE)) {
    return("jazz")
  }
  if (grepl("\\bmetal\\b", terms, ignore.case = TRUE)) {
    return("metal")
  }
  return(NA_character_)
}

music$genre <- vapply(music$artist.terms, assign_genre, character(1), USE.NAMES = FALSE)

# Filter to artists with usable coordinates and a matching genre
genre_artists <- subset(music,
                        !(artist.latitude == 0 & artist.longitude == 0) &
                        !is.na(artist.latitude) & !is.na(artist.longitude) &
                        !is.na(genre) & genre != "")

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
    data = genre_artists,
    aes(x = artist.longitude, y = artist.latitude, color = genre),
    alpha = 0.75,
    size = 1.8
  ) +
  scale_color_manual(
    values = c(
      rock = "#d73027",
      pop = "#fc8d59",
      blues = "#91bfdb",
      jazz = "#4575b4",
      metal = "#7f3b08"
    ),
    name = "Genre"
  ) +
  coord_fixed(1.3, xlim = c(-180, 180), ylim = c(-60, 85)) +
  labs(
    title = "Artist Locations by Inferred Genre",
    subtitle = "Geographic pattern of artists assigned to rock, pop, blues, jazz, or metal based on artist terms.",
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
ggsave("artist_genre_map.png", gg, width = 12, height = 6, dpi = 300)
message("Saved genre map to artist_genre_map.png")
