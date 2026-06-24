# Read the music CSV file
music <- read.csv("data/music.csv", stringsAsFactors = FALSE)

# Convert song.year to numeric and filter out 0 values
music$song.year <- as.numeric(as.character(music$song.year))
music_filtered <- subset(music, song.year != 0 & !is.na(song.year))

# Count songs used in the distribution
song_count <- nrow(music_filtered)

# Create the distribution plot
library(ggplot2)
plot_year <- ggplot(music_filtered, aes(x = song.year)) +
  geom_histogram(binwidth = 5, fill = "#2c7fb8", color = "white", alpha = 0.85) +
  labs(
    title = "Distribution of Song Year",
    subtitle = paste0("Number of songs used: ", song_count),
    x = "Song Year",
    y = "Count"
  ) +
  theme_minimal()

# Display the plot when interactive, otherwise save it to a PNG file
if (interactive()) {
  print(plot_year)
} else {
  ggsave("song_year_distribution.png", plot_year, width = 10, height = 6)
  message("Plot saved to song_year_distribution.png")
}
