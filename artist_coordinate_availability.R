# Read the music CSV file
music <- read.csv("data/music.csv", stringsAsFactors = FALSE)

# Convert coordinate columns to numeric
music$artist.latitude <- as.numeric(as.character(music$artist.latitude))
music$artist.longitude <- as.numeric(as.character(music$artist.longitude))

# Determine whether each artist row has placeholder coordinates (both latitude and longitude equal 0)
music$placeholder_coords <- with(music, artist.latitude == 0 & artist.longitude == 0)

# Count usable and placeholder coordinates
counts <- table(placeholder = music$placeholder_coords)

usable_count <- counts["FALSE"]
placeholder_count <- counts["TRUE"]

# Handle cases where one of the categories may be absent
if (is.na(usable_count)) usable_count <- 0
if (is.na(placeholder_count)) placeholder_count <- 0

# Output the results
result <- data.frame(
  category = c("usable_coordinates", "placeholder_coordinates"),
  count = c(usable_count, placeholder_count),
  stringsAsFactors = FALSE
)

print(result)
