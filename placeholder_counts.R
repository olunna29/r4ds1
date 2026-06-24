# Read the music CSV file
music <- read.csv("data/music.csv", stringsAsFactors = FALSE)

# Define the target columns
cols <- c(
  "artist.location",
  "release.name",
  "song.title",
  "song.year",
  "artist.familiarity",
  "artist.hotttnesss"
)

# Helper for identifying placeholders
is_placeholder <- function(x, numeric_zero = FALSE) {
  if (numeric_zero) {
    x_num <- suppressWarnings(as.numeric(as.character(x)))
    return(is.na(x_num) | x_num == 0)
  }
  x_char <- trimws(as.character(x))
  return(is.na(x_char) | x_char == "")
}

# Compute placeholder counts for each column
placeholder_counts <- sapply(cols, function(col_name) {
  col <- music[[col_name]]
  numeric_cols <- c("song.year", "artist.familiarity", "artist.hotttnesss")
  sum(is_placeholder(col, numeric_zero = col_name %in% numeric_cols))
})

# Present the results
placeholder_summary <- data.frame(
  column = cols,
  placeholder_count = placeholder_counts,
  stringsAsFactors = FALSE
)

print(placeholder_summary)
