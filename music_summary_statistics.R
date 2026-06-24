# Read the music CSV file
music <- read.csv("data/music.csv", stringsAsFactors = FALSE)

# Select the requested numeric columns
cols <- c("artist.familiarity", "artist.hotttnesss", "song.year", "song.tempo")
selected <- music[cols]

# Convert all values to numeric and compute summary statistics
to_numeric <- function(x) {
  as.numeric(as.character(x))
}

summary_stats <- t(sapply(selected, function(x) {
  x_num <- to_numeric(x)
  x_num <- x_num[!is.na(x_num)]
  c(
    min = min(x_num),
    q1 = quantile(x_num, 0.25),
    median = median(x_num),
    q3 = quantile(x_num, 0.75),
    max = max(x_num)
  )
}))

# Print results in a readable format
print(summary_stats)
