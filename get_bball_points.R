# get_bball_points.R

library(readr)
library(purrr)
library(tibble)
library(dplyr)
library(stringr)

# URL for KenPom data
src <- "https://kenpom.com/cbbga26.txt"
lines <- read_lines(src)

# Function to extract rows
extract_row <- function(x) {
  if (str_length(str_trim(x)) < 25) return(NULL)
  d <- str_sub(x, 1, 10)
  body <- str_trim(str_sub(x, 12))
  z <- str_match(body, "^(.+?)\\s+(\\d+)\\s+(.+?)\\s+(\\d+)")
  if (is.na(z[1,1])) return(NULL)
  tibble(
    date = d,
    away_points = as.numeric(z[1,3]),
    home_points = as.numeric(z[1,5])
  )
}

# Process all lines
games <- map_df(lines, extract_row) %>%
  filter(!is.na(away_points), !is.na(home_points))

# Summarise by date
daily <- games %>%
  group_by(date) %>%
  summarise(
    total_away_points = sum(away_points),
    total_home_points = sum(home_points),
    num_games = n(),
    avg_points_per_game = (total_away_points + total_home_points) / num_games,
    .groups = "drop"
  ) %>%
  mutate(
    total_away_points = as.numeric(total_away_points),
    total_home_points = as.numeric(total_home_points),
    num_games = as.integer(num_games),
    avg_points_per_game = as.numeric(avg_points_per_game)
  )

# Ensure data folder exists
if (!dir.exists("data")) {
  dir.create("data")
}

# Path to CSV
path <- file.path("data", "kenpom_daily_summary.csv")

# Update or create CSV
if (file.exists(path)) {
  old <- read_csv(path, show_col_types = FALSE) %>%
    mutate(
      total_away_points = as.numeric(total_away_points),
      total_home_points = as.numeric(total_home_points),
      num_games = as.integer(num_games),
      avg_points_per_game = as.numeric(avg_points_per_game)
    )
  add <- anti_join(daily, old, by = "date")
  out <- bind_rows(old, add)
} else {
  out <- daily
}

# Write CSV
write_csv(out, path)
