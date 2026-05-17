# Load packages
library(nflfastR)
library(nflreadr)
library(readr)
library(tidyr)
library(dplyr)

years <- 1999:2025

# Define the cleaning function (PASTE THIS AT THE TOP, before you use it!)
clean_for_mysql <- function(df) {
  # Convert all date/datetime columns to character (YYYY-MM-DD for dates)
  df[] <- lapply(df, function(x) {
    if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
    if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(format(x, "%Y-%m-%d %H:%M:%S"))
    x
  })
  # Flatten any list-type columns (convert lists to comma-separated strings)
  df[] <- lapply(df, function(x) {
    if (is.list(x)) sapply(x, function(y) paste(y, collapse = ",")) else x
  })
  # Replace NaN (numeric) and "NaN" (string) with NA
  df[] <- lapply(df, function(x) {
    if (is.factor(x)) x <- as.character(x)
    if (is.numeric(x)) x[is.nan(x)] <- NA
    x[x == "NaN"] <- NA
    return(x)
  })
  # Convert everything to character for export (prevents Date conversion error)
  df[] <- lapply(df, as.character)
  return(df)
}

# Export function
write_null_csv <- function(df, filename) {
  write.table(df, file = filename, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE, na = "NULL")
}

# Download data (after defining the functions above)
all_nfl_playerstats <- load_player_stats(seasons = years)
all_nfl_games <- load_schedules(seasons = years)
all_nfl_rosters <- load_rosters(seasons = years)

# Clean and export all datasets (AFTER both functions are defined)
write_null_csv(clean_for_mysql(all_nfl_playerstats), "all_nfl_playerstats.csv")
write_null_csv(clean_for_mysql(all_nfl_games), "all_nfl_games.csv")
write_null_csv(clean_for_mysql(all_nfl_rosters), "all_nfl_rosters.csv")
