#WebScraping for Database Project
#API Key: HbhaAKKmMChZn9memm6VL8bmJD8ytJOWZhmEFFIWYuTtkOUtso2dX6ugSpppvPId
library(httr)
library(jsonlite)
library(cfbfastR)
library(dplyr)

years <- 1990:2025

##############################Draft Picks ###################################
all_drafts <- lapply(years, function(y) {
  # Pause is nice to not overload the server (polite practice!)
  Sys.sleep(0.5)
  message("Getting draft picks for ", y)
  cfbd_draft_picks(year = y)
})

# Combine all years into a single data frame
all_drafts_df <- bind_rows(all_drafts)

# Preview your result!
print(head(all_drafts_df))
all_drafts_df_char <- all_drafts_df %>%
  mutate(across(everything(), as.character))

all_drafts_df_char[is.na(all_drafts_df_char)] <- ""
write.csv(all_drafts_df_char, "all_draft.csv", row.names = FALSE)
#########################################################################

#############################Player Season Stats #########################
all_player_stats <- lapply(years, function(y) {
  Sys.sleep(0.5)  # Polite pause for the API
  message("Getting player season stats for ", y)
  cfbd_stats_season_player(year = y)
})

# Combine all years into a single data frame
all_player_stats_df <- bind_rows(all_player_stats)

# Preview your data
print(head(all_player_stats_df))
all_player_stats_df_char <- all_player_stats_df %>%
  mutate(across(everything(), as.character))

all_player_stats_df_char[is.na(all_player_stats_df_char)] <- ""
write.csv(all_player_stats_df_char, "all_player_stats.csv", row.names = FALSE)
##########################################################################

#############################Team Season Stats ###########################
all_team_stats <- lapply(years, function(y) {
  Sys.sleep(0.5)  # Polite API pause
  message("Getting team season stats for ", y)
  cfbd_stats_season_team(year = y)
})

all_team_stats_df <- bind_rows(all_team_stats)

print(head(all_team_stats_df))
all_team_stats_df_char <- all_team_stats_df %>%
  mutate(across(everything(), as.character))

all_team_stats_df_char[is.na(all_team_stats_df_char)] <- ""
write.csv(all_team_stats_df_char, "all_team_stats.csv", row.names = FALSE)
###########################################################################

#################################### CFB Rankings############################
all_rankings <- lapply(years, function(y) {
  Sys.sleep(0.5)
  message("Getting rankings for ", y)
  cfbd_rankings(year = y)
})
all_rankings_df <- bind_rows(all_rankings)
print(head(all_rankings_df))
all_rankings_df_char <- all_rankings_df %>%
  mutate(across(everything(), as.character))

all_rankings_df_char[is.na(all_rankings_df_char)] <- ""
write.csv(all_rankings_df_char, "all_rankings.csv", row.names = FALSE)
#####################################################################

################################# Recruiting Players#########################
rec_year = 2000:2025
recruit_years <- 2000:2025
all_recruiting <- lapply(recruit_years, function(y) {
  Sys.sleep(0.5)
  message("Getting recruiting for ", y)
  cfbd_recruiting_player(year = y)
})
all_recruiting_df <- bind_rows(all_recruiting)
print(head(all_recruiting_df))

# Convert to character type for export
all_recruiting_df_char <- all_recruiting_df %>%
  mutate(across(everything(), as.character))

all_recruiting_df_char[is.na(all_recruiting_df_char)] <- ""
write.csv(all_recruiting_df_char, "all_recruiting.csv", row.names = FALSE)

#######################################################################

################################ Team Rosters#############################
roster_years <- 2000:2025

all_rosters <- lapply(roster_years, function(y) {
  Sys.sleep(0.5)
  message("Getting team rosters for ", y)
  cfbd_team_roster(year = y)
})

all_rosters_df <- bind_rows(all_rosters)
print(head(all_rosters_df))

# Flatten a list-column if present
if ("recruit_ids" %in% colnames(all_rosters_df)) {
  all_rosters_df$recruit_ids <- sapply(all_rosters_df$recruit_ids, function(x) paste(x, collapse = ","))
}

# Convert all columns to character for easy NA/NULL handling in CSV export
all_rosters_df_char <- all_rosters_df %>%
  mutate(across(everything(), as.character))

# Replace NA with empty string (for MySQL import as NULL)
all_rosters_df_char[is.na(all_rosters_df_char)] <- ""

# Write to CSV
write.csv(all_rosters_df_char, "all_rosters.csv", row.names = FALSE)
######################################################################




