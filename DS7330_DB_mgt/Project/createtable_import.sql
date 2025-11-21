LOAD DATA INFILE 'C:\\ProgramData\\MySQL\\MySQL Server 9.4\\Uploads\\all_player_stats.csv'
INTO TABLE all_player_stats_college
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 LINES;


CREATE TABLE recruiting_players (
  id INT,
  athlete_id INT,
  recruit_type VARCHAR(40),
  year INT,
  ranking INT,
  name VARCHAR(100),
  school VARCHAR(100),
  committed_to VARCHAR(100),
  position VARCHAR(10),
  height INT,
  weight INT,
  stars INT,
  rating FLOAT,
  city VARCHAR(50),
  state_province VARCHAR(10),
  country VARCHAR(20),
  hometown_info_latitude DOUBLE,
  hometown_info_longitude DOUBLE,
  hometown_info_fips_code INT,
  PRIMARY KEY (id)
);

LOAD DATA INFILE 'C:\\ProgramData\\MySQL\\MySQL Server 9.4\\Uploads\\all_recruiting.csv'
INTO TABLE recruiting_players
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 LINES;

CREATE TABLE season_rankings (
  season INT,
  season_type VARCHAR(20),
  week INT,
  poll VARCHAR(40),
  ranking INT,
  teamId INT,
  school VARCHAR(100),
  conference VARCHAR(50),
  first_place_votes INT,
  points INT
);

LOAD DATA INFILE 'C:\\ProgramData\\MySQL\\MySQL Server 9.4\\Uploads\\all_rankings.csv'
INTO TABLE season_rankings
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 LINES;

CREATE TABLE team_season_stats (
  season INT,
  team VARCHAR(100),
  conference VARCHAR(50),
  games INT,
  time_of_poss_total FLOAT,
  pass_comps INT,
  pass_atts INT,
  net_pass_yds INT,
  pass_TDs INT,
  interceptions INT,
  rush_atts INT,
  rush_yds INT,
  rush_TDs INT,
  total_yds INT,
  fumbles_lost INT,
  turnovers INT,
  first_downs INT,
  third_downs INT,
  third_down_convs INT,
  fourth_down_convs INT,
  fourth_downs INT,
  penalties INT,
  penalty_yds INT,
  kick_returns INT,
  kick_return_yds INT,
  kick_return_TDs INT,
  punt_returns INT,
  punt_return_yds INT,
  punt_return_TDs INT,
  passes_intercepted INT,
  passes_intercepted_yds INT,
  passes_intercepted_TDs INT
);

LOAD DATA INFILE 'C:\\ProgramData\\MySQL\\MySQL Server 9.4\\Uploads\\all_team_stats.csv'
INTO TABLE team_season_stats
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 LINES;

CREATE TABLE team_roster (
  athlete_id INT,
  first_name VARCHAR(50),
  last_name VARCHAR(50),
  team VARCHAR(100),
  weight INT,
  height INT,
  jersey INT,
  year INT,
  position VARCHAR(20),
  home_city VARCHAR(100),
  home_state VARCHAR(20),
  home_country VARCHAR(50),
  home_latitude DOUBLE,
  home_longitude DOUBLE,
  home_county_fips INT,
  recruit_ids VARCHAR(255),
  headshot_url VARCHAR(255)
);

ALTER TABLE team_roster
MODIFY COLUMN athlete_id BIGINT;

LOAD DATA INFILE 'C:\\ProgramData\\MySQL\\MySQL Server 9.4\\Uploads\\all_rosters.csv'
INTO TABLE team_roster
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 LINES;

CREATE TABLE nfl_player_stats (
  player_id varchar(20),
  player_name VARCHAR(100),
  player_display_name VARCHAR(100),
  position VARCHAR(20),
  position_group VARCHAR(20),
  headshot_url VARCHAR(255),
  season INT,
  week INT,
  season_type VARCHAR(10),
  team VARCHAR(10),
  opponent_team VARCHAR(10),
  completions INT,
  attempts INT,
  passing_yards INT,
  passing_tds INT,
  passing_interceptions INT,
  sacks_suffered INT,
  sack_yards_lost INT,
  sack_fumbles INT,
  sack_fumbles_lost INT,
  passing_air_yards INT,
  passing_yards_after_catch INT,
  passing_first_downs INT,
  passing_epa FLOAT,
  passing_cpoe FLOAT,
  passing_2pt_conversions INT,
  pacr FLOAT,
  carries INT,
  rushing_yards INT,
  rushing_tds INT,
  rushing_fumbles INT,
  rushing_fumbles_lost INT,
  rushing_first_downs INT,
  rushing_epa FLOAT,
  rushing_2pt_conversions INT,
  receptions INT,
  targets INT,
  receiving_yards INT,
  receiving_tds INT,
  receiving_fumbles INT,
  receiving_fumbles_lost INT,
  receiving_air_yards INT,
  receiving_yards_after_catch INT,
  receiving_first_downs INT,
  receiving_epa FLOAT,
  receiving_2pt_conversions INT,
  racr FLOAT,
  target_share FLOAT,
  air_yards_share FLOAT,
  wopr FLOAT,
  special_teams_tds INT,
  def_tackles_solo INT,
  def_tackles_with_assist INT,
  def_tackle_assists INT,
  def_tackles_for_loss INT,
  def_tackles_for_loss_yards INT,
  def_fumbles_forced INT,
  def_sacks INT,
  def_sack_yards INT,
  def_qb_hits INT,
  def_interceptions INT,
  def_interception_yards INT,
  def_pass_defended INT,
  def_tds INT,
  def_fumbles INT,
  def_safeties INT,
  misc_yards INT,
  fumble_recovery_own INT,
  fumble_recovery_yards_own INT,
  fumble_recovery_opp INT,
  fumble_recovery_yards_opp INT,
  fumble_recovery_tds INT,
  penalties INT,
  penalty_yards INT,
  punt_returns INT,
  punt_return_yards INT,
  kickoff_returns INT,
  kickoff_return_yards INT,
  fg_made INT,
  fg_att INT,
  fg_missed INT,
  fg_blocked INT,
  fg_long INT,
  fg_pct FLOAT,
  fg_made_0_19 INT,
  fg_made_20_29 INT,
  fg_made_30_39 INT,
  fg_made_40_49 INT,
  fg_made_50_59 INT,
  fg_made_60_ INT,
  fg_missed_0_19 INT,
  fg_missed_20_29 INT,
  fg_missed_30_39 INT,
  fg_missed_40_49 INT,
  fg_missed_50_59 INT,
  fg_missed_60_ INT,
  fg_made_list VARCHAR(255),
  fg_missed_list VARCHAR(255),
  fg_blocked_list VARCHAR(255),
  fg_made_distance VARCHAR(255),
  fg_missed_distance VARCHAR(255),
  fg_blocked_distance VARCHAR(255),
  pat_made INT,
  pat_att INT,
  pat_missed INT,
  pat_blocked INT,
  pat_pct FLOAT,
  gwfg_made INT,
  gwfg_att INT,
  gwfg_missed INT,
  gwfg_blocked INT,
  gwfg_distance INT,
  fantasy_points FLOAT,
  fantasy_points_ppr FLOAT
);

ALTER TABLE nfl_player_stats
modify player_id varchar(20);

drop table nfl_player_stats;

LOAD DATA INFILE 'C:\\ProgramData\\MySQL\\MySQL Server 9.4\\Uploads\\all_nfl_playerstats.csv'
INTO TABLE nfl_player_stats
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 LINES;

CREATE TABLE nfl_roster (
  season INT,
  team VARCHAR(10),
  position VARCHAR(10),
  depth_chart_position VARCHAR(10),
  jersey_number INT,
  status VARCHAR(10),
  full_name VARCHAR(80),
  first_name VARCHAR(40),
  last_name VARCHAR(40),
  birth_date DATE,
  height INT,
  weight INT,
  college VARCHAR(80),
  gsis_id VARCHAR(20),
  espn_id VARCHAR(20),
  sportradar_id VARCHAR(30),
  yahoo_id VARCHAR(20),
  rotowire_id VARCHAR(20),
  pff_id VARCHAR(20),
  pfr_id VARCHAR(20),
  fantasy_data_id VARCHAR(20),
  sleeper_id VARCHAR(20),
  years_exp INT,
  headshot_url VARCHAR(255),
  esb_id VARCHAR(20),
  gsis_it_id VARCHAR(30),
  smart_id VARCHAR(40),
  entry_year INT,
  rookie_year INT,
  draft_club VARCHAR(10),
  ngs_position VARCHAR(10),
  week INT,
  game_type VARCHAR(10),
  status_description_abbr VARCHAR(20),
  football_name VARCHAR(80),
  draft_number INT
);

ALTER TABLE nfl_roster
MODIFY COLUMN college VARCHAR(150);

LOAD DATA INFILE 'C:\\ProgramData\\MySQL\\MySQL Server 9.4\\Uploads\\all_nfl_rosters.csv'
INTO TABLE nfl_roster
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 LINES;

CREATE TABLE nfl_games (
  game_id VARCHAR(20),
  season INT,
  game_type VARCHAR(5),
  week INT,
  gameday DATE,
  weekday VARCHAR(15),
  gametime VARCHAR(10),
  away_team VARCHAR(10),
  away_score INT,
  home_team VARCHAR(10),
  home_score INT,
  location VARCHAR(50),
  result VARCHAR(10),
  total INT,
  overtime TINYINT,
  old_game_id VARCHAR(20),
  gsis VARCHAR(20),
  nfl_detail_id VARCHAR(30),
  pfr VARCHAR(30),
  pff VARCHAR(20),
  espn VARCHAR(20),
  ftn VARCHAR(20),
  away_rest INT,
  home_rest INT,
  away_moneyline INT,
  home_moneyline INT,
  spread_line FLOAT,
  away_spread_odds INT,
  home_spread_odds INT,
  total_line FLOAT,
  under_odds INT,
  over_odds INT,
  div_game TINYINT,
  roof VARCHAR(20),
  surface VARCHAR(20),
  temp INT,
  wind INT,
  away_qb_id VARCHAR(20),
  home_qb_id VARCHAR(20),
  away_qb_name VARCHAR(40),
  home_qb_name VARCHAR(40),
  away_coach VARCHAR(40),
  home_coach VARCHAR(40),
  referee VARCHAR(40),
  stadium_id VARCHAR(10),
  stadium VARCHAR(50)
);

ALTER TABLE nfl_games
MODIFY COLUMN nfl_detail_id VARCHAR(50);

LOAD DATA INFILE 'C:\\ProgramData\\MySQL\\MySQL Server 9.4\\Uploads\\all_nfl_games.csv'
INTO TABLE nfl_games
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 LINES;
