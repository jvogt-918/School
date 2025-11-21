create database if not exists project;
use project;

-- TEAMS Table (for use in all tables requiring a team reference)
CREATE TABLE teams (
    team_id INT PRIMARY KEY AUTO_INCREMENT,
    name VARCHAR(100),
    conference VARCHAR(50),
    school VARCHAR(100)
);

-- PLAYERS Table (core player bio data)
CREATE TABLE players (
    athlete_id INT PRIMARY KEY,
    first_name VARCHAR(50),
    last_name VARCHAR(50),
    home_city VARCHAR(50),
    home_state VARCHAR(50),
    home_country VARCHAR(50),
    home_latitude DECIMAL(10,7),
    home_longitude DECIMAL(10,7),
    home_county_fips VARCHAR(10),
    recruit_ids VARCHAR(100),
    headshot_url VARCHAR(255)
);

-- ROSTERS Table (season-specific player info)
CREATE TABLE rosters (
    roster_id INT PRIMARY KEY AUTO_INCREMENT,
    athlete_id INT,
    team_id INT,
    weight INT,
    height INT,
    jersey VARCHAR(10),
    year INT,
    position VARCHAR(10),
    FOREIGN KEY (athlete_id) REFERENCES players(athlete_id),
    FOREIGN KEY (team_id) REFERENCES teams(team_id)
);

-- PLAYER_STATS Table (per-season stats, one row per player per year per team)
CREATE TABLE player_stats (
    stat_id INT PRIMARY KEY AUTO_INCREMENT,
    year INT,
    team_id INT,
    athlete_id INT,
    position VARCHAR(10),
    -- Add all the statistic columns here as described
    passing_completions INT,
    passing_att INT,
    passing_pct DECIMAL(5,2),
    -- ... (other stats truncated for brevity)
    FOREIGN KEY (team_id) REFERENCES teams(team_id),
    FOREIGN KEY (athlete_id) REFERENCES players(athlete_id)
);

-- RANKINGS Table (season team rankings)
CREATE TABLE rankings (
    ranking_id INT AUTO_INCREMENT PRIMARY KEY,
    season INT,
    season_type VARCHAR(20),
    week INT,
    poll VARCHAR(20),
    `rank` INT,
    team_id INT,
    first_place_votes INT,
    points INT,
    FOREIGN KEY (team_id) REFERENCES teams(team_id)
);

-- NFL DRAFTS Table (draft history for players)
CREATE TABLE drafts (
    draft_id INT PRIMARY KEY AUTO_INCREMENT,
    college_athlete_id INT,
    nfl_athlete_id INT,
    college_id INT,
    college_team VARCHAR(100),
    college_conference VARCHAR(50),
    nfl_team_id INT,
    nfl_team VARCHAR(100),
    year INT,
    overall INT,
    round INT,
    pick INT,
    name VARCHAR(100),
    position VARCHAR(10),
    height INT,
    weight INT,
    pre_draft_ranking INT,
    pre_draft_position_ranking INT,
    pre_draft_grade DECIMAL(4,2),
    hometown_info_city VARCHAR(50),
    hometown_info_state VARCHAR(50),
    hometown_info_country VARCHAR(50),
    hometown_info_latitude DECIMAL(10,7),
    hometown_info_longitude DECIMAL(10,7),
    hometown_info_county_fips VARCHAR(10),
    FOREIGN KEY (college_athlete_id) REFERENCES players(athlete_id)
    -- Add other FKs here as desired
);
