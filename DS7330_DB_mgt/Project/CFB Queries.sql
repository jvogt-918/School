#QB College
use project;

SELECT
    player,
    year,
    SUM(passing_yds)      AS total_college_passing_yards,
    SUM(passing_td)       AS total_college_passing_tds,
    SUM(rushing_yds)      AS total_college_rushing_yards,
    SUM(rushing_td)       AS total_college_rushing_tds,
    SUM(passing_completions) AS total_college_completions,
    SUM(passing_att)      AS total_college_attempts
FROM
    all_player_stats_college
WHERE
    position = 'QB'
GROUP BY
    player, year
ORDER BY
    player, year;


#NFL QB Passing Yards 
SELECT
    n.player_name,
    n.season,
    d.college_team,
    SUM(n.passing_yards)   AS total_nfl_passing_yards,
    SUM(n.passing_tds)     AS total_nfl_passing_tds,
    SUM(n.rushing_yards)   AS total_nfl_rushing_yards,
    SUM(n.rushing_tds)     AS total_nfl_rushing_tds,
    SUM(n.completions)     AS total_nfl_completions,
    SUM(n.attempts)        AS total_nfl_attempts
FROM
    nfl_player_stats n
LEFT JOIN
    all_draft d
    ON n.player_name = d.name    -- Change key as appropriate!
WHERE
    n.position = 'QB'
GROUP BY
    n.player_name, n.season, d.college_team
ORDER BY
    n.player_name, n.season;



#college season stat
SELECT
    player,
    team AS college_team,
    SUM(passing_yds)      AS career_college_passing_yards,
    SUM(passing_td)       AS career_college_passing_tds,
    SUM(rushing_yds)      AS career_college_rushing_yards,
    SUM(rushing_td)       AS career_college_rushing_tds,
    SUM(passing_completions) AS career_college_completions,
    SUM(passing_att)      AS career_college_attempts
FROM
    all_player_stats_college
WHERE
    position = 'QB'
GROUP BY
    player, team
ORDER BY
    player;

#College
SELECT
    player,
    team AS college_team,
    COUNT(DISTINCT year)                          AS college_seasons_played,
    SUM(passing_yds)                              AS career_college_passing_yards,
    ROUND(SUM(passing_yds)/COUNT(DISTINCT year), 1) AS avg_college_passing_yards_per_season,
    SUM(passing_td)                               AS career_college_passing_tds,
    ROUND(SUM(passing_td)/COUNT(DISTINCT year), 2)  AS avg_college_passing_tds_per_season,
    SUM(rushing_yds)                              AS career_college_rushing_yards,
    ROUND(SUM(rushing_yds)/COUNT(DISTINCT year), 1) AS avg_college_rushing_yards_per_season,
    SUM(rushing_td)                               AS career_college_rushing_tds,
    ROUND(SUM(rushing_td)/COUNT(DISTINCT year), 2)  AS avg_college_rushing_tds_per_season,
    SUM(passing_completions)                      AS career_college_completions,
    ROUND(SUM(passing_completions)/COUNT(DISTINCT year), 1) AS avg_college_completions_per_season,
    SUM(passing_att)                              AS career_college_attempts,
    ROUND(SUM(passing_att)/COUNT(DISTINCT year), 1)   AS avg_college_attempts_per_season
FROM
    all_player_stats_college
WHERE
    position = 'QB'
GROUP BY
    player, team
ORDER BY
    player;


#nfl season
SELECT
    n.player_name,
    d.college_team,
    COUNT(DISTINCT n.season)                    AS nfl_seasons_played,
    SUM(n.passing_yards)                        AS career_nfl_passing_yards,
    ROUND(SUM(n.passing_yards)/COUNT(DISTINCT n.season), 1) AS avg_nfl_passing_yards_per_season,
    SUM(n.passing_tds)                          AS career_nfl_passing_tds,
    ROUND(SUM(n.passing_tds)/COUNT(DISTINCT n.season), 2)   AS avg_nfl_passing_tds_per_season,
    SUM(n.rushing_yards)                        AS career_nfl_rushing_yards,
    ROUND(SUM(n.rushing_yards)/COUNT(DISTINCT n.season), 1) AS avg_nfl_rushing_yards_per_season,
    SUM(n.rushing_tds)                          AS career_nfl_rushing_tds,
    ROUND(SUM(n.rushing_tds)/COUNT(DISTINCT n.seasoall_player_stats_collegeall_player_stats_collegen), 2)   AS avg_nfl_rushing_tds_per_season,
    SUM(n.completions)                          AS career_nfl_completions,
    ROUND(SUM(n.completions)/COUNT(DISTINCT n.season), 1)   AS avg_nfl_completions_per_season,
    SUM(n.attempts)                             AS career_nfl_attempts,
    ROUND(SUM(n.attempts)/COUNT(DISTINCT n.season), 1)      AS avg_nfl_attempts_per_season
FROM
    nfl_player_stats n
LEFT JOIN
    all_draft d ON n.player_name = d.name      -- change join key as needed
WHERE
    n.position = 'QB'
GROUP BY
    n.player_name, d.college_team
ORDER BY
    n.player_name;
