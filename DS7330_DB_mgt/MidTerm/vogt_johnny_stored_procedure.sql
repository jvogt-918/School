use midterm;

DROP PROCEDURE IF EXISTS GetTournamentLocation;
DROP PROCEDURE IF EXISTS allplayers;
DROP PROCEDURE IF EXISTS coachandteam;
DROP PROCEDURE IF EXISTS unplayedtourn;
DROP PROCEDURE IF EXISTS top10;
DROP PROCEDURE IF EXISTS highscore;
DROP PROCEDURE IF EXISTS tenaboveavg;


#Retrieve locations where tournaments are hosted.
DELIMITER //
create procedure GetTournamentLocation()
begin
	select distinct TourneyLocation from tournaments;
end //
DELIMITER ; 

#Display all players with their addresses formatted for mailing, sorted by zipcode.
DELIMITER //
create procedure allplayers()
begin
	select FirstName, LastName, concat(Address, ', ', City, ', ', State, ' ', Zipcode) as MailingAddress
    from players
    order by ZipCode;
end //
DELIMITER ; 

#List teams and their coaches.
DELIMITER //
create procedure coachandteam()
begin
	select teams.TeamName, coaches.CoachFirstName, coaches.CoachLastName
    from teams
    join coaches on teams.Coaches_CoachID = coaches.CoachID;
end //
DELIMITER ; 

#Show tournaments hat have not been played yet
DELIMITER //
create procedure unplayedtourn()
begin
	select tournaments.TourneyID, tournaments.TourneyDate, tournaments.TourneyLocation
    from tournaments
    left join games on tournaments.TourneyID = games.tournaments_TourneyID
    where games.GameID is NULL;
end //
DELIMITER ; 

#display top 10 players with the highest total score
DELIMITER //
create procedure top10()
begin
	select Players_PlayerID, sum(PlayerScore) as TotalScore
    from games_has_players
    group by Players_PlayerID
    order by TotalScore desc
    limit 10;
end //
DELIMITER ; 

#Display highest score for single game
DELIMITER //
create procedure highscore()
begin
	select Players_PlayerID, max(PlayerScore) as MaxScore
    from games_has_players
    group by Players_PlayerID;
end //
DELIMITER ; 

#find  players whose trop score is above 10 points their avg-
DELIMITER //
create procedure tenaboveavg()
begin
	select games_has_players.Players_PlayerID,
    max(games_has_players.PlayerScore) as maxscore,
    avg(games_has_players.PlayerScore) as avgscore
    from games_has_players
    group by games_has_players.Players_PlayerID
    having maxscore > avgscore + 10;
end //
DELIMITER ; 