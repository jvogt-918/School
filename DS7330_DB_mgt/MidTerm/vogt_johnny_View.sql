create view rra as
select
	tournaments.TourneyID,
    tournaments.TourneyDate,
    tournaments.TourneyLocation,
    count(games.GameID) as totalgamesplayed
from
	tournaments
    join games on tournaments.TourneyID = games.tournaments_TourneyID
    join courts on games.Courts_CourtID = courts.CourtID
where
	courts.CourtLocation = 'Red Rooster Arena'
group by
	tournaments.TourneyID, tournaments.TourneyDate, tournaments.TourneyLocation;