select midterm;

insert into teams (TeamID, TeamName, Coaches_CoachID)
values 
(1, 'Dallas Hawks', 1),
(2, 'Austin Bears', 2),
(3, 'Houston Hornets', 3),
(4, 'San Antonio Spurs', 4);

insert into courts (CourtID, CourtLocation)
values
(1, 'Red Rooster Arena'),
(2, 'Dallas Field House'),
(3, 'Austin SPorts Center');

insert into games_has_players (Games_GameID, Players_PlayerID, PlayerScore)
values
(1, 1, 12),
(1, 2, 8),
(1, 3, 10),
(1, 4, 6),
(2, 5, 14),
(2, 6, 7),
(2, 7, 9 ),
(2, 8, 11),
(3, 3, 18),
(3, 4, 12),
(3, 5, 10),
(3, 6, 9),
(4, 1, 16),
(4, 2, 13),
(4, 7, 8),
(4, 8, 10);

insert into tournaments (TourneyID, TourneyDate, TourneyLocation)
values
(1, 2025-01-10, 'Red Rooster Arena'),
(2, 2025-03-14, 'Dallas Field House'),
(3, 2025-05-22, 'Austin Sports Center'),
(4, 2025-08-10, 'Fort Worth Invintational');

insert into coaches (CoachID, CoachFirstName, CoachLastName)
values
(1, 'John', 'Stevens'),
(2, 'Maria', 'Lopez'),
(3, 'Kevin', 'Tuner'),
(4, 'Nina', 'Patel');

 insert into players (PlayerID, FirstName, LastName, Address, City, State, ZipCode, TeamID)
 values
 (1, 'Liam', 'Carter', '100 Elm St', 'Dallas', 'TX', 75001, 1),
 (2, 'Noah', 'Wright', '220 Pine Ave', 'Dallas', 'TX', 75002, 1),
 (3, 'Emma', 'Johnson', '15 Maple Rd', 'Austin', 'TX', 73301, 2),
 (4, 'Olivia', 'Brown', '90 Oak Dr', 'Austin', 'TX', 73302, 2),
 (5, 'Mason', 'Davies', '67 Hilltop Blvd', 'Houston', 'TX', 77001, 3),
 (6, 'Ava', 'Lewis', '43 Creek Ln', 'Houston', 'TX', 77003, 3),
 (7, 'James', 'Wilson', '87 Meadow Ct', 'San Antonio', 'TX', 78201, 4),
 (8, 'Mia', 'Taylor', '125 Sunset Rd', 'San Antonio', 'TX', 78205, 4);
 
 insert into games (GameID, TourneyID, HomeTeamID, AwayTeamID, WinningTeamID, CourtID)
 values
 (1, 1, 1, 2, 1, 1),
 (2, 1, 3, 4, 4, 1),
 (3, 2, 2, 3, 3, 2),
 (4, 3, 1, 4, 1, 3);