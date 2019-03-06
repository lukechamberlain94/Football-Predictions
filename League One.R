### League One

setwd("~/League One Data")

LeagueOne <- read.csv(file = "League One.csv")
summary(LeagueOne$team_name)

## Home Model
HomeModel <- lm(points_per_game ~ wins_home + goals_scored_home + clean_sheets_home + shots_home
                + goal_difference_home + goal_difference_home + average_possession_home + 
                  average_total_goals_per_match_home + goals_scored_per_match_home +
                  goals_conceded_per_match_home + shots_on_target_home + win_percentage_home +
                  clean_sheet_percentage_home + draws_home + losses_home + leading_at_half_time_home, data = LeagueOne)

## AWay Model
AwayModel <- lm(points_per_game ~ wins_away + goals_scored_away + clean_sheets_away + shots_away +
                  goal_difference_away + goal_difference_away + average_possession_away + 
                  average_total_goals_per_match_away + goals_scored_per_match_away + goals_conceded_per_match_away +
                  shots_on_target_away + win_percentage_away + clean_sheet_percentage_away + draws_away + losses_away +
                  leading_at_half_time_away, data = LeagueOne)


### Predicted Goals Model Home
GoalsHome <- lm(goals_scored_per_match_home ~ total_goal_count_home + goals_scored_home +
                  average_possession_home + shots_home + shots_on_target_home + shots_off_target +
                  average_total_goals_per_match_home, data = LeagueOne)


## Predicted Goals Model Away
GoalsAway <- lm(goals_scored_per_match_away ~ total_goal_count_away + goals_scored_away +
                  average_possession_away + shots_away + shots_on_target_away + shots_off_target +
                  average_total_goals_per_match_away, data = LeagueOne)

#######################################################

## Doncaster
Doncaster <- LeagueOne[which(LeagueOne$common_name == "Doncaster Rovers"),]

A1 <- predict(HomeModel, Doncaster)
A2 <- predict(AwayModel, Doncaster)

B1 <- predict(GoalsHome, Doncaster)
B2 <- predict(GoalsAway, Doncaster)

C <- Doncaster$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Scunthorpe
Scunthorpe <- LeagueOne[which(LeagueOne$common_name == "Scunthorpe United"),]

A1 <- predict(HomeModel, Scunthorpe)
A2 <- predict(AwayModel, Scunthorpe)

B1 <- predict(GoalsHome, Scunthorpe)
B2 <- predict(GoalsAway, Scunthorpe)

C <- Scunthorpe$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Barnsley
Barnsley <- LeagueOne[which(LeagueOne$common_name == "Barnsley"),]

A1 <- predict(HomeModel, Barnsley)
A2 <- predict(AwayModel, Barnsley)

B1 <- predict(GoalsHome, Barnsley)
B2 <- predict(GoalsAway, Barnsley)

C <- Barnsley$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Portsmouth
Portsmouth <- LeagueOne[which(LeagueOne$common_name == "Portsmouth"),]

A1 <- predict(HomeModel, Portsmouth)
A2 <- predict(AwayModel, Portsmouth)

B1 <- predict(GoalsHome, Portsmouth)
B2 <- predict(GoalsAway, Portsmouth)

C <- Portsmouth$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3



## Bradford
Bradford <- LeagueOne[which(LeagueOne$common_name == "Bradford City"),]

A1 <- predict(HomeModel, Bradford)
A2 <- predict(AwayModel, Bradford)

B1 <- predict(GoalsHome, Bradford)
B2 <- predict(GoalsAway, Bradford)

C <- Bradford$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Walsall
Walsall <- LeagueOne[which(LeagueOne$common_name == "Walsall"),]

A1 <- predict(HomeModel, Walsall)
A2 <- predict(AwayModel, Walsall)

B1 <- predict(GoalsHome, Walsall)
B2 <- predict(GoalsAway, Walsall)

C <- Walsall$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Charlton Athletic 
Charlton <- LeagueOne[which(LeagueOne$common_name == "Charlton Athletic"),]

A1 <- predict(HomeModel, Charlton)
A2 <- predict(AwayModel, Charlton)

B1 <- predict(GoalsHome, Charlton)
B2 <- predict(GoalsAway, Charlton)

C <- Charlton$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Wimbledon
Wimbledon <- LeagueOne[which(LeagueOne$common_name == "AFC Wimbledon"),]

A1 <- predict(HomeModel, Wimbledon)
A2 <- predict(AwayModel, Wimbledon)

B1 <- predict(GoalsHome, Wimbledon)
B2 <- predict(GoalsAway, Wimbledon)

C <- Wimbledon$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Coventry City
Coventry <- LeagueOne[which(LeagueOne$common_name == "Coventry City"),]

A1 <- predict(HomeModel, Coventry)
A2 <- predict(AwayModel, Coventry)

B1 <- predict(GoalsHome, Coventry)
B2 <- predict(GoalsAway, Coventry)

C <- Coventry$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Luton 
Luton <- LeagueOne[which(LeagueOne$common_name == "Luton Town"),]

A1 <- predict(HomeModel, Luton)
A2 <- predict(AwayModel, Luton)

B1 <- predict(GoalsHome, Luton)
B2 <- predict(GoalsAway, Luton)

C <- Luton$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Fleetwood
Fleetwood <- LeagueOne[which(LeagueOne$common_name == "Fleetwood Town"),]

A1 <- predict(HomeModel, Fleetwood)
A2 <- predict(AwayModel, Fleetwood)

B1 <- predict(GoalsHome, Fleetwood)
B2 <- predict(GoalsAway, Fleetwood)

C <- Fleetwood$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Burton
Burton <- LeagueOne[which(LeagueOne$common_name == "Burton Albion"),]

A1 <- predict(HomeModel, Burton)
A2 <- predict(AwayModel, Burton)

B1 <- predict(GoalsHome, Burton)
B2 <- predict(GoalsAway, Burton)

C <- Burton$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Gillingham
Gillingham <- LeagueOne[which(LeagueOne$common_name == "Gillingham"),]

A1 <- predict(HomeModel, Gillingham)
A2 <- predict(AwayModel, Gillingham)

B1 <- predict(GoalsHome, Gillingham)
B2 <- predict(GoalsAway, Gillingham)

C <- Gillingham$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Wycombe
Wycombe <- LeagueOne[which(LeagueOne$common_name == "Wycombe Wanderers"),]

A1 <- predict(HomeModel, Wycombe)
A2 <- predict(AwayModel, Wycombe)

B1 <- predict(GoalsHome, Wycombe)
B2 <- predict(GoalsAway, Wycombe)

C <- Wycombe$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Oxford
Oxford <- LeagueOne[which(LeagueOne$common_name == "Oxford United"),]

A1 <- predict(HomeModel, Oxford)
A2 <- predict(AwayModel, Oxford)

B1 <- predict(GoalsHome, Oxford)
B2 <- predict(GoalsAway, Oxford)

C <- Oxford$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Blackpool
Blackpool <- LeagueOne[which(LeagueOne$common_name == "Blackpool"),]

A1 <- predict(HomeModel, Blackpool)
A2 <- predict(AwayModel, Blackpool)

B1 <- predict(GoalsHome, Blackpool)
B2 <- predict(GoalsAway, Blackpool)

C <- Blackpool$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Rochdale
Rochdale <- LeagueOne[which(LeagueOne$common_name == "Rochdale"),]

A1 <- predict(HomeModel, Rochdale)
A2 <- predict(AwayModel, Rochdale)

B1 <- predict(GoalsHome, Rochdale)
B2 <- predict(GoalsAway, Rochdale)

C <- Rochdale$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Plymouth 
Plymouth <- LeagueOne[which(LeagueOne$common_name == "Plymouth Argyle"),]

A1 <- predict(HomeModel, Plymouth)
A2 <- predict(AwayModel, Plymouth)

B1 <- predict(GoalsHome, Plymouth)
B2 <- predict(GoalsAway, Plymouth)

C <- Plymouth$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Shrewsbury
Shrewsbury <- LeagueOne[which(LeagueOne$common_name == "Shrewsbury Town"),]

A1 <- predict(HomeModel, Shrewsbury)
A2 <- predict(AwayModel, Shrewsbury)

B1 <- predict(GoalsHome, Shrewsbury)
B2 <- predict(GoalsAway, Shrewsbury)

C <- Shrewsbury$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Peterborough
Peterborough <- LeagueOne[which(LeagueOne$common_name == "Peterborough United"),]

A1 <- predict(HomeModel, Peterborough)
A2 <- predict(AwayModel, Peterborough)

B1 <- predict(GoalsHome, Peterborough)
B2 <- predict(GoalsAway, Peterborough)

C <- Peterborough$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Southend
Southend <- LeagueOne[which(LeagueOne$common_name == "Southend United"),]

A1 <- predict(HomeModel, Southend )
A2 <- predict(AwayModel, Southend )

B1 <- predict(GoalsHome, Southend )
B2 <- predict(GoalsAway, Southend )

C <- Southend $points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Accrington
Accrington <- LeagueOne[which(LeagueOne$common_name == "Accrington Stanley"),]

A1 <- predict(HomeModel, Accrington)
A2 <- predict(AwayModel, Accrington)

B1 <- predict(GoalsHome, Accrington)
B2 <- predict(GoalsAway, Accrington)

C <- Accrington$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Sunderland
Sunderland <- LeagueOne[which(LeagueOne$common_name == "Sunderland"),]

A1 <- predict(HomeModel, Sunderland)
A2 <- predict(AwayModel, Sunderland)

B1 <- predict(GoalsHome, Sunderland)
B2 <- predict(GoalsAway, Sunderland)

C <- Sunderland$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Bristol Rovers
BRovers <- LeagueOne[which(LeagueOne$common_name == "Bristol Rovers"),]

A1 <- predict(HomeModel, BRovers)
A2 <- predict(AwayModel, BRovers)

B1 <- predict(GoalsHome, BRovers)
B2 <- predict(GoalsAway, BRovers)

C <- BRovers$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

