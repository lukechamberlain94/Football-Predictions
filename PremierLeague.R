### Premier League

setwd("~/Premier League Data")

Prem <- read.csv(file = "PremTeams.csv")
summary(Prem$team_name)

## Home Model
HomeModel <- lm(points_per_game ~ wins_home + goals_scored_home + clean_sheets_home + shots_home
                + goal_difference_home + goal_difference_home + average_possession_home + 
                  average_total_goals_per_match_home + goals_scored_per_match_home +
                  goals_conceded_per_match_home + shots_on_target_home + win_percentage_home +
                  clean_sheet_percentage_home + draws_home + losses_home + leading_at_half_time_home, data = Prem)

## AWay Model
AwayModel <- lm(points_per_game ~ wins_away + goals_scored_away + clean_sheets_away + shots_away +
                  goal_difference_away + goal_difference_away + average_possession_away + 
                  average_total_goals_per_match_away + goals_scored_per_match_away + goals_conceded_per_match_away +
                  shots_on_target_away + win_percentage_away + clean_sheet_percentage_away + draws_away + losses_away +
                  leading_at_half_time_away, data = Prem)

### Predicted Goals Model Home
GoalsHome <- lm(goals_scored_per_match_home ~ total_goal_count_home + goals_scored_home +
                  average_possession_home + shots_home + shots_on_target_home + shots_off_target +
                  average_total_goals_per_match_home, data = Prem)


## Predicted Goals Model Away
GoalsAway <- lm(goals_scored_per_match_away ~ total_goal_count_away + goals_scored_away +
                  average_possession_away + shots_away + shots_on_target_away + shots_off_target +
                  average_total_goals_per_match_away, data = Prem)

#######################################################

## Arsenal
Arsenal <- Prem[which(Prem$team_name == "Arsenal FC"),]

A1 <- predict(HomeModel, Arsenal)
A2 <- predict(AwayModel, Arsenal)

B1 <- predict(GoalsHome, Arsenal)
B2 <- predict(GoalsAway, Arsenal)

C <- Arsenal$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Tottenham Hotspur FC
TottenhamHotspurFC <- Prem[which(Prem$team_name == "Tottenham Hotspur FC"),]

A1 <- predict(HomeModel, TottenhamHotspurFC)
A2 <- predict(AwayModel, TottenhamHotspurFC)

B1 <- predict(GoalsHome, TottenhamHotspurFC)
B2 <- predict(GoalsAway, TottenhamHotspurFC)

C <- TottenhamHotspurFC$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## ManCity
ManCity<- Prem[which(Prem$team_name == "Manchester City FC"),]

A1 <- predict(HomeModel, ManCity)
A2 <- predict(AwayModel, ManCity)

B1 <- predict(GoalsHome, ManCity)
B2 <- predict(GoalsAway, ManCity)

C <- ManCity$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Leicester
Leicester<- Prem[which(Prem$team_name == "Leicester City FC"),]

A1 <- predict(HomeModel, Leicester)
A2 <- predict(AwayModel, Leicester)

B1 <- predict(GoalsHome, Leicester)
B2 <- predict(GoalsAway, Leicester)

C <- Leicester$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3



## Palace
Palace <- Prem[which(Prem$team_name == "Crystal Palace FC"),]

A1 <- predict(HomeModel, Palace)
A2 <- predict(AwayModel, Palace)

B1 <- predict(GoalsHome, Palace)
B2 <- predict(GoalsAway, Palace)

C <- Palace$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Everton
Everton <- Prem[which(Prem$team_name == "Everton FC"),]

A1 <- predict(HomeModel, Everton)
A2 <- predict(AwayModel, Everton)

B1 <- predict(GoalsHome, Everton)
B2 <- predict(GoalsAway, Everton)

C <- Everton$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Burnley
Burnley <- Prem[which(Prem$team_name == "Burnley FC"),]

A1 <- predict(HomeModel, Burnley)
A2 <- predict(AwayModel, Burnley)

B1 <- predict(GoalsHome, Burnley)
B2 <- predict(GoalsAway, Burnley)

C <- Burnley$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Southampton
Saints <- Prem[which(Prem$team_name == "Southampton FC"),]

A1 <- predict(HomeModel, Saints)
A2 <- predict(AwayModel, Saints)

B1 <- predict(GoalsHome, Saints)
B2 <- predict(GoalsAway, Saints)

C <- Saints$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Bournemouth
Bournemouth <- Prem[which(Prem$team_name == "AFC Bournemouth"),]

A1 <- predict(HomeModel, Bournemouth)
A2 <- predict(AwayModel, Bournemouth)

B1 <- predict(GoalsHome, Bournemouth)
B2 <- predict(GoalsAway, Bournemouth)

C <- Bournemouth$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## ManUtd
ManUtd <- Prem[which(Prem$team_name == "Manchester United FC"),]

A1 <- predict(HomeModel, ManUtd)
A2 <- predict(AwayModel, ManUtd)

B1 <- predict(GoalsHome, ManUtd)
B2 <- predict(GoalsAway, ManUtd)

C <- ManUtd$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Liverpool
Liverpool <- Prem[which(Prem$team_name == "Liverpool FC"),]

A1 <- predict(HomeModel, Liverpool)
A2 <- predict(AwayModel, Liverpool)

B1 <- predict(GoalsHome, Liverpool)
B2 <- predict(GoalsAway, Liverpool)

C <- Liverpool$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Chelsea
Chelsea <- Prem[which(Prem$team_name == "Chelsea FC"),]

A1 <- predict(HomeModel, Chelsea)
A2 <- predict(AwayModel, Chelsea)

B1 <- predict(GoalsHome, Chelsea)
B2 <- predict(GoalsAway, Chelsea)

C <- Chelsea$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## WestHam
WestHam <- Prem[which(Prem$team_name == "West Ham United FC"),]

A1 <- predict(HomeModel, WestHam)
A2 <- predict(AwayModel, WestHam)

B1 <- predict(GoalsHome, WestHam)
B2 <- predict(GoalsAway, WestHam)

C <- WestHam$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Watford
Watford <- Prem[which(Prem$team_name == "Watford FC"),]

A1 <- predict(HomeModel, Watford)
A2 <- predict(AwayModel, Watford)

B1 <- predict(GoalsHome, Watford)
B2 <- predict(GoalsAway, Watford)

C <- Watford$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Newcastle
Newcastle <- Prem[which(Prem$team_name == "Newcastle United FC"),]

A1 <- predict(HomeModel, Newcastle)
A2 <- predict(AwayModel, Newcastle)

B1 <- predict(GoalsHome, Newcastle)
B2 <- predict(GoalsAway, Newcastle)

C <- Newcastle$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Cardiff
Cardiff <- Prem[which(Prem$team_name == "Cardiff City FC"),]

A1 <- predict(HomeModel, Cardiff)
A2 <- predict(AwayModel, Cardiff)

B1 <- predict(GoalsHome, Cardiff)
B2 <- predict(GoalsAway, Cardiff)

C <- Cardiff$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Fulham
Fulham <- Prem[which(Prem$team_name == "Fulham FC"),]

A1 <- predict(HomeModel, Fulham)
A2 <- predict(AwayModel, Fulham)

B1 <- predict(GoalsHome, Fulham)
B2 <- predict(GoalsAway, Fulham)

C <- Fulham$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Brighton
Brighton <- Prem[which(Prem$team_name == "Brighton & Hove Albion FC"),]

A1 <- predict(HomeModel, Brighton)
A2 <- predict(AwayModel, Brighton)

B1 <- predict(GoalsHome, Brighton)
B2 <- predict(GoalsAway, Brighton)

C <- Brighton$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Huddersfield
Huddersfield <- Prem[which(Prem$team_name == "Huddersfield Town FC"),]

A1 <- predict(HomeModel, Huddersfield)
A2 <- predict(AwayModel, Huddersfield)

B1 <- predict(GoalsHome, Huddersfield)
B2 <- predict(GoalsAway, Huddersfield)

C <- Huddersfield$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Wolves
Wolves <- Prem[which(Prem$team_name == "Wolverhampton Wanderers FC"),]

A1 <- predict(HomeModel, Wolves)
A2 <- predict(AwayModel, Wolves)

B1 <- predict(GoalsHome, Wolves)
B2 <- predict(GoalsAway, Wolves)

C <- Wolves$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

