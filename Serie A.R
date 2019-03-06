### Serie A

setwd("~/Football Data/Serie A")

SerieA <- read.csv(file = "Serie A.csv")
summary(SerieA$team_name)

## Home Model
HomeModel <- lm(points_per_game ~ wins_home + goals_scored_home + clean_sheets_home + shots_home
                + goal_difference_home + goal_difference_home + average_possession_home + 
                  average_total_goals_per_match_home + goals_scored_per_match_home +
                  goals_conceded_per_match_home + shots_on_target_home + win_percentage_home +
                  clean_sheet_percentage_home + draws_home + losses_home + leading_at_half_time_home, data = SerieA)

## AWay Model
AwayModel <- lm(points_per_game ~ wins_away + goals_scored_away + clean_sheets_away + shots_away +
                  goal_difference_away + goal_difference_away + average_possession_away + 
                  average_total_goals_per_match_away + goals_scored_per_match_away + goals_conceded_per_match_away +
                  shots_on_target_away + win_percentage_away + clean_sheet_percentage_away + draws_away + losses_away +
                  leading_at_half_time_away, data = SerieA)


### Predicted Goals Model Home
GoalsHome <- lm(goals_scored_per_match_home ~ total_goal_count_home + goals_scored_home +
                  average_possession_home + shots_home + shots_on_target_home + shots_off_target +
                  average_total_goals_per_match_home, data = SerieA)


## Predicted Goals Model Away
GoalsAway <- lm(goals_scored_per_match_away ~ total_goal_count_away + goals_scored_away +
                  average_possession_away + shots_away + shots_on_target_away + shots_off_target +
                  average_total_goals_per_match_away, data = SerieA)

#######################################################

## Napoli
Napoli <- SerieA[which(SerieA$common_name == "Napoli"),]

A1 <- predict(HomeModel, Napoli)
A2 <- predict(AwayModel, Napoli)

B1 <- predict(GoalsHome, Napoli)
B2 <- predict(GoalsAway, Napoli)

C <- Napoli$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Juventus
Juventus <- SerieA[which(SerieA$common_name == "Juventus"),]

A1 <- predict(HomeModel, Juventus)
A2 <- predict(AwayModel, Juventus)

B1 <- predict(GoalsHome, Juventus)
B2 <- predict(GoalsAway, Juventus)

C <- Juventus$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Roma
Roma <- SerieA[which(SerieA$common_name == "Roma"),]

A1 <- predict(HomeModel, Roma)
A2 <- predict(AwayModel, Roma)

B1 <- predict(GoalsHome, Roma)
B2 <- predict(GoalsAway, Roma)

C <- Roma$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Cagliari
Cagliari <- SerieA[which(SerieA$common_name == "Cagliari"),]

A1 <- predict(HomeModel, Cagliari)
A2 <- predict(AwayModel, Cagliari)

B1 <- predict(GoalsHome, Cagliari)
B2 <- predict(GoalsAway, Cagliari)

C <- Cagliari$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3



## Torino
Torino <- SerieA[which(SerieA$common_name == "Torino"),]

A1 <- predict(HomeModel, Torino)
A2 <- predict(AwayModel, Torino)

B1 <- predict(GoalsHome, Torino)
B2 <- predict(GoalsAway, Torino)

C <- Torino$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Empoli
Empoli <- SerieA[which(SerieA$common_name == "Empoli"),]

A1 <- predict(HomeModel, Empoli)
A2 <- predict(AwayModel, Empoli)

B1 <- predict(GoalsHome, Empoli)
B2 <- predict(GoalsAway, Empoli)

C <- Empoli$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Genoa 
Genoa <- SerieA[which(SerieA$common_name == "Genoa"),]

A1 <- predict(HomeModel, Genoa)
A2 <- predict(AwayModel, Genoa)

B1 <- predict(GoalsHome, Genoa)
B2 <- predict(GoalsAway, Genoa)

C <- Genoa$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Milan
Milan <- SerieA[which(SerieA$common_name == "Milan"),]

A1 <- predict(HomeModel, Milan)
A2 <- predict(AwayModel, Milan)

B1 <- predict(GoalsHome, Milan)
B2 <- predict(GoalsAway, Milan)

C <- Milan$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Lazio 
Lazio <- SerieA[which(SerieA$common_name == "Lazio"),]

A1 <- predict(HomeModel, Lazio)
A2 <- predict(AwayModel, Lazio)

B1 <- predict(GoalsHome, Lazio)
B2 <- predict(GoalsAway, Lazio)

C <- Lazio$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Atalanta 
Atalanta <- SerieA[which(SerieA$common_name == "Atalanta"),]

A1 <- predict(HomeModel, Atalanta)
A2 <- predict(AwayModel, Atalanta)

B1 <- predict(GoalsHome, Atalanta)
B2 <- predict(GoalsAway, Atalanta)

C <- Atalanta$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Sampdoria
Sampdoria <- SerieA[which(SerieA$common_name == "Sampdoria"),]

A1 <- predict(HomeModel, Sampdoria)
A2 <- predict(AwayModel, Sampdoria)

B1 <- predict(GoalsHome, Sampdoria)
B2 <- predict(GoalsAway, Sampdoria)

C <- Sampdoria$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Udinese
Udinese <- SerieA[which(SerieA$common_name == "Udinese"),]

A1 <- predict(HomeModel, Udinese)
A2 <- predict(AwayModel, Udinese)

B1 <- predict(GoalsHome, Udinese)
B2 <- predict(GoalsAway, Udinese)

C <- Udinese$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Bologna
Bologna <- SerieA[which(SerieA$common_name == "Bologna"),]

A1 <- predict(HomeModel, Bologna)
A2 <- predict(AwayModel, Bologna)

B1 <- predict(GoalsHome, Bologna)
B2 <- predict(GoalsAway, Bologna)

C <- Bologna$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Chievo
Chievo <- SerieA[which(SerieA$common_name == "Chievo"),]

A1 <- predict(HomeModel, Chievo)
A2 <- predict(AwayModel, Chievo)

B1 <- predict(GoalsHome, Chievo)
B2 <- predict(GoalsAway, Chievo)

C <- Chievo$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Sassuolo
Sassuolo <- SerieA[which(SerieA$common_name == "Sassuolo"),]

A1 <- predict(HomeModel, Sassuolo)
A2 <- predict(AwayModel, Sassuolo)

B1 <- predict(GoalsHome, Sassuolo)
B2 <- predict(GoalsAway, Sassuolo)

C <- Sassuolo$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Internazionale
Internazionale <- SerieA[which(SerieA$common_name == "Internazionale"),]

A1 <- predict(HomeModel, Internazionale)
A2 <- predict(AwayModel, Internazionale)

B1 <- predict(GoalsHome, Internazionale)
B2 <- predict(GoalsAway, Internazionale)

C <- Internazionale$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Fiorentina
Fiorentina <- SerieA[which(SerieA$common_name == "Fiorentina"),]

A1 <- predict(HomeModel, Fiorentina)
A2 <- predict(AwayModel, Fiorentina)

B1 <- predict(GoalsHome, Fiorentina)
B2 <- predict(GoalsAway, Fiorentina)

C <- Fiorentina$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Frosinone 
Frosinone <- SerieA[which(SerieA$common_name == "Frosinone"),]

A1 <- predict(HomeModel, Frosinone)
A2 <- predict(AwayModel, Frosinone)

B1 <- predict(GoalsHome, Frosinone)
B2 <- predict(GoalsAway, Frosinone)

C <- Frosinone$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Parma
Parma <- SerieA[which(SerieA$common_name == "Parma"),]

A1 <- predict(HomeModel, Parma)
A2 <- predict(AwayModel, Parma)

B1 <- predict(GoalsHome, Parma)
B2 <- predict(GoalsAway, Parma)

C <- Parma$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## SPAL
SPAL <- SerieA[which(SerieA$common_name == "SPAL"),]

A1 <- predict(HomeModel, SPAL)
A2 <- predict(AwayModel, SPAL)

B1 <- predict(GoalsHome, SPAL)
B2 <- predict(GoalsAway, SPAL)

C <- SPAL$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

