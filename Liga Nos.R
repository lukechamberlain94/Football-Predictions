### Liga Nos

setwd("~/Football Data/Liga Nos")

LigaNos <- read.csv(file = "Liga Nos.csv")
summary(LigaNos$team_name)

## Home Model
HomeModel <- lm(points_per_game ~ wins_home + goals_scored_home + clean_sheets_home + shots_home
                + goal_difference_home + goal_difference_home + average_possession_home + 
                  average_total_goals_per_match_home + goals_scored_per_match_home +
                  goals_conceded_per_match_home + shots_on_target_home + win_percentage_home +
                  clean_sheet_percentage_home + draws_home + losses_home + leading_at_half_time_home, data = LigaNos)

## AWay Model
AwayModel <- lm(points_per_game ~ wins_away + goals_scored_away + clean_sheets_away + shots_away +
                  goal_difference_away + goal_difference_away + average_possession_away + 
                  average_total_goals_per_match_away + goals_scored_per_match_away + goals_conceded_per_match_away +
                  shots_on_target_away + win_percentage_away + clean_sheet_percentage_away + draws_away + losses_away +
                  leading_at_half_time_away, data = LigaNos)


### Predicted Goals Model Home
GoalsHome <- lm(goals_scored_per_match_home ~ total_goal_count_home + goals_scored_home +
                  average_possession_home + shots_home + shots_on_target_home + shots_off_target +
                  average_total_goals_per_match_home, data = LigaNos)


## Predicted Goals Model Away
GoalsAway <- lm(goals_scored_per_match_away ~ total_goal_count_away + goals_scored_away +
                  average_possession_away + shots_away + shots_on_target_away + shots_off_target +
                  average_total_goals_per_match_away, data = LigaNos)

#######################################################

## Benfica
Benfica <- LigaNos[which(LigaNos$common_name == "Benfica"),]

A1 <- predict(HomeModel, Benfica)
A2 <- predict(AwayModel, Benfica)

B1 <- predict(GoalsHome, Benfica)
B2 <- predict(GoalsAway, Benfica)

C <- Benfica$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Porto
Porto <- LigaNos[which(LigaNos$common_name == "Porto"),]

A1 <- predict(HomeModel, Porto)
A2 <- predict(AwayModel, Porto)

B1 <- predict(GoalsHome, Porto)
B2 <- predict(GoalsAway, Porto)

C <- Porto$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Sporting
Sporting <- LigaNos[which(LigaNos$common_name == "Sporting CP"),]

A1 <- predict(HomeModel, Sporting)
A2 <- predict(AwayModel, Sporting)

B1 <- predict(GoalsHome, Sporting)
B2 <- predict(GoalsAway, Sporting)

C <- Sporting$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Nacional
Nacional <- LigaNos[which(LigaNos$common_name == "Nacional"),]

A1 <- predict(HomeModel, Nacional)
A2 <- predict(AwayModel, Nacional)

B1 <- predict(GoalsHome, Nacional)
B2 <- predict(GoalsAway, Nacional)

C <- Nacional$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3



## Chaves
Chaves <- LigaNos[which(LigaNos$common_name == "Chaves"),]

A1 <- predict(HomeModel, Chaves)
A2 <- predict(AwayModel, Chaves)

B1 <- predict(GoalsHome, Chaves)
B2 <- predict(GoalsAway, Chaves)

C <- Chaves$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Feirense
Feirense <- LigaNos[which(LigaNos$common_name == "Feirense"),]

A1 <- predict(HomeModel, Feirense)
A2 <- predict(AwayModel, Feirense)

B1 <- predict(GoalsHome, Feirense)
B2 <- predict(GoalsAway, Feirense)

C <- Feirense$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Belenenses
Belenenses <- LigaNos[which(LigaNos$common_name == "Belenenses"),]

A1 <- predict(HomeModel, Belenenses)
A2 <- predict(AwayModel, Belenenses)

B1 <- predict(GoalsHome, Belenenses)
B2 <- predict(GoalsAway, Belenenses)

C <- Belenenses$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Tondela
Tondela <- LigaNos[which(LigaNos$common_name == "Tondela"),]

A1 <- predict(HomeModel, Tondela)
A2 <- predict(AwayModel, Tondela)

B1 <- predict(GoalsHome, Tondela)
B2 <- predict(GoalsAway, Tondela)

C <- Tondela$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Victoria
Victoria <- LigaNos[which(LigaNos$common_name == "Vitória Setúbal"),]

A1 <- predict(HomeModel, Victoria)
A2 <- predict(AwayModel, Victoria)

B1 <- predict(GoalsHome, Victoria)
B2 <- predict(GoalsAway, Victoria)

C <- Victoria$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Rio
Rio <- LigaNos[which(LigaNos$common_name == "Rio Ave"),]

A1 <- predict(HomeModel, Rio)
A2 <- predict(AwayModel, Rio)

B1 <- predict(GoalsHome, Rio)
B2 <- predict(GoalsAway, Rio)

C <- Rio$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Braga
Braga <- LigaNos[which(LigaNos$common_name == "Sporting Braga"),]

A1 <- predict(HomeModel, Braga)
A2 <- predict(AwayModel, Braga)

B1 <- predict(GoalsHome, Braga)
B2 <- predict(GoalsAway, Braga)

C <- Braga$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Moreirense
Moreirense <- LigaNos[which(LigaNos$common_name == "Moreirense"),]

A1 <- predict(HomeModel, Moreirense)
A2 <- predict(AwayModel, Moreirense)

B1 <- predict(GoalsHome, Moreirense)
B2 <- predict(GoalsAway, Moreirense)

C <- Moreirense$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Guimares
Guimares<- LigaNos[which(LigaNos$common_name == "Vitória Guimarães"),]

A1 <- predict(HomeModel, Guimares)
A2 <- predict(AwayModel, Guimares)

B1 <- predict(GoalsHome, Guimares)
B2 <- predict(GoalsAway, Guimares)

C <- Guimares$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Boavista
Boavista <- LigaNos[which(LigaNos$common_name == "Boavista"),]

A1 <- predict(HomeModel, Boavista)
A2 <- predict(AwayModel, Boavista)

B1 <- predict(GoalsHome, Boavista)
B2 <- predict(GoalsAway, Boavista)

C <- Boavista$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Maratimo
Maratimo <- LigaNos[which(LigaNos$common_name == "Maratimo"),]

A1 <- predict(HomeModel, Maratimo)
A2 <- predict(AwayModel, Maratimo)

B1 <- predict(GoalsHome, Maratimo)
B2 <- predict(GoalsAway, Maratimo)

C <- Maratimo$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Aves
Aves <- LigaNos[which(LigaNos$common_name == "Desportivo Aves"),]

A1 <- predict(HomeModel, Aves)
A2 <- predict(AwayModel, Aves)

B1 <- predict(GoalsHome, Aves)
B2 <- predict(GoalsAway, Aves)

C <- Aves$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Portimonense
Portimonense <- LigaNos[which(LigaNos$common_name == "Portimonense"),]

A1 <- predict(HomeModel, Portimonense)
A2 <- predict(AwayModel, Portimonense)

B1 <- predict(GoalsHome, Portimonense)
B2 <- predict(GoalsAway, Portimonense)

C <- Portimonense$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Clara
Clara <- LigaNos[which(LigaNos$common_name == "Santa Clara"),]

A1 <- predict(HomeModel, Clara)
A2 <- predict(AwayModel, Clara)

B1 <- predict(GoalsHome, Clara)
B2 <- predict(GoalsAway, Clara)

C <- Clara$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3





