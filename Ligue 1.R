### Ligue 1

setwd("~/Football Data/Ligue 1")

Ligue1 <- read.csv(file = "Ligue 1.csv")
summary(Ligue1$team_name)

## Home Model
HomeModel <- lm(points_per_game ~ wins_home + goals_scored_home + clean_sheets_home + shots_home
                + goal_difference_home + goal_difference_home + average_possession_home + 
                  average_total_goals_per_match_home + goals_scored_per_match_home +
                  goals_conceded_per_match_home + shots_on_target_home + win_percentage_home +
                  clean_sheet_percentage_home + draws_home + losses_home + leading_at_half_time_home, data = Ligue1)

## AWay Model
AwayModel <- lm(points_per_game ~ wins_away + goals_scored_away + clean_sheets_away + shots_away +
                  goal_difference_away + goal_difference_away + average_possession_away + 
                  average_total_goals_per_match_away + goals_scored_per_match_away + goals_conceded_per_match_away +
                  shots_on_target_away + win_percentage_away + clean_sheet_percentage_away + draws_away + losses_away +
                  leading_at_half_time_away, data = Ligue1)


### Predicted Goals Model Home
GoalsHome <- lm(goals_scored_per_match_home ~ total_goal_count_home + goals_scored_home +
                  average_possession_home + shots_home + shots_on_target_home + shots_off_target +
                  average_total_goals_per_match_home, data = Ligue1)


## Predicted Goals Model Away
GoalsAway <- lm(goals_scored_per_match_away ~ total_goal_count_away + goals_scored_away +
                  average_possession_away + shots_away + shots_on_target_away + shots_off_target +
                  average_total_goals_per_match_away, data = Ligue1)

#######################################################

## Monaco
Monaco <- Ligue1[which(Ligue1$common_name == "Monaco"),]

A1 <- predict(HomeModel, Monaco)
A2 <- predict(AwayModel, Monaco)

B1 <- predict(GoalsHome, Monaco)
B2 <- predict(GoalsAway, Monaco)

C <- Monaco$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Lyon
Lyon <- Ligue1[which(Ligue1$common_name == "Olympique Lyonnais"),]

A1 <- predict(HomeModel, Lyon)
A2 <- predict(AwayModel, Lyon)

B1 <- predict(GoalsHome, Lyon)
B2 <- predict(GoalsAway, Lyon)

C <- Lyon$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## PSG
PSG <- Ligue1[which(Ligue1$common_name == "PSG"),]

A1 <- predict(HomeModel, PSG)
A2 <- predict(AwayModel, PSG)

B1 <- predict(GoalsHome, PSG)
B2 <- predict(GoalsAway, PSG)

C <- PSG$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Caen
Caen <- Ligue1[which(Ligue1$common_name == "Caen"),]

A1 <- predict(HomeModel, Caen)
A2 <- predict(AwayModel, Caen)

B1 <- predict(GoalsHome, Caen)
B2 <- predict(GoalsAway, Caen)

C <- Caen$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3



## Bordeaux
Bordeaux <- Ligue1[which(Ligue1$common_name == "Bordeaux"),]

A1 <- predict(HomeModel, Bordeaux)
A2 <- predict(AwayModel, Bordeaux)

B1 <- predict(GoalsHome, Bordeaux)
B2 <- predict(GoalsAway, Bordeaux)

C <- Bordeaux$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Etienne
Etienne <- Ligue1[which(Ligue1$common_name == "Saint-Ã???tienne"),]

A1 <- predict(HomeModel, Etienne)
A2 <- predict(AwayModel, Etienne)

B1 <- predict(GoalsHome, Etienne)
B2 <- predict(GoalsAway, Etienne)

C <- Etienne$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Montpellier
Montpellier <- Ligue1[which(Ligue1$common_name == "Montpellier"),]

A1 <- predict(HomeModel, Montpellier)
A2 <- predict(AwayModel, Montpellier)

B1 <- predict(GoalsHome, Montpellier)
B2 <- predict(GoalsAway, Montpellier)

C <- Montpellier$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Guingamp
Guingamp <- Ligue1[which(Ligue1$common_name == "Guingamp"),]

A1 <- predict(HomeModel, Guingamp)
A2 <- predict(AwayModel, Guingamp)

B1 <- predict(GoalsHome, Guingamp)
B2 <- predict(GoalsAway, Guingamp)

C <- Guingamp$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Toulouse
Toulouse <- Ligue1[which(Ligue1$common_name == "Toulouse"),]

A1 <- predict(HomeModel, Toulouse)
A2 <- predict(AwayModel, Toulouse)

B1 <- predict(GoalsHome, Toulouse)
B2 <- predict(GoalsAway, Toulouse)

C <- Toulouse$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Nantes
Nantes <- Ligue1[which(Ligue1$common_name == "Nantes"),]

A1 <- predict(HomeModel, Nantes)
A2 <- predict(AwayModel, Nantes)

B1 <- predict(GoalsHome, Nantes)
B2 <- predict(GoalsAway, Nantes)

C <- Nantes$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Lille
Lille <- Ligue1[which(Ligue1$common_name == "Lille"),]

A1 <- predict(HomeModel, Lille)
A2 <- predict(AwayModel, Lille)

B1 <- predict(GoalsHome, Lille)
B2 <- predict(GoalsAway, Lille)

C <- Lille$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Rennes 
Rennes <- Ligue1[which(Ligue1$common_name == "Rennes"),]

A1 <- predict(HomeModel, Rennes)
A2 <- predict(AwayModel, Rennes)

B1 <- predict(GoalsHome, Rennes)
B2 <- predict(GoalsAway, Rennes)

C <- Rennes$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Marseille
Marseille <- Ligue1[which(Ligue1$common_name == "Olympique Marseille"),]

A1 <- predict(HomeModel, Marseille)
A2 <- predict(AwayModel, Marseille)

B1 <- predict(GoalsHome, Marseille)
B2 <- predict(GoalsAway, Marseille)

C <- Marseille$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Angers
Angers <- Ligue1[which(Ligue1$common_name == "Angers SCO"),]

A1 <- predict(HomeModel, Angers)
A2 <- predict(AwayModel, Angers)

B1 <- predict(GoalsHome, Angers)
B2 <- predict(GoalsAway, Angers)

C <- Angers$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Nice
Nice <- Ligue1[which(Ligue1$common_name == "Nice"),]

A1 <- predict(HomeModel, Nice)
A2 <- predict(AwayModel, Nice)

B1 <- predict(GoalsHome, Nice)
B2 <- predict(GoalsAway, Nice)

C <- Nice$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Dijon
Dijon <- Ligue1[which(Ligue1$common_name == "Dijon"),]

A1 <- predict(HomeModel, Dijon)
A2 <- predict(AwayModel, Dijon)

B1 <- predict(GoalsHome, Dijon)
B2 <- predict(GoalsAway, Dijon)

C <- Dijon$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Reims
Reims <- Ligue1[which(Ligue1$common_name == "Reims"),]

A1 <- predict(HomeModel, Reims)
A2 <- predict(AwayModel, Reims)

B1 <- predict(GoalsHome, Reims)
B2 <- predict(GoalsAway, Reims)

C <- Reims$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Strasbourg
Strasbourg <- Ligue1[which(Ligue1$common_name == "Strasbourg"),]

A1 <- predict(HomeModel, Strasbourg)
A2 <- predict(AwayModel, Strasbourg)

B1 <- predict(GoalsHome, Strasbourg)
B2 <- predict(GoalsAway, Strasbourg)

C <- Strasbourg$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Amiens
Amiens <- Ligue1[which(Ligue1$common_name == "Amiens SC"),]

A1 <- predict(HomeModel, Amiens)
A2 <- predict(AwayModel, Amiens)

B1 <- predict(GoalsHome, Amiens)
B2 <- predict(GoalsAway, Amiens)

C <- Amiens$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Nimes
Nimes <- Ligue1[which(Ligue1$common_name == "NÃ®mes"),]

A1 <- predict(HomeModel, Nimes)
A2 <- predict(AwayModel, Nimes)

B1 <- predict(GoalsHome, Nimes)
B2 <- predict(GoalsAway, Nimes)

C <- Nimes$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3




