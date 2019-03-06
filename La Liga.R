### La Liga

setwd("~/Football Data/La Liga Data")

LaLiga <- read.csv(file = "LaLiga.csv")
summary(LaLiga$team_name)

## Home Model
HomeModel <- lm(points_per_game ~ wins_home + goals_scored_home + clean_sheets_home + shots_home
                + goal_difference_home + goal_difference_home + average_possession_home + 
                  average_total_goals_per_match_home + goals_scored_per_match_home +
                  goals_conceded_per_match_home + shots_on_target_home + win_percentage_home +
                  clean_sheet_percentage_home + draws_home + losses_home + leading_at_half_time_home, data = LaLiga)

## AWay Model
AwayModel <- lm(points_per_game ~ wins_away + goals_scored_away + clean_sheets_away + shots_away +
                  goal_difference_away + goal_difference_away + average_possession_away + 
                  average_total_goals_per_match_away + goals_scored_per_match_away + goals_conceded_per_match_away +
                  shots_on_target_away + win_percentage_away + clean_sheet_percentage_away + draws_away + losses_away +
                  leading_at_half_time_away, data = LaLiga)


### Predicted Goals Model Home
GoalsHome <- lm(goals_scored_per_match_home ~ total_goal_count_home + goals_scored_home +
                  average_possession_home + shots_home + shots_on_target_home + shots_off_target +
                  average_total_goals_per_match_home, data = LaLiga)


## Predicted Goals Model Away
GoalsAway <- lm(goals_scored_per_match_away ~ total_goal_count_away + goals_scored_away +
                  average_possession_away + shots_away + shots_on_target_away + shots_off_target +
                  average_total_goals_per_match_away, data = LaLiga)

#######################################################

## Girona
Girona <- LaLiga[which(LaLiga$common_name == "Girona"),]

A1 <- predict(HomeModel, Girona)
A2 <- predict(AwayModel, Girona)

B1 <- predict(GoalsHome, Girona)
B2 <- predict(GoalsAway, Girona)

C <- Girona$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Getafe
Getafe <- LaLiga[which(LaLiga$common_name == "Getafe"),]

A1 <- predict(HomeModel, Getafe)
A2 <- predict(AwayModel, Getafe)

B1 <- predict(GoalsHome, Getafe)
B2 <- predict(GoalsAway, Getafe)

C <- Getafe$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Real Sociedad
RealSociedad <- LaLiga[which(LaLiga$common_name == "Real Sociedad"),]

A1 <- predict(HomeModel, RealSociedad)
A2 <- predict(AwayModel, RealSociedad)

B1 <- predict(GoalsHome, RealSociedad)
B2 <- predict(GoalsAway, RealSociedad)

C <- RealSociedad$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## CD Alaves
Deportivo <- LaLiga[which(LaLiga$common_name == "Deportivo "),]

A1 <- predict(HomeModel, Deportivo)
A2 <- predict(AwayModel, Deportivo)

B1 <- predict(GoalsHome, Deportivo)
B2 <- predict(GoalsAway, Deportivo)

C <- Deportivo$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3



## Real Betis
Betis <- LaLiga[which(LaLiga$common_name == "Real Betis"),]

A1 <- predict(HomeModel, Betis)
A2 <- predict(AwayModel, Betis)

B1 <- predict(GoalsHome, Betis)
B2 <- predict(GoalsAway, Betis)

C <- Betis$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Eibar
Eibar <- LaLiga[which(LaLiga$common_name == "Eibar"),]

A1 <- predict(HomeModel, Eibar)
A2 <- predict(AwayModel, Eibar)

B1 <- predict(GoalsHome, Eibar)
B2 <- predict(GoalsAway, Eibar)

C <- Eibar$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Atletico Madrid
AtleticoMadrid <- LaLiga[which(LaLiga$common_name == "Atletico Madrid"),]

A1 <- predict(HomeModel, AtleticoMadrid)
A2 <- predict(AwayModel, AtleticoMadrid)

B1 <- predict(GoalsHome, AtleticoMadrid)
B2 <- predict(GoalsAway, AtleticoMadrid)

C <- AtleticoMadrid$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Espanyol
Espanyol <- LaLiga[which(LaLiga$common_name == "Espanyol"),]

A1 <- predict(HomeModel, Espanyol)
A2 <- predict(AwayModel, Espanyol)

B1 <- predict(GoalsHome, Espanyol)
B2 <- predict(GoalsAway, Espanyol)

C <- Espanyol$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Barcelona
Barcelona <- LaLiga[which(LaLiga$common_name == "Barcelona"),]

A1 <- predict(HomeModel, Barcelona)
A2 <- predict(AwayModel, Barcelona)

B1 <- predict(GoalsHome, Barcelona)
B2 <- predict(GoalsAway, Barcelona)

C <- Barcelona$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Celta Vigo
CeltaVigo <- LaLiga[which(LaLiga$common_name == "Celta de Vigo"),]

A1 <- predict(HomeModel, CeltaVigo)
A2 <- predict(AwayModel, CeltaVigo)

B1 <- predict(GoalsHome, CeltaVigo)
B2 <- predict(GoalsAway, CeltaVigo)

C <- CeltaVigo$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Atletic Bilbao
AtleticBilbao <- LaLiga[which(LaLiga$common_name == "Athletic Club"),]

A1 <- predict(HomeModel, AtleticBilbao)
A2 <- predict(AwayModel, AtleticBilbao)

B1 <- predict(GoalsHome, AtleticBilbao)
B2 <- predict(GoalsAway, AtleticBilbao)

C <- AtleticBilbao$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Valladoid
RealValladoid <- LaLiga[which(LaLiga$common_name == "Real Valladolid"),]

A1 <- predict(HomeModel, RealValladoid)
A2 <- predict(AwayModel, RealValladoid)

B1 <- predict(GoalsHome, RealValladoid)
B2 <- predict(GoalsAway, RealValladoid)

C <- RealValladoid$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Valencia
Valencia <- LaLiga[which(LaLiga$common_name == "Valencia"),]

A1 <- predict(HomeModel, Valencia)
A2 <- predict(AwayModel, Valencia)

B1 <- predict(GoalsHome, Valencia)
B2 <- predict(GoalsAway, Valencia)

C <- Valencia$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Huesca
Huesca <- LaLiga[which(LaLiga$common_name == "Huesca"),]

A1 <- predict(HomeModel, Huesca)
A2 <- predict(AwayModel, Huesca)

B1 <- predict(GoalsHome, Huesca)
B2 <- predict(GoalsAway, Huesca)

C <- Huesca$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Leganes
Leganes <- LaLiga[which(LaLiga$common_name == "Leganes"),]

A1 <- predict(HomeModel, Leganes)
A2 <- predict(AwayModel, Leganes)

B1 <- predict(GoalsHome, Leganes)
B2 <- predict(GoalsAway, Leganes)

C <- Leganes$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Sevilla
Sevilla <- LaLiga[which(LaLiga$common_name == "Sevilla"),]

A1 <- predict(HomeModel, Sevilla)
A2 <- predict(AwayModel, Sevilla)

B1 <- predict(GoalsHome, Sevilla)
B2 <- predict(GoalsAway, Sevilla)

C <- Sevilla$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## RVallecano
RVallecano <- LaLiga[which(LaLiga$common_name == "Rayo Vallecano"),]

A1 <- predict(HomeModel, RVallecano)
A2 <- predict(AwayModel, RVallecano)

B1 <- predict(GoalsHome, RVallecano)
B2 <- predict(GoalsAway, RVallecano)

C <- RVallecano$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Levante
Levante <- LaLiga[which(LaLiga$common_name == "Levante"),]

A1 <- predict(HomeModel, Levante)
A2 <- predict(AwayModel, Levante)

B1 <- predict(GoalsHome, Levante)
B2 <- predict(GoalsAway, Levante)

C <- Levante$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Villareal
Villareal <- LaLiga[which(LaLiga$common_name == "Villarreal"),]

A1 <- predict(HomeModel, Villareal)
A2 <- predict(AwayModel, Villareal)

B1 <- predict(GoalsHome, Villareal)
B2 <- predict(GoalsAway, Villareal)

C <- Villareal$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## RealMadrid
RealMadrid <- LaLiga[which(LaLiga$common_name == "Real Madrid"),]

A1 <- predict(HomeModel, RealMadrid)
A2 <- predict(AwayModel, RealMadrid)

B1 <- predict(GoalsHome, RealMadrid)
B2 <- predict(GoalsAway, RealMadrid)

C <- RealMadrid$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3
 



