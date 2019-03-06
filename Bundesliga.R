### Bundesliga

setwd("~/Football Data/Bundesliga")

Bundesliga <- read.csv(file = "Bundesliga.csv")
summary(Bundesliga$team_name)

## Home Model
HomeModel <- lm(points_per_game ~ wins_home + goals_scored_home + clean_sheets_home + shots_home
                + goal_difference_home + goal_difference_home + average_possession_home + 
                  average_total_goals_per_match_home + goals_scored_per_match_home +
                  goals_conceded_per_match_home + shots_on_target_home + win_percentage_home +
                  clean_sheet_percentage_home + draws_home + losses_home + leading_at_half_time_home, data = Bundesliga)

HomeModel
## AWay Model
AwayModel <- lm(points_per_game ~ wins_away + goals_scored_away + clean_sheets_away + shots_away +
                  goal_difference_away + goal_difference_away + average_possession_away + 
                  average_total_goals_per_match_away + goals_scored_per_match_away + goals_conceded_per_match_away +
                  shots_on_target_away + win_percentage_away + clean_sheet_percentage_away + draws_away + losses_away +
                  leading_at_half_time_away, data = Bundesliga)


### Predicted Goals Model Home
GoalsHome <- lm(goals_scored_per_match_home ~ total_goal_count_home + goals_scored_home +
                  average_possession_home + shots_home + shots_on_target_home + shots_off_target +
                  average_total_goals_per_match_home, data = Bundesliga)


## Predicted Goals Model Away
GoalsAway <- lm(goals_scored_per_match_away ~ total_goal_count_away + goals_scored_away +
                  average_possession_away + shots_away + shots_on_target_away + shots_off_target +
                  average_total_goals_per_match_away, data = Bundesliga)

#######################################################

## Dortmund
Dortmund <- Bundesliga[which(Bundesliga$common_name == "Borussia Dortmund"),]

A1 <- predict(HomeModel, Dortmund)
A2 <- predict(AwayModel, Dortmund)

B1 <- predict(GoalsHome, Dortmund)
B2 <- predict(GoalsAway, Dortmund)

C <- Dortmund$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Mainz
Mainz <- Bundesliga[which(Bundesliga$common_name == "Mainz 05"),]

A1 <- predict(HomeModel, Mainz)
A2 <- predict(AwayModel, Mainz)

B1 <- predict(GoalsHome, Mainz)
B2 <- predict(GoalsAway, Mainz)

C <- Mainz$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Schalke
Schalke <- Bundesliga[which(Bundesliga$common_name == "Schalke 04"),]

A1 <- predict(HomeModel, Schalke)
A2 <- predict(AwayModel, Schalke)

B1 <- predict(GoalsHome, Schalke)
B2 <- predict(GoalsAway, Schalke)

C <- Schalke$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Bremen
Bremen <- Bundesliga[which(Bundesliga$common_name == "Werder Bremen"),]

A1 <- predict(HomeModel, Bremen)
A2 <- predict(AwayModel, Bremen)

B1 <- predict(GoalsHome, Bremen)
B2 <- predict(GoalsAway, Bremen)

C <- Bremen$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3



## Munich
Munich <- Bundesliga[which(Bundesliga$common_name == "Bayern München"),]

A1 <- predict(HomeModel, Munich)
A2 <- predict(AwayModel, Munich)

B1 <- predict(GoalsHome, Munich)
B2 <- predict(GoalsAway, Munich)

C <- Munich$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Gladbach
Gladbach <- Bundesliga[which(Bundesliga$common_name == "Borussia M'gladbach"),]

A1 <- predict(HomeModel, Gladbach)
A2 <- predict(AwayModel, Gladbach)

B1 <- predict(GoalsHome, Gladbach)
B2 <- predict(GoalsAway, Gladbach)

C <- Gladbach$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Leverkusen 
Leverkusen <- Bundesliga[which(Bundesliga$common_name == "Bayer Leverkusen"),]

A1 <- predict(HomeModel, Leverkusen)
A2 <- predict(AwayModel, Leverkusen)

B1 <- predict(GoalsHome, Leverkusen)
B2 <- predict(GoalsAway, Leverkusen)

C <- Leverkusen$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Freiburg
Freiburg <- Bundesliga[which(Bundesliga$common_name == "Freiburg"),]

A1 <- predict(HomeModel, Freiburg)
A2 <- predict(AwayModel, Freiburg)

B1 <- predict(GoalsHome, Freiburg)
B2 <- predict(GoalsAway, Freiburg)

C <- Freiburg$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Wolfsburg 
Wolfsburg <- Bundesliga[which(Bundesliga$common_name == "Wolfsburg"),]

A1 <- predict(HomeModel, Wolfsburg)
A2 <- predict(AwayModel, Wolfsburg)

B1 <- predict(GoalsHome, Wolfsburg)
B2 <- predict(GoalsAway, Wolfsburg)

C <- Wolfsburg$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Frankfurt 
Frankfurt <- Bundesliga[which(Bundesliga$common_name == "Eintracht Frankfurt"),]

A1 <- predict(HomeModel, Frankfurt)
A2 <- predict(AwayModel, Frankfurt)

B1 <- predict(GoalsHome, Frankfurt)
B2 <- predict(GoalsAway, Frankfurt)

C <- Frankfurt$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Leipzig
Leipzig <- Bundesliga[which(Bundesliga$common_name == "RB Leipzig"),]

A1 <- predict(HomeModel, Leipzig)
A2 <- predict(AwayModel, Leipzig)

B1 <- predict(GoalsHome, Leipzig)
B2 <- predict(GoalsAway, Leipzig)

C <- Leipzig$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Hertha
Hertha <- Bundesliga[which(Bundesliga$common_name == "Hertha BSC"),]

A1 <- predict(HomeModel, Hertha)
A2 <- predict(AwayModel, Hertha)

B1 <- predict(GoalsHome, Hertha)
B2 <- predict(GoalsAway, Hertha)

C <- Hertha$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Hoffenheim
Hoffenheim <- Bundesliga[which(Bundesliga$common_name == "Hoffenheim"),]

A1 <- predict(HomeModel, Hoffenheim)
A2 <- predict(AwayModel, Hoffenheim)

B1 <- predict(GoalsHome, Hoffenheim)
B2 <- predict(GoalsAway, Hoffenheim)

C <- Hoffenheim$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Augsburg
Augsburg <- Bundesliga[which(Bundesliga$common_name == "Augsburg"),]

A1 <- predict(HomeModel, Augsburg)
A2 <- predict(AwayModel, Augsburg)

B1 <- predict(GoalsHome, Augsburg)
B2 <- predict(GoalsAway, Augsburg)

C <- Augsburg$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Stuttgart
Stuttgart <- Bundesliga[which(Bundesliga$common_name == "Stuttgart"),]

A1 <- predict(HomeModel, Stuttgart)
A2 <- predict(AwayModel, Stuttgart)

B1 <- predict(GoalsHome, Stuttgart)
B2 <- predict(GoalsAway, Stuttgart)

C <- Stuttgart$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Hannover
Hannover <- Bundesliga[which(Bundesliga$common_name == "Hannover 96"),]

A1 <- predict(HomeModel, Hannover)
A2 <- predict(AwayModel, Hannover)

B1 <- predict(GoalsHome, Hannover)
B2 <- predict(GoalsAway, Hannover)

C <- Hannover$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Nurnberg
Nurnberg <- Bundesliga[which(Bundesliga$common_name == "Nürnberg"),]

A1 <- predict(HomeModel, Nurnberg)
A2 <- predict(AwayModel, Nurnberg)

B1 <- predict(GoalsHome, Nurnberg)
B2 <- predict(GoalsAway, Nurnberg)

C <- Nurnberg$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Dusseldorf
Dusseldorf<- Bundesliga[which(Bundesliga$common_name == "Fortuna Düsseldorf"),]

A1 <- predict(HomeModel, Dusseldorf)
A2 <- predict(AwayModel, Dusseldorf)

B1 <- predict(GoalsHome, Dusseldorf)
B2 <- predict(GoalsAway, Dusseldorf)

C <- Dusseldorf$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Parma
Parma <- Bundesliga[which(Bundesliga$common_name == "Parma"),]

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
SPAL <- Bundesliga[which(Bundesliga$common_name == "SPAL"),]

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

