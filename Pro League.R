### Pro League

setwd("~/Football Data/Pro League")

ProLeague <- read.csv(file = "Pro League.csv")
summary(ProLeague$team_name)

## Home Model
HomeModel <- lm(points_per_game ~ wins_home + goals_scored_home + clean_sheets_home + shots_home
                + goal_difference_home + goal_difference_home + average_possession_home + 
                  average_total_goals_per_match_home + goals_scored_per_match_home +
                  goals_conceded_per_match_home + shots_on_target_home + win_percentage_home +
                  clean_sheet_percentage_home + draws_home + losses_home + leading_at_half_time_home, data = ProLeague)

## AWay Model
AwayModel <- lm(points_per_game ~ wins_away + goals_scored_away + clean_sheets_away + shots_away +
                  goal_difference_away + goal_difference_away + average_possession_away + 
                  average_total_goals_per_match_away + goals_scored_per_match_away + goals_conceded_per_match_away +
                  shots_on_target_away + win_percentage_away + clean_sheet_percentage_away + draws_away + losses_away +
                  leading_at_half_time_away, data = ProLeague)


### Predicted Goals Model Home
GoalsHome <- lm(goals_scored_per_match_home ~ total_goal_count_home + goals_scored_home +
                  average_possession_home + shots_home + shots_on_target_home + shots_off_target +
                  average_total_goals_per_match_home, data = ProLeague)


## Predicted Goals Model Away
GoalsAway <- lm(goals_scored_per_match_away ~ total_goal_count_away + goals_scored_away +
                  average_possession_away + shots_away + shots_on_target_away + shots_off_target +
                  average_total_goals_per_match_away, data = ProLeague)

#######################################################

## Anderlecht
Anderlecht <- ProLeague[which(ProLeague$common_name == "Anderlecht"),]

A1 <- predict(HomeModel, Anderlecht)
A2 <- predict(AwayModel, Anderlecht)

B1 <- predict(GoalsHome, Anderlecht)
B2 <- predict(GoalsAway, Anderlecht)

C <- Anderlecht$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

##Club Brugge
Brugge <- ProLeague[which(ProLeague$common_name == "Club Brugge"),]

A1 <- predict(HomeModel, Brugge)
A2 <- predict(AwayModel, Brugge)

B1 <- predict(GoalsHome, Brugge)
B2 <- predict(GoalsAway, Brugge)

C <- Brugge$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Gent
Gent <- ProLeague[which(ProLeague$common_name == "Gent"),]

A1 <- predict(HomeModel, Gent)
A2 <- predict(AwayModel, Gent)

B1 <- predict(GoalsHome, Gent)
B2 <- predict(GoalsAway, Gent)

C <- Gent$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Waasland-Beveren
WaaslandBeveren <- ProLeague[which(ProLeague$common_name == "Waasland-Beveren"),]

A1 <- predict(HomeModel, WaaslandBeveren)
A2 <- predict(AwayModel, WaaslandBeveren)

B1 <- predict(GoalsHome, WaaslandBeveren)
B2 <- predict(GoalsAway, WaaslandBeveren)

C <- WaaslandBeveren$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3



## Liege
Liege <- ProLeague[which(ProLeague$common_name == "Standard Liège"),]

A1 <- predict(HomeModel, Liege)
A2 <- predict(AwayModel, Liege)

B1 <- predict(GoalsHome, Liege)
B2 <- predict(GoalsAway, Liege)

C <- Liege$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Lokeren
Lokeren <- ProLeague[which(ProLeague$common_name == "Lokeren"),]

A1 <- predict(HomeModel, Lokeren)
A2 <- predict(AwayModel, Lokeren)

B1 <- predict(GoalsHome, Lokeren)
B2 <- predict(GoalsAway, Lokeren)

C <- Lokeren$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Charleroi
Charleroi <- ProLeague[which(ProLeague$common_name == "Sporting Charleroi"),]

A1 <- predict(HomeModel, Charleroi)
A2 <- predict(AwayModel, Charleroi)

B1 <- predict(GoalsHome, Charleroi)
B2 <- predict(GoalsAway, Charleroi)

C <- Charleroi$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Eupen
Eupen <- ProLeague[which(ProLeague$common_name == "AS Eupen"),]

A1 <- predict(HomeModel, Eupen)
A2 <- predict(AwayModel, Eupen)

B1 <- predict(GoalsHome, Eupen)
B2 <- predict(GoalsAway, Eupen)

C <- Eupen$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Ooostende
Ooostende <- ProLeague[which(ProLeague$common_name == "KV Oostende"),]

A1 <- predict(HomeModel, Ooostende)
A2 <- predict(AwayModel, Ooostende)

B1 <- predict(GoalsHome, Ooostende)
B2 <- predict(GoalsAway, Ooostende)

C <- Ooostende$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Sint-Truiden
Truiden <- ProLeague[which(ProLeague$common_name == "Sint-Truiden"),]

A1 <- predict(HomeModel, Truiden)
A2 <- predict(AwayModel, Truiden)

B1 <- predict(GoalsHome, Truiden)
B2 <- predict(GoalsAway, Truiden)

C <- Truiden$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Genk
Genk <- ProLeague[which(ProLeague$common_name == "Genk"),]

A1 <- predict(HomeModel, Genk)
A2 <- predict(AwayModel, Genk)

B1 <- predict(GoalsHome, Genk)
B2 <- predict(GoalsAway, Genk)

C <- Genk$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Zulte
Zulte <- ProLeague[which(ProLeague$common_name == "Zulte-Waregem"),]

A1 <- predict(HomeModel, Zulte)
A2 <- predict(AwayModel, Zulte)

B1 <- predict(GoalsHome, Zulte)
B2 <- predict(GoalsAway, Zulte)

C <- Zulte$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Kortrijk
Kortrijk <- ProLeague[which(ProLeague$common_name == "Kortrijk"),]

A1 <- predict(HomeModel, Kortrijk)
A2 <- predict(AwayModel, Kortrijk)

B1 <- predict(GoalsHome, Kortrijk)
B2 <- predict(GoalsAway, Kortrijk)

C <- Kortrijk$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## mouscron
mouscron <- ProLeague[which(ProLeague$common_name == "Royal Excel Mouscron"),]

A1 <- predict(HomeModel, mouscron)
A2 <- predict(AwayModel, mouscron)

B1 <- predict(GoalsHome, mouscron)
B2 <- predict(GoalsAway, mouscron)

C <- mouscron$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Antwerp
Antwerp <- ProLeague[which(ProLeague$common_name == "Antwerp"),]

A1 <- predict(HomeModel, Antwerp)
A2 <- predict(AwayModel, Antwerp)

B1 <- predict(GoalsHome, Antwerp)
B2 <- predict(GoalsAway, Antwerp)

C <- Antwerp$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## CercleBrugge
CercleBrugge <- ProLeague[which(ProLeague$common_name == "Cercle Brugge"),]

A1 <- predict(HomeModel, CercleBrugge)
A2 <- predict(AwayModel, CercleBrugge)

B1 <- predict(GoalsHome, CercleBrugge)
B2 <- predict(GoalsAway, CercleBrugge)

C <- CercleBrugge$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

