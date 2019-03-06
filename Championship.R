### Championship

setwd("~/Championship Data")

Champ <- read.csv(file = "Championship.csv")
summary(Champ$team_name)

## Home Model
HomeModel <- lm(points_per_game ~ wins_home + goals_scored_home + clean_sheets_home + shots_home
                + goal_difference_home + goal_difference_home + average_possession_home + 
                  average_total_goals_per_match_home + goals_scored_per_match_home +
                  goals_conceded_per_match_home + shots_on_target_home + win_percentage_home +
                  clean_sheet_percentage_home + draws_home + losses_home + leading_at_half_time_home, data = Champ)

## AWay Model
AwayModel <- lm(points_per_game ~ wins_away + goals_scored_away + clean_sheets_away + shots_away +
                  goal_difference_away + goal_difference_away + average_possession_away + 
                  average_total_goals_per_match_away + goals_scored_per_match_away + goals_conceded_per_match_away +
                  shots_on_target_away + win_percentage_away + clean_sheet_percentage_away + draws_away + losses_away +
                  leading_at_half_time_away, data = Champ)

### Predicted Goals Model Home
GoalsHome <- lm(goals_scored_per_match_home ~ total_goal_count_home + goals_scored_home +
                  average_possession_home + shots_home + shots_on_target_home + shots_off_target +
                  average_total_goals_per_match_home, data = Champ)


## Predicted Goals Model Away
GoalsAway <- lm(goals_scored_per_match_away ~ total_goal_count_away + goals_scored_away +
                  average_possession_away + shots_away + shots_on_target_away + shots_off_target +
                  average_total_goals_per_match_away, data = Champ)

#######################################################

## Sheffield United
SheffU <- Champ[which(Champ$common_name == "Sheffield United"),]

A1 <- predict(HomeModel, SheffU)
A2 <- predict(AwayModel, SheffU)

B1 <- predict(GoalsHome, SheffU)
B2 <- predict(GoalsAway, SheffU)

C <- SheffU$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## West Brom
WestBrom <- Champ[which(Champ$common_name == "West Bromwich Albion"),]

A1 <- predict(HomeModel, WestBrom)
A2 <- predict(AwayModel, WestBrom)

B1 <- predict(GoalsHome, WestBrom)
B2 <- predict(GoalsAway, WestBrom)

C <- WestBrom$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Aston Villa
Villa<- Champ[which(Champ$common_name == "Aston Villa"),]

A1 <- predict(HomeModel, Villa)
A2 <- predict(AwayModel, Villa)

B1 <- predict(GoalsHome, Villa)
B2 <- predict(GoalsAway, Villa)

C <- Villa$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Stoke City
Stoke<- Champ[which(Champ$common_name == "Stoke City"),]

A1 <- predict(HomeModel, Stoke)
A2 <- predict(AwayModel, Stoke)

B1 <- predict(GoalsHome, Stoke)
B2 <- predict(GoalsAway, Stoke)

C <- Stoke$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3



## Blackburn
Blackburn <- Champ[which(Champ$common_name == "Blackburn Rovers"),]

A1 <- predict(HomeModel, Blackburn)
A2 <- predict(AwayModel, Blackburn)

B1 <- predict(GoalsHome, Blackburn)
B2 <- predict(GoalsAway, Blackburn)

C <- Blackburn$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Birmingham 
Blues <- Champ[which(Champ$common_name == "Birmingham City"),]

A1 <- predict(HomeModel, Blues)
A2 <- predict(AwayModel, Blues)

B1 <- predict(GoalsHome, Blues)
B2 <- predict(GoalsAway, Blues)

C <- Blues$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Bolton
Bolton <- Champ[which(Champ$common_name == "Bolton Wanderers"),]

A1 <- predict(HomeModel, Bolton)
A2 <- predict(AwayModel, Bolton)

B1 <- predict(GoalsHome, Bolton)
B2 <- predict(GoalsAway, Bolton)

C <- Bolton$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Leeds
Leeds <- Champ[which(Champ$common_name == "Leeds United"),]

A1 <- predict(HomeModel, Leeds)
A2 <- predict(AwayModel, Leeds)

B1 <- predict(GoalsHome, Leeds)
B2 <- predict(GoalsAway, Leeds)

C <- Leeds$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Hull City
Hull <- Champ[which(Champ$common_name == "Hull City"),]

A1 <- predict(HomeModel, Hull)
A2 <- predict(AwayModel, Hull)

B1 <- predict(GoalsHome, Hull)
B2 <- predict(GoalsAway, HUll)

C <- Hull$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Brentford 
Brentford <- Champ[which(Champ$common_name == "Brentford"),]

A1 <- predict(HomeModel, Brentford)
A2 <- predict(AwayModel, Brentford)

B1 <- predict(GoalsHome, Brentford)
B2 <- predict(GoalsAway, Brentford)

C <- Brentford$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Ipswich
Ipswich <- Champ[which(Champ$common_name == "Ipswich Town"),]

A1 <- predict(HomeModel, Ipswich)
A2 <- predict(AwayModel, Ipswich)

B1 <- predict(GoalsHome, Ipswich)
B2 <- predict(GoalsAway, Ipswich)

C <- Ipswich$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Wigan
Wigan <- Champ[which(Champ$common_name == "Wigan Athletic"),]

A1 <- predict(HomeModel, Wigan)
A2 <- predict(AwayModel, Wigan)

B1 <- predict(GoalsHome, Wigan)
B2 <- predict(GoalsAway, Wigan)

C <- Wigan$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Preston
Preston <- Champ[which(Champ$common_name == "Preston North End"),]

A1 <- predict(HomeModel, Preston)
A2 <- predict(AwayModel, Preston)

B1 <- predict(GoalsHome, Preston)
B2 <- predict(GoalsAway, Preston)

C <- Preston$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Millwall
Millwall <- Champ[which(Champ$common_name == "Millwall"),]

A1 <- predict(HomeModel, Millwall)
A2 <- predict(AwayModel, Millwall)

B1 <- predict(GoalsHome, Millwall)
B2 <- predict(GoalsAway, Millwall)

C <- Millwall$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## QPR
QPR <- Champ[which(Champ$common_name == "Queens Park Rangers"),]

A1 <- predict(HomeModel, QPR)
A2 <- predict(AwayModel, QPR)

B1 <- predict(GoalsHome, QPR)
B2 <- predict(GoalsAway, QPR)

C <- QPR$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Middlesbrough
Middlesbrough <- Champ[which(Champ$common_name == "Middlesbrough"),]

A1 <- predict(HomeModel, Middlesbrough)
A2 <- predict(AwayModel, Middlesbrough)

B1 <- predict(GoalsHome, Middlesbrough)
B2 <- predict(GoalsAway, Middlesbrough)

C <- Middlesbrough$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Rotherham
Rotherham <- Champ[which(Champ$common_name == "Rotherham United"),]

A1 <- predict(HomeModel, Rotherham)
A2 <- predict(AwayModel, Rotherham)

B1 <- predict(GoalsHome, Rotherham)
B2 <- predict(GoalsAway, Rotherham)

C <- Rotherham$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Reading
Reading <- Champ[which(Champ$common_name == "Reading"),]

A1 <- predict(HomeModel, Reading)
A2 <- predict(AwayModel, Reading)

B1 <- predict(GoalsHome, Reading)
B2 <- predict(GoalsAway, Reading)

C <- Reading$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Swansea
Swansea <- Champ[which(Champ$common_name == "Swansea City"),]

A1 <- predict(HomeModel, Swansea)
A2 <- predict(AwayModel, Swansea)

B1 <- predict(GoalsHome, Swansea)
B2 <- predict(GoalsAway, Swansea)

C <- Swansea$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Shef Wednesday
SheffWed<- Champ[which(Champ$common_name == "Sheffield Wednesday"),]

A1 <- predict(HomeModel, SheffWed)
A2 <- predict(AwayModel, SheffWed)

B1 <- predict(GoalsHome, SheffWed)
B2 <- predict(GoalsAway, SheffWed)

C <- SheffWed$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Bristol
Bristol <- Champ[which(Champ$common_name == "Bristol City"),]

A1 <- predict(HomeModel, Bristol )
A2 <- predict(AwayModel, Bristol )

B1 <- predict(GoalsHome, Bristol )
B2 <- predict(GoalsAway, Bristol )

C <- Bristol $points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Norwich
Norwich <- Champ[which(Champ$common_name == "Norwich City"),]

A1 <- predict(HomeModel, Norwich)
A2 <- predict(AwayModel, Norwich)

B1 <- predict(GoalsHome, Norwich)
B2 <- predict(GoalsAway, Norwich)

C <- Norwich$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Derby
Derby <- Champ[which(Champ$common_name == "Derby County"),]

A1 <- predict(HomeModel, Derby)
A2 <- predict(AwayModel, Derby)

B1 <- predict(GoalsHome, Derby)
B2 <- predict(GoalsAway, Derby)

C <- Derby$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

## Nottingham Forest
Forest <- Champ[which(Champ$common_name == "Nottingham Forest"),]

A1 <- predict(HomeModel, Forest)
A2 <- predict(AwayModel, Forest)

B1 <- predict(GoalsHome, Forest)
B2 <- predict(GoalsAway, Forest)

C <- Forest$points_per_game_last_5

##Home
D1 <- A1+B1+C*1.5
D1/3

##Away
D2 <- A2+B2+C*1.5
D2/3

