source('data.R')

current_year <- 2025

player_list <- get_per_100_poss(current_year)

team_viz_lab_variable_list <- c('Age', 'W', 'L', 'PW', 'PL', 
                                'MOV', 'SOS', 'SRS',
                                'ORtg', 'DRtg', 'NRtg', 'Pace', 
                                'FTR', '3PAR', 'TS%', 'eFG%', 
                                'TOV%', 'ORB%', 'FT/FGA', 'Opp. eFG%',
                                'Opp. TOV%', 'DRB%', 'Opp. FT/FGA')

#team_viz_lab_variable_list <- c('Age', 'Wins', 'Losses', 'Pythag. Wins', 'Pythag. Losses', 
#                                'Margin of Victory', 'Strength of Schedule', 'Simple Rating System',
#                                'Offensive Rating', 'Defensive Rating', 'Net Rating', 'Pace', 
#                                'FT Rate', '3PA Rate', 'True Shooting %', 'Effective FG %', 
#                                'Turnover %', 'Offensive Rebound %', 'FT/FGA', 'Opp. Effective FG %',
#                                'Opp. Turnover %', 'Defensive Rebound %', 'Opp. FT/FGA')