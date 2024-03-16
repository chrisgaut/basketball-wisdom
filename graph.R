# Load libraries
library(tidyverse)
library(nbaplotR)

source("data.R")

# League Net Ratings Graph
team_ratings_graph <- function(year) {
  data <- get_team_ratings(year)
  
  median_ortg <- median(data$ORtg)
  median_drtg <- median(data$DRtg)
  
  net_ratings_plot <- ggplot(data = data, aes(x=ORtg, y=DRtg, label = Tm)) +
    geom_nba_logos(aes(team_abbr = Tm), width = 0.05) +
    labs(title="2023-24 League Ratings") + xlab("Offensive Rating") + ylab("Defensive Rating") +
    geom_hline(yintercept=median_drtg, color = "black", size = 1, linetype = 2) +
    geom_vline(xintercept=median_ortg, color = "black", size = 1, linetype = 2) +
    scale_x_continuous(limits = c(median_ortg - 10, median_ortg + 10)) +
    scale_y_continuous(limits = c(median_drtg - 10, median_drtg + 10)) +
    scale_y_reverse() + 
    labs(caption = "Data from Basketball Reference | @chrisgaut9") +
    theme(panel.grid = element_line(color = "grey", size = 0.75, linetype = 1),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "grey", fill=NA, size=1),
          text=element_text(family="source-sans-pro"))
  
  return(net_ratings_plot)
}

# Player Per 100 Possession Percentile Graph
player_percentile_graph <- function(player, year) {
  data <- get_per_100_poss(year)
  
  player_data <- data %>%
    filter(Player == player)
  
  # Plot
  player_percentile_plot <- ggplot(player_data) +
    #geom_col(aes(percentile, factor(Stat)), width = 0.6) +
    geom_col(aes(x = percentile, y = factor(Stat), fill = percentile, stat = "identity")) +
    scale_y_discrete(limits = c("TO/100", "AST/100", "FTA/100", "3PA/100", "2PA/100", "PTS/100")) +
    xlim(0, 100) +
    labs(title = "Per 100 Possessions Percentiles",
         caption = "Data from Basketball Reference | @chrisgaut9") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "gray", midpoint = 50) +
    theme(panel.grid = element_line(color = "grey", size = 0.75, linetype = 1),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "grey", fill=NA, size=1),
          text=element_text(family="source-sans-pro"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  return(player_percentile_plot)
}