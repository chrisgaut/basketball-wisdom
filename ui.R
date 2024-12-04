## Shiny ui

# Load libraries
library(shiny)
library(shinythemes)
library(gt)

# Load app information
source('information.R')

# UI
shinyUI(
  navbarPage(title="Basketball Wisdom",
             theme = shinytheme("cosmo"),
             tabPanel("League",
                        tabsetPanel(tabPanel("Team Stats",
                                             gt_output("team_advanced_stats_table")
                                             ),
                                    tabPanel("Visualization Builder",
                                             sidebarLayout(
                                               sidebarPanel(selectInput("xAxisTeam", "X-Axis", choices = team_viz_lab_variable_list, selected = "ORtg"),
                                                            selectInput("yAxisTeam", "Y-Axis", choices = team_viz_lab_variable_list, selected = "DRtg")),
                                               mainPanel(plotOutput("teamVizLabPlot", width = "550px", height = "550px"))
                                               )
                                             )
                        )
                      ),
             tabPanel("Players", 
                      tabsetPanel(
                        tabPanel("Player Stats",
                                 "Same table as Team Stats table"
                                 ),
                        tabPanel("Sliders",
                                 selectInput("player", label = "Player", choices = player_list),
                                 plotOutput("playerPer36PercentilePlot", width = "550px", height = "550px")
                                 ),
                        tabPanel("Viz 2",
                                 "Another viz"
                                 ),
                        tabPanel("Viz 3",
                                 "Yet another viz"
                                 )
                        )
                      ),
             tabPanel("Matchup On Paper", "Matchups on Paper"),
             tabPanel("Betting Tools", 
                      tabsetPanel(tabPanel("Odds Converter",
                                           "Convert odds between American and Decimal"
                      ),
                      tabPanel("Parlay Independence Odds",
                               "Calculate fair odds for parlay if all events are independent"
                      )
                      )),
             tabPanel("About", "About page")
             )
)