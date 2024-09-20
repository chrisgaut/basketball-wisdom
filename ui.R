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
             tabPanel("Visualization Lab", 
                      selectInput("xAxisTeam", "X-Axis", choices = c("MOV", "SOS")),
                      selectInput("yAxisTeam", "Y-Axis", choices = c("MOV", "SOS")),
                      plotOutput("teamVizLabPlot")),
             tabPanel("League", 
                      plotOutput("teamRatingsPlot", width = "550px", height = "550px"),
                      gt_output("teamRatingsTable")),
             tabPanel("Player", 
                      selectInput("player", label = "Player", choices = player_list),
                      plotOutput("playerPercentilePlot", width = "550px", height = "550px")),
             tabPanel("On Paper Matchup", "Team vs Team On Paper Visualization"),
             tabPanel("Model", "Model page"),
             tabPanel("Betting Tools", "Page with different betting calculation tools"),
             tabPanel("About", "About page")
)
)
