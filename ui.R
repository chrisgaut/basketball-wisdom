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
             tabPanel("Visualization Lab", "Visualization Page"),
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
