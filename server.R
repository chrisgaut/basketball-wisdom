## Shiny server

# Load libraries
library(shiny)
library(gt)

# Load source code
source('data.R')
source('graph.R')
source('table.R')
source('information.R')

# Server
server <- function(input, output) {
  
  # Outputs
  output$teamRatingsPlot <- renderPlot({
    plot <- team_ratings_graph(current_year)
    return(plot)
  })
  
  output$teamRatingsTable <- render_gt({
    table <- team_ratings_table(current_year)
    return(table)
  })
  
  output$playerPercentilePlot <- renderPlot({
    plot <- player_percentile_graph(input$player, current_year)
    return(plot)
  })
}
