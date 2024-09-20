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
  output$teamVizLabPlot <- renderPlot({
    data <- get_team_advanced_stats_bref(current_year) %>% filter(Team!="League Average")
    
    tm_var <- data[['Tm']]
    x_axis <- data[[input$xAxisTeam]]
    y_axis <- data[[input$yAxisTeam]]
    
    data <- do.call(rbind, Map(data.frame, Tm=tm_var, X=x_axis, Y=y_axis))
    
    plot <- ggplot(data = data, aes(x=X, y=Y, label = Tm)) +
      geom_nba_logos(aes(team_abbr = Tm), width = 0.05) +
      labs(title="Visualization Builder Rough Draft") + xlab(input$xAxisTeam) + ylab(input$yAxisTeam) +
      labs(caption = "Data from Basketball Reference | https://chris-gaut.shinyapps.io/basketball-wisdom/") +
      theme(panel.grid = element_line(color = "grey", size = 0.75, linetype = 1),
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(colour = "grey", fill=NA, size=1),
            text=element_text(family="source-sans-pro"))
    
    return(plot)
  })
  
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
