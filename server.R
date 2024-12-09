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
  
  output$team_advanced_stats_table <- render_gt({
    data <- get_team_advanced_stats_bref(current_year) %>% 
      filter(Team!="League Average") %>%
      mutate(`W-L` = str_c(W, "-", L),
             `PW-PL` = str_c(PW, "-", PL),
             .before = 2) %>%
      select(-Age, -W, -L, -PW, -PL, -Arena, -Attendance, -`Attendance/G`, -Team)
    
    table <- data |> 
      mutate(`Opp. ORB%` = 100-`DRB%`) |>
      select(Tm, `W-L`, `PW-PL`, MOV, SOS, NRtg, ORtg, DRtg, `TS%`, `eFG%`, `3PAr`, 
             `ORB%`, `Opp. ORB%`, `FT/FGA`, `TOV%`, `Opp. eFG%`, `Opp. FT/FGA`, `Opp. TOV%`, Pace, FTr) |>
      gt() |> 
      data_color(columns = vars(MOV, SOS, ORtg, NRtg, Pace, FTr, `3PAr`, `TS%`, 
                                `eFG%`, `ORB%`, `FT/FGA`, `Opp. TOV%`),
                 colors = scales::col_numeric(palette = c("red", "white", "green"), domain = NULL)) |>
      data_color(columns = vars(DRtg, `TOV%`, `Opp. eFG%`, `Opp. FT/FGA`, `Opp. ORB%`),
                 colors = scales::col_numeric(palette = c("green", "white", "red"), domain = NULL)) |>
      tab_spanner(
        label = "Record",
        columns = c(`W-L`, `PW-PL`, MOV, SOS)
      ) |>
      tab_spanner(
        label = "Ratings",
        columns = c(NRtg, ORtg, DRtg, Pace)
      ) |>
      tab_spanner(
        label = "Shooting",
        columns = c(`TS%`, `3PAr`)
      ) |>
      tab_spanner(
        label = "Team Four Factors",
        columns = c(`eFG%`, `ORB%`, `FT/FGA`, `TOV%`)
      ) |>
      tab_spanner(
        label = "Opponent Four Factors",
        columns = c(`Opp. eFG%`, `Opp. ORB%`, `Opp. FT/FGA`, `Opp. TOV%`)
      ) |>
      tab_style(style = cell_borders(sides = c("right"),  
                                     weight = px(2)), 
                locations = cells_body(columns = c(Tm, SOS, Pace, `3PAr`, `TOV%`, `Opp. TOV%`))) |>
      tab_style(style = cell_borders(sides = c("top"),  
                                     weight = px(3)), 
                locations = cells_body(rows = c(11, 21))) |>
      cols_width(
        Tm ~ px(70),
        everything() ~ px(80)
      ) |>
      opt_interactive(use_pagination = FALSE,
                      use_sorting = TRUE,
                      use_compact_mode = TRUE)
    
    return(table)
  })
  
  output$teamRatingsPlot <- renderPlot({
    plot <- team_ratings_graph(current_year)
    return(plot)
  })
  
  output$teamRatingsTable <- render_gt({
    table <- team_ratings_table(current_year)
    return(table)
  })
  
  output$playerPer36PercentilePlot <- renderPlot({
    plot <- player_per_36_percentile_graph(input$player, current_year)
    return(plot)
  })
}
