library(shiny)
server <- function(input, output, session){
  sum_match_stats_long_numeric <- unlist(lapply(sum_match_stats_long, is.numeric))
  sum_match_stats_long_numeric <- colnames(sum_match_stats_long[sum_match_stats_long_numeric])
  stats <- reactive({
    if(input$surface_selector == "all") {
      stats <- as.list(round(sum_match_stats_long[sum_match_stats_long$player_id ==input$player_selector,sum_match_stats_long_numeric],2))
    } else
      stats <- as.list(round(sum_match_stats_long_by_tournament[sum_match_stats_long_by_tournament$player_id ==input$player_selector & sum_match_stats_long_by_tournament$tourney_surface == input$surface_selector,sum_match_stats_long_numeric],2))
  })
  
  new_surfaces <- reactive({
    choices <- unique(as.character(sum_match_stats_long_by_tournament[sum_match_stats_long_by_tournament$player_id == input$player_selector, ]$tourney_surface))
    names(choices) <- choices 
    choices["All Surfaces"] <- "all"
    choices
  })
  
  observeEvent(new_surfaces(), {
    updateSelectInput(session = session, inputId = "surface_selector", choices = new_surfaces())
  })
  
  output$serve_performance <-  renderText({
    stats()$serve_performance
  })
  
  output$first_serve_in_ratio <-  renderText({
    stats()$first_serve_in_ratio
  })
  
  output$first_serve_points_won_ratio <-  renderText({
    stats()$first_serve_points_won_ratio
  })
  
  output$serve_games_won_ratio <-  renderText({
    stats()$serve_games_won_ratio
  })
  
  output$second_serve_points_won_ratio <-  renderText({
    stats()$second_serve_points_won_ratio
  })
  
  output$average_aces_match <-  renderText({
    stats()$average_aces_match
  })
  
  output$average_double_faults <-  renderText({
    stats()$average_double_faults
  })
  
  output$return_performance <-  renderText({
    stats()$return_performance
  })
  
  output$first_serve_return_ratio <-  renderText({
    stats()$first_serve_return_ratio
  })
  
  output$second_serve_return_ratio <-  renderText({
    stats()$second_serve_return_ratio
  })
  
  output$break_points_converted_ratio <-  renderText({
    stats()$break_points_converted_ratio
  })
  
  output$return_games_won_ratio <-  renderText({
    stats()$return_games_won_ratio
  })
  
  output$under_pressure_performance <-  renderText({
    stats()$under_pressure_performance
  })
  
  output$break_points_saved_ratio <-  renderText({
    stats()$break_points_saved_ratio
  })
  
  output$break_points_converted_ratio2 <-  renderText({
    stats()$break_points_converted_ratio
  })
  
  output$tiebreaks_won_ratio <-  renderText({
    stats()$tiebreaks_won_ratio
  })
  
  output$deciding_sets_won_ratio <-  renderText({
    stats()$deciding_sets_won_ratio
  })
  
  tournaments_played <- reactive({
    tourneys <- tournaments_by_player[tournaments_by_player$player_id == input$player_selector,]
    if(input$surface_selector != "all"){
      tourneys <- tourneys[tourneys$tourney_surface == input$surface_selector,]
    }
    tourneys$tourney_position <- ifelse(tourneys$tourney_round_name == "Finals" & tourneys$match_result == "Won", "Champion",tourneys$tourney_round_name)
    tourneys$popup <- paste("<h3>", tourneys$tourney_name, "</h3><p>Position: <strong>", tourneys$tourney_position, "</strong></p><p>Surface: ", tourneys$tourney_surface, "</p>")
    random_lat_long_jitter <- runif(dim(tourneys)[1], -0.5, 0.5)
    tourneys$tourney_lat <- tourneys$tourney_lat + random_lat_long_jitter
    tourneys$tourney_long <- tourneys$tourney_long + random_lat_long_jitter
    tourneys
  })
  
  tennis_ball_icon <- iconList(
    tennis_ball = makeIcon("tennis_ball.png", "tennis_ball.png", 25, 25)
  )
  
  output$mapa <- renderLeaflet({
    leaflet(data=tournaments_played()) %>% addTiles() %>%
      addMarkers(lng = ~tourney_long, 
                 lat =  ~tourney_lat, 
                 popup = ~popup,
                 icon = tennis_ball_icon)
  })
  
  output$serve_graph <- renderPlot({
    serve_plot
  })
  
  output$return_graph <- renderPlot({
    return_plot
  })
  
  output$under_pressure_graph <- renderPlot({
    under_pressure_plot
  })
  
  player_info <- reactive({
    players_overview_2017[players_overview_2017$player_id == input$player_selector,]
  })
  
  output$player_height <- renderText({
    player_info()$height_cm
  })
  
  output$player_weight <- renderText({
    player_info()$weight_kg
  })
  
  output$player_handedness <- renderText({
    player_info()$handedness
  })
  
  output$player_backhand <- renderText({
    player_info()$backhand
  })
  
  output$player_pro_since <- renderText({
    player_info()$turned_pro
  })
  
  output$player_img <- renderText({
    paste0('<img class="player_img" src="https://ws.protennislive.com/api/Atp/PlayerImage?playerId=',player_info()$player_id,'">')
  })
  
}
