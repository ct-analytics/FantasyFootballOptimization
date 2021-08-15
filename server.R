
function(input, output, session) {
  number_of_seasons <- 10
  weeks_in_season <- 14

  rankings_stamp <- stamp("Based on Rankings as of Jan 1, 1999")
  
  # conn <- espn_connect(2021, 367113688)
  # rosters <- ffs_rosters(conn)
  # rosters$position <- factor(rosters$pos, levels=c("QB","RB","WR","TE","DST","K"))
  
  observeEvent(input$getData, {
    # sim_data <- reactive({
    #   conn <- espn_connect(input$yearID, input$leagueID)
    #   espn_sim <- ff_simulate(conn = conn, n_seasons = number_of_seasons, n_weeks = weeks_in_season)  
    #   espn_sim$summary_season 
    # })
    # 
    # roster_data <- reactive({
    #   conn <- espn_connect(input$yearID, input$leagueID)
    #   rosters <- ffs_rosters(conn)
    #   rosters$position <- factor(rosters$pos, levels=c("QB","RB","WR","TE","DST","K"))
    #   rosters
    # })
    
    conn <- espn_connect(input$yearID, input$leagueID)
    espn_sim <- ff_simulate(conn = conn, n_seasons = number_of_seasons, n_weeks = weeks_in_season)
    v$sim_data <- espn_sim$summary_season

    v$roster_data <- ffs_rosters(conn)
    v$roster_data$position <- factor(v$roster_data$pos, levels=c("QB","RB","WR","TE","DST","K"))
    
    updateSelectInput(session, "team_name",
                      label = "Team:",
                      choices = v$roster_data %>% select(franchise_name) %>% distinct(),
                      selected = v$roster_data %>% select(franchise_name) %>% distinct() %>% slice(1) %>% pull())
  })

  v <- reactiveValues(
    sim_data = NULL,
    roster_data = NULL,
    team_data = NULL,
    sim_data_team = NULL
  )
  
  # observeEvent(input$getTeam, {
  #   v$team_data <- v$roster_data %>% 
  #     filter(franchise_name==input$team_name) %>% 
  #     select(player_name,team,status,position,eligible_pos) %>% 
  #     arrange(position)
  #   v$sim_data_team <- v$sim_data %>% 
  #     filter(franchise_name==input$team_name)
  # })
  team_data <- reactive({
    if (is.null(v$roster_data)) return()
    v$roster_data %>% 
      filter(franchise_name==input$team_name) %>% 
      select(player_name,team,status,position,eligible_pos) %>% 
      arrange(position)
  })
  sim_data_team <- reactive({
    if (is.null(v$sim_data)) return()
    v$sim_data %>% 
      filter(franchise_name==input$team_name)
  })
  
  output$plot_expectedwins <- renderPlot({
    if (is.null(sim_data_team())) return()
    
    sim_data_team() %>%
      select(franchise_name, h2h_wins) %>%
      ggplot(aes(x=h2h_wins)) +
      geom_histogram(stat="count") +
      scale_x_continuous(breaks = seq(0,14), limits = c(0,14)) +
      xlab("Head to Head Wins in a Season") +
      ylab("Number of Simulated Seasons") +
      labs(title = paste("Season win totals for",input$team_name),
           subtitle = paste("Expected number of wins after simulating ",number_of_seasons," ",weeks_in_season,"-week seasons.",sep=""),
           caption = paste("ffsimulator R package |",rankings_stamp(today()))) +
      theme_hc()
  })
  
  output$table_roster <- renderDataTable({
    if (is.null(team_data())) return()
    
    datatable(team_data(),
              rownames = FALSE,
              colnames = c("Name", "Team", "Status", "Position", "Eligible Positions"),
              filter = "none",
              options = list("searching"=F,
                             pageLength=16,
                             ordering=F,
                             rowReorder=F,
                             lengthChange=F)
    )
  })
}
