
function(input, output, session) {

  rankings_stamp <- stamp("Based on Rankings as of Jan 1, 1999")
  
  observeEvent(input$getData, {
    conn <- espn_connect(input$yearID, input$leagueID)

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
  
  team_data <- reactive({
    if (is.null(v$roster_data)) return()
    v$roster_data %>% 
      filter(franchise_name==input$team_name) %>% 
      select(player_name,team,status,position,eligible_pos) %>% 
      arrange(position)
  })
  sim_data <- reactive({
    progress <- Progress$new(session, min=1, max=4)
    on.exit(progress$close())
    
    progress$set(message = 'Running simulation...',
                 detail = 'this may take a while...')
    
    conn <- espn_connect(input$yearID, input$leagueID)
    
    if (is.null(v$roster_data)) return()
    ff_simulate(conn = conn, 
                n_seasons = input$number_of_seasons, 
                n_weeks = input$weeks_in_season,
                injury_model = "simple",
                best_ball = T,
                seed = 4321)
    
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
                             lengthChange=F,
                             info=F,
                             paging=F)
    )
  })
  
  output$plot_sim <- renderPlot({
    if (is.null(sim_data())) return()
    df <- sim_data()
    
    p1 <- df$summary_season %>%
      filter(franchise_name==input$team_name) %>%
      select(franchise_name, h2h_wins) %>%
      ggplot(aes(x=h2h_wins)) +
      geom_histogram(stat="count") +
      scale_x_continuous(breaks = seq(0,14), limits = c(0,14)) +
      xlab("Head to Head Wins in a Season") +
      ylab("Number of Simulated Seasons") +
      labs(title = paste("Season win totals for",input$team_name),
           subtitle = paste("Expected number of wins after simulating ",input$number_of_seasons," ",input$weeks_in_season,"-week seasons.",sep=""),
           caption = paste("ffsimulator R package |",rankings_stamp(today()))) +
      theme_hc()

    p2 <- df$summary_week %>%
      filter(franchise_name==input$team_name) %>%
      select(franchise_name,team_score,result) %>%
      ggplot(aes(y=result, x=team_score, fill=result, color=result)) +
      geom_jitter(color="black", size=2, alpha=0.2) +
      geom_violin(alpha=0.75) +
      xlab("Simulated Team Score per Week") +
      ylab("Weekly Result") +
      labs(title = paste("Weekly point totals for",input$team_name),
           subtitle = paste("Expected weekly point total after simulating ",input$number_of_seasons," ",input$weeks_in_season,"-week seasons.",sep=""),
           caption = paste("ffsimulator R package |",rankings_stamp(today()))) +
      theme_hc() +
      theme(legend.position = "none")
    
    p3 <- autoplot(df, type = "rank")
    
    p <- (p1 / p2) | p3
    return(p)
  })
}
