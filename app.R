ui <- 
  navbarPage(title=div(a(href="https://shiny.christopherteixeira.com",img(src="logo-high-transparent.png",height=50)),
                       "Fantasy Football Season Simulator"),
             windowTitle = "Fantasy Football Season Simulator",
             
             tabPanel("Roster Data",
                      fluidPage(theme = theme_ct,
                                useShinyjs()),
                      tags$head(
                        tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                      pageWithSidebar(
                        tags$h3("Input Values"),
                        sidebarPanel(width = 3,
                                     selectInput('serviceID', 
                                                 label='Select service:',
                                                 choices=c("ESPN"),
                                                 selected="ESPN") %>% shinyjs::disabled(),
                                     selectInput("yearID", 
                                                 label="Year:",
                                                 choices=seq(2019,2021),
                                                 selected = c(2021)) %>% disabled(),
                                     textInput("leagueID", "League ID:", value="367113688") %>% disabled(),
                                     selectInput("team_name",
                                                 label="Team:",
                                                 choices=c(""),
                                                 selected="") %>% shinyjs::disabled(),
                                     actionButton("getData", label = "Get league data", class = "btn-primary")
                                     
                                     
                                     # actionButton("getTeam", label = "Get team results", class = "btn-primary")
                        ),
                        mainPanel(
                          column(12, 
                                 # plotOutput("plot_expectedwins"),
                                 dataTableOutput("table_roster")
                          )
                        )
                      )),
             tabPanel("Simulation",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     tags$h3("Input values"),
                                     sliderInput("number_of_seasons",
                                                 label="Number of Simulated Seasons:",
                                                 value=10,
                                                 min=10,
                                                 max=100,
                                                 step=10,
                                                 round = 1,
                                                 ticks=F),
                                     numericInput("weeks_in_season",
                                                  label="Number of Weeks to Simulate:",
                                                  value=14,
                                                  min=10,
                                                  max=16,
                                                  step=1) %>% disabled()
                        ),
                        mainPanel(
                          plotOutput("plot_sim", height="600px")
                        )
                      )),
             tabPanel("Optimization", 
                      p("Future version will have an optimization component that will provide suggestions on who and when to pick up players.",style = "font-size:14px")
             ),
             tabPanel("About", 
                      p("This app utilizes a lot of work put into packages for scraping and analyzing fantasy football data from multiple providers. This app is still in development and will be updated periodically. For more information, see the ", 
                        a("github repository",href="#"),
                        " for this app to review the code and provide issues to fix.", 
                        style = "font-size:14px")
             )
  )

server <- 
  function(input, output, session) {
    
    rankings_stamp <- stamp("Based on Rankings as of Jan 1, 1999")
    
    observeEvent(input$getData, {
      conn <- espn_connect(input$yearID, input$leagueID)
      
      v$roster_data <- ffs_rosters(conn)
      v$roster_data$position <- factor(v$roster_data$pos, levels=c("QB","RB","WR","TE","DST","K"))
      
      shinyjs::enable("team_name")
      
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
             caption = paste("ffsimulator R package |",rankings_stamp(today()))) 
      
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
             caption = paste("ffsimulator R package |",rankings_stamp(today()))) 
        theme(legend.position = "none")
      
      p3 <- autoplot(df, type = "rank")
      
      p <- (p1 / p2) | p3
      return(p)
    })
  }

shinyApp(ui,server)