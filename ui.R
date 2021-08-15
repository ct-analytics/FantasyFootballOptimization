
navbarPage("Fantasy Football Season Simulator",
           
           tabPanel("Roster Data",
                    fluidPage(theme = shinytheme("flatly"),
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
                                   actionButton("getData", label = "Get league data", class = "btn-primary"),
                                   selectInput("team_name",
                                               label="Team:",
                                               choices=c(""),
                                               selected="")
                                   
                                   # actionButton("getTeam", label = "Get team results", class = "btn-primary")
                      ),
                      mainPanel(
                        column(12, 
                               # plotOutput("plot_expectedwins"),
                               dataTableOutput("table_roster")
                        )
                      )
                    )),
           tabPanel("Simulation Data",
                    sidebarLayout(
                      sidebarPanel(width = 3,
                                   tags$h3("Input values"),
                                   sliderInput("number_of_seasons",
                                               label="Number of Simulated Seasons:",
                                               value=50,
                                               min=1,
                                               max=100,
                                               # step=10,
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
                        plotOutput("plot_expectedwins"),
                        plotOutput("plot_weekly_points")
                      )
                    )),
           tabPanel("About", 
                    p("Test ", a("PES Data Base", href="http://pesdb.net/", target="_blank"),".",style = "font-size:14px"),
                    )
)