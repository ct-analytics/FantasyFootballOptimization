
navbarPage(title=div(img(src="icon.png",style="margin-top: -14px; padding-right:10px;padding-bottom:10px",height=50),"Fantasy Football Season Simulator"),
           windowTitle = "Fantasy Football Season Simulator",
           
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
