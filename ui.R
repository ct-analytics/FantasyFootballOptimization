navbarPage("Fantasy Football Season Simulator",
           tabPanel("Simulated Data",
                    fluidPage(theme = shinytheme("flatly")),
                    tags$head(
                      tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                    pageWithSidebar(
                      headerPanel('Apply filters'),
                      sidebarPanel(width = 3,
                                   selectInput('serviceID', 
                                               label='Select service:',
                                               choices=c("ESPN"),
                                               selected="ESPN"),
                                   selectInput("yearID", 
                                               label="Year:",
                                               choices=seq(2019,2021),
                                               selected = c(2021)),
                                   textInput("leagueID", "League ID:", value="367113688"),
                                   actionButton("getData", label = "Get league data", class = "btn-primary"),
                                   selectInput("team_name",
                                               label="Team:",
                                               choices=c(""),
                                               selected="")
                                   
                                   # actionButton("getTeam", label = "Get team results", class = "btn-primary")
                      ),
                      mainPanel(
                        column(12, 
                               plotOutput("plot_expectedwins"),
                               dataTableOutput("table_roster")
                        )
                      )
                    )),
           tabPanel("About", 
                    p("Test ", a("PES Data Base", href="http://pesdb.net/", target="_blank"),".",style = "font-size:14px"),
                    )
)