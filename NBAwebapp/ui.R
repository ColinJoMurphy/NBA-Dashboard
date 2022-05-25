#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinycssloaders)
library(shiny)
library(plotly)
library(bslib)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    theme = bs_theme(bootswatch = 'yeti'),
    # Application title
    titlePanel('2021-22 NBA Season Dashboard' ),
    
    mainPanel(
    navlistPanel(widths = c(2, 10),
        "Teams",
        tabPanel("Standings",
                 withSpinner(DT::dataTableOutput('teamstandings'), 
                             image = 'https://media.giphy.com/media/glXhanMgKLLRWir9MR/giphy.gif',
                             image.height = 180)),
        tabPanel("Shooting",
                 h5('Choose two teams to compare their field goal percentages at different ranges.'),
                 fluidRow(
                    column(6,
                        selectInput('shootingteam1',
                                    'Team 1',
                                    choices = c('',
                                                 'Atlanta Hawks',
                                                 'Boston Celtics',
                                                 'Charlotte Hornets',
                                                 'Chicago Bulls',
                                                 'Cleveland Cavaliers',
                                                 'Dallas Mavericks',
                                                 'Denver Nuggets',
                                                 'Detroit Pistons',
                                                 'Golden State Warriors',
                                                 'Houston Rockets',
                                                 'Indiana Pacers',
                                                 'Los Angeles Clippers',
                                                 'Los Angeles Lakers',
                                                 'Memphis Grizzlies',
                                                 'Miami Heat',
                                                 'Milwaukee Bucks',
                                                 'Minnesota Timberwolves',
                                                 'New Orleans Pelicans',
                                                 'New York Knicks',
                                                 'Brooklyn Nets',
                                                 'Oklahoma City Thunder',
                                                 'Orlando Magic',
                                                 'Philadelphia 76ers',
                                                 'Phoenix Suns',
                                                 'Portland Trail Blazers',
                                                 'Sacramento Kings',
                                                 'Toronto Raptors',
                                                 'Utah Jazz',
                                                 'Washington Wizards'
                                                ),
                                    selected = '')
                    ), 
                     column(6,
                        selectInput('shootingteam2',
                                    'Team 2',
                                    choices = c('',
                                                 'Atlanta Hawks',
                                                 'Boston Celtics',
                                                 'Charlotte Hornets',
                                                 'Chicago Bulls',
                                                 'Cleveland Cavaliers',
                                                 'Dallas Mavericks',
                                                 'Denver Nuggets',
                                                 'Detroit Pistons',
                                                 'Golden State Warriors',
                                                 'Houston Rockets',
                                                 'Indiana Pacers',
                                                 'Los Angeles Clippers',
                                                 'Los Angeles Lakers',
                                                 'Memphis Grizzlies',
                                                 'Miami Heat',
                                                 'Milwaukee Bucks',
                                                 'Minnesota Timberwolves',
                                                 'New Orleans Pelicans',
                                                 'New York Knicks',
                                                 'Brooklyn Nets',
                                                 'Oklahoma City Thunder',
                                                 'Orlando Magic',
                                                 'Philadelphia 76ers',
                                                 'Phoenix Suns',
                                                 'Portland Trail Blazers',
                                                 'Sacramento Kings',
                                                 'Toronto Raptors',
                                                 'Utah Jazz',
                                                 'Washington Wizards'
                                                ),
                                    selected = ''))
                     ),
     
                fluidRow(
                    splitLayout(cellWidths = c(600, 600), 
                        withSpinner(plotOutput('teamFGdiagram1',
                                                height = 700),
                                    image = 'https://media.giphy.com/media/glXhanMgKLLRWir9MR/giphy.gif',
                                    image.height = 180),
                        withSpinner(plotOutput('teamFGdiagram2',
                                                height = 700),
                                    image = 'https://media.giphy.com/media/glXhanMgKLLRWir9MR/giphy.gif',
                                    image.height = 180)
                    ))
                 ),
        tabPanel('Defense',
                 h4('Choose any number of teams to compare their defensive stats.'),
                    fluidRow(
                        column(2, 
                            selectInput('defenseteams',
                                    'Teams',
                                    multiple = TRUE,
                                    choices = c(
                                                'Atlanta Hawks',
                                                'Boston Celtics',
                                                'Charlotte Hornets',
                                                'Chicago Bulls',
                                                'Cleveland Cavaliers',
                                                'Dallas Mavericks',
                                                'Denver Nuggets',
                                                'Detroit Pistons',
                                                'Golden State Warriors',
                                                'Houston Rockets',
                                                'Indiana Pacers',
                                                'Los Angeles Clippers',
                                                'Los Angeles Lakers',
                                                'Memphis Grizzlies',
                                                'Miami Heat',
                                                'Milwaukee Bucks',
                                                'Minnesota Timberwolves',
                                                'New Orleans Pelicans',
                                                'New York Knicks',
                                                'Brooklyn Nets',
                                                'Oklahoma City Thunder',
                                                'Orlando Magic',
                                                'Philadelphia 76ers',
                                                'Phoenix Suns',
                                                'Portland Trail Blazers',
                                                'Sacramento Kings',
                                                'Toronto Raptors',
                                                'Utah Jazz',
                                                'Washington Wizards'
                                                ),
                                    selectize = TRUE
                                    )
                                )
                    ),

                        fluidRow(column(6, offset = 4,
                                 withSpinner(
                                     plotOutput('defenseplot', 
                                                        width =1200,
                                                        height = 600
                                                      ),
                                             image = 'https://media.giphy.com/media/glXhanMgKLLRWir9MR/giphy.gif',
                                             image.height = 180)
                             )
                     )
                     
                 
        ),
                 
        'Players',
        tabPanel('Stats by Postion',
                 h4('Select a statistic to compare across positions'),
                 fluidRow(
                     column(4, 
                            selectInput('positionstat',
                                        'Statistic',
                                        choices = c('',
                                                    'Field Goal Percentage',
                                                    '3-Point Field Goal Percentage',
                                                    '2-Point Field Goal Percentage',
                                                    'Effective Field Goal Percentage',
                                                    'Free Throw Percentage',
                                                    'Offensive Rebounds',
                                                    'Defensive Rebounds',
                                                    'Total Rebounds',
                                                    'Assists',
                                                    'Steals',
                                                    'Blocks',
                                                    'Turnovers'
                                                    ),
                                        selected = '')
                            ),
                     ),
                  fluidRow(
                        # column( 8,
                             withSpinner(plotlyOutput('positionplot', 
                                                      width =1200,
                                                      height = 600),
                                         image = 'https://media.giphy.com/media/glXhanMgKLLRWir9MR/giphy.gif',
                                         image.height = 180)
                              # )
                           )
                 ),
        tabPanel('Shooting and Defending',
                
                 h4('Select which player stats you would like to see'),
                 fluidRow(column(10,
                            checkboxGroupInput('offensestat',
                                               'Offensive Stats',
                                               choices = c('Field Goal Percentage',
                                                           '3-Point Field Goal Percentage',
                                                           '2-Point Field Goal Percentage',
                                                           'Effective Field Goal Percentage',
                                                           'Free Throw Percentage',
                                                           'Assits',
                                                           'Offensive Rebounds'
                                                           ),
                                                inline = TRUE
                                              )
                                        
                            ),
                         ),
                 fluidRow(column(10,
                            checkboxGroupInput('defensestat',
                                               'Defensive Stats',
                                               choices = c('Defensive Rebounds',
                                                           'Steals',
                                                           'Blocks',
                                                           'Turnovers'
                                                            ),
                                               inline = TRUE
                                               )
                                )

                    
                          ),
                
                 fluidRow(column(12,
                                 withSpinner(DT::dataTableOutput('playertable'),
                                             image = 'https://media.giphy.com/media/glXhanMgKLLRWir9MR/giphy.gif',
                                             image.height = 180)
                                )
                 
                        )
    ),
    
    tabPanel('Top Players On Top Teams',
             h4('Select a player stat and team stat to generate a table of the top five teams and their top five players'),
             fluidRow(column(4, 
                             selectInput('teamstat',
                                         'Team Stat',
                                         choices = c('',
                                                     'Wins',
                                                     'Losses',
                                                     'Margin of Victory',
                                                     'Offensive Rating',
                                                     'Defensive Rating',
                                                     'Pace Factor',
                                                     'Free Throw Attempt Rate',
                                                     '3-Point Attempt Rate',
                                                     'Effective Field Goal Percentage',
                                                     'Offensive Rebound Percentage',
                                                     'Opponent Turnover Percentage',
                                                     'Defensive Rebound Percentage'),
                                         selected = ''
                             )
             ),
                 
                    column(4, 
                             selectInput('playerstat',
                                         'Player Stat',
                                         choices = c('',
                                                     'Games Started',
                                                     'Minutes Played',
                                                     'Field Goals',
                                                     'Field Goal Percentage',
                                                     '3-Point Field Goal Percentage',
                                                     '2-Point Field Goal Percentage',
                                                     'Free Throw Percentage',
                                                     'Offensive Rebounds',
                                                     'Defensive Rebounds',
                                                     'Total Rebounds',
                                                     'Assists',
                                                     'Steals',
                                                     'Blocks',
                                                     'Turnovers',
                                                     'Personal Fouls',
                                                     'Points'),
                                         selected = ''
                                         )
                             )

                      ),
             fluidRow(column(12,
                             withSpinner(DT::dataTableOutput('topteams_topplayers'),
                                         image = 'https://media.giphy.com/media/glXhanMgKLLRWir9MR/giphy.gif',
                                         image.height = 180)
                             )
                     )
            
            
            )


)
)
)
)
