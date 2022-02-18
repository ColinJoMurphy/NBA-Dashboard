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

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("NBA Dashboard"),

    # Sidebar with a slider input for number of bins
    navlistPanel(widths = c(1, 2),
        "Teams",
        tabPanel("Standings",
                 withSpinner(DT::dataTableOutput('teamstandings'))),
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
                                                 'Washington Wizards'),
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
                                                 'Washington Wizards'),
                                    selected = ''))
                     ),
     
                fluidRow(
                    splitLayout(cellWidths = c(800, 800), 
                        plotOutput('teamFGdiagram1',
                                   height = 950),
                        plotOutput('teamFGdiagram2',
                                   height = 950)
                    ))
                 ),
        tabPanel('Defense',
                 h4('Choose any number of teams for which to query the database and return their defensive stats.'),
                    fluidRow(
                        column(8, 
                            selectInput('defenseteams',
                                    'Teams',
                                    multiple = TRUE,
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
                                                'Washington Wizards'),
                                    selected = '')
                     ),
                         column(1,
                            actionButton('getdefense', 'Compare')
                            ),
                     #),
                     # fluidRow(
                        column( 3,
                               plotOutput('defenseplot', 
                                          width =1500,
                                          height = 800)
                        )
                     )
                     
                 
        ),
                 
        "Players",
        tabPanel("Standings"),
        tabPanel("Top Shooters"), 
        tabPanel('Top Defenders'),
        "-----",
        tabPanel("Predictions")
    )

))
