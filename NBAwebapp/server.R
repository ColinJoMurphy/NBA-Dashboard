
##### Required Packages #####
library(shiny)
library(data.table)
library(tidyr)
library(xml2)
library(rvest) 
library(DT)
library(ggplot2)
library(ggforce)
library(plotly)
library(stringr)
library(bslib)
library(DBI)
library(RPostgres)

##### Define Data Needed For The Scrape Function #####

# Build a table with all needed info for scraping data
webdata <- data.table('dataname' = c('player_data', 
                                     'team_advanced_data', 
                                     'team_shooting'),
                      
                      'urlsource' = c('https://www.basketball-reference.com/leagues/NBA_2022_totals.html',
                                      'https://www.basketball-reference.com/leagues/NBA_2022.html',
                                      'https://www.basketball-reference.com/leagues/NBA_2022.html'),
                      
                      'tableselector' = c('#totals_stats > tbody', 
                                          '#advanced-team > tbody', 
                                          '#shooting-team > tbody'),
                      
                      'headerselector' = c('#totals_stats > thead > tr',
                                           '#advanced-team > thead > tr:nth-child(2)',
                                           '#shooting-team > thead > tr:nth-child(2)')
)


# Scrape a table of team names and team ids from Wikipedia. Team ids will be added  
# to 'team_advanced_data' and will serve as the foreign key for 'player_data'
teamnames <- 'https://en.wikipedia.org/wiki/Wikipedia:WikiProject_National_Basketball_Association/National_Basketball_Association_team_abbreviations' %>%
    read_html() %>%
    html_elements(css = '#mw-content-text > div.mw-parser-output > table') %>%
    html_table(header = TRUE) %>%
    as.data.table
setnames(teamnames, names(teamnames), c('team_id', 'team'))

# Change a few abbreviations to match the scraped data
teamnames[team_id == 'PHX', team_id := 'PHO'][
    team_id == 'CHA', team_id := 'CHO'][
        team_id == 'BKN', team_id := 'BRK']

##### Scrape Function #####
scrapeandwrite.data <- function(webdata, teamnames){        
    scraped <- list()
    statkeys <- list()
    for (row in 1:nrow(webdata)){
        
        webtable <- webdata[row, urlsource] %>%
            read_html() %>%
            html_elements(css = webdata[row, tableselector]) %>%
            html_table() %>%
            as.data.table
        
        webtablekey <- webdata[row, urlsource] %>%
            read_html() %>%
            html_elements(css = webdata[row, headerselector]) %>%
            html_children()
        
        vars <- sapply(webtablekey, function(x) {x <- html_attr(x,'data-stat') %>%
            tolower()})
        setnames(webtable, names(webtable), tolower(vars))
        webtable[ , which(names(webtable) == 'dummy') := NULL]
        
        if (row == 1){
            webtable[, 
                     6:ncol(webtable) := lapply(.SD, as.numeric), 
                     .SDcols = (6:ncol(webtable))]
            
            # Remove rows of headers
            webtable <- webtable[player != 'Player']
        }
        
        if (row %in% c(2,3)){
            webtable[, team := str_remove(team, '[*]')]
        }
        
        percstats <- names(webtable) %like% 'pct'
        percstats <- names(webtable)[percstats]
        webtable[, 
                 eval(percstats) := lapply(.SD, 
                                             function(x) fifelse(x < 1, 
                                                                 x*100, 
                                                                 x)), 
                 .SDcols = percstats]        
        
        if (row == 2){
            webtable <- webtable[teamnames, on = 'team']
        }
        
        webtablekey <- sapply(webtablekey, function(x) {n <- html_attr(x,'aria-label')})
        k <- data.table('stat' = vars, 'descr' = webtablekey)
        k <- k[stat != 'dummy']
        
        tablename <- webdata[row, dataname]
        scraped[[tablename]] <- webtable
        keyname <- paste0(webdata[row, dataname], "_key")
        statkeys[[keyname]] <- k
    } 
    
    
    # Connect using database connection and write data to cloud database
    for (dname in webdata$dataname){
        
        # Store key name for key table corresponding to the current table name
        keyname <- names(statkeys)[str_detect(names(statkeys), dname)]
        
        # Open connection to database
        con <- DBI::dbConnect(
            Postgres(),
            user = 'postgres',
            password = 'test',
            host = '34.127.19.215',
            dbname = 'postgres'
        )
        
        # Write data and key to data base, overwriting the previous table of the same name
        dbWriteTable(con, dname, scraped[[dname]], overwrite = T)
        
        dbWriteTable(con, keyname, statkeys[[keyname]], overwrite = T)
    }
    # Close connection
    dbDisconnect(con)
}


##### Shiny Server #####

shinyServer(function(input, output, session) {

    # Set the scrapeandwrite.data() function to update the database every 24hrs 
    observe({
        invalidateLater(8.64*10^7)    
        scrapeandwrite.data(webdata, teamnames)                
    print('Data Scraped and Writen to Server')
    })
    
    ##### Team Standings Table #####
    output$teamstandings <- DT::renderDataTable({
        
        # Open the database connection
        con <- DBI::dbConnect(
            Postgres(),
            user = 'postgres',
            password = 'test',
            host = '34.127.19.215',
            dbname = 'postgres'
        )  
        
        # Pull team_advanced_data from database
        
        teams <- dbReadTable(con, 
                             'team_advanced_data') %>%
            setDT()
        
        # Close connection
        dbDisconnect(con)
        
        # Generate table
        DT::datatable(teams[,.(team, wins, losses)][order(wins, decreasing = TRUE)], 
                      options = list(pagelength = 10))
    })
   

    
    ##### Team FG% Diagram #####
    
    # Pull data from data base before generating figures so that only two database 
    # transactions are required, and data is not pulled every time team input is changed.
    
    # Open the database connection
    con <- DBI::dbConnect(
        Postgres(),
        user = 'postgres',
        password = 'test',
        host = '34.127.19.215',
        dbname = 'postgres'
    )
    d <- dbReadTable(con, 'team_shooting') %>% setDT()

    k <-  dbReadTable(con, 'team_shooting_key') %>% setDT()

    # Close connection
    dbDisconnect(con)
    
    
    # Team1 Field Goal Percentage diagram
    output$teamFGdiagram1 <- renderPlot({
        
        # Wait to render plot until team is chosen
        validate(
            need(input$shootingteam1, ''),
        )
        
        teamchoice1 <- input$shootingteam1
        # Separte FG%s and calculate FG% on heaves
        shots <- d[, c(2, 14:18, 27,28)
        ][, fg_heave := round(fg3_heave / fg3a_heave, 3)
        ][, c('fg3a_heave', 'fg3_heave') := NULL]
        
        t <- shots[team == teamchoice1]
        dists <- list(0:3, 3:10, 10:16, c(16:23,23.75), c(23.75, 24:26))
        shotdist <- list()
        for (n in 1:5){
            tindex <- n+1
            fgp <- data.table('distance' = dists[[n]], t[1, ..tindex], 'grouping' = names(t)[tindex])
            setnames(fgp, names(fgp), c('distance', 'fg.percent', 'grouping'))
            shotdist <- c(shotdist, list(fgp))
        }
        shotdist <- rbindlist(shotdist)
        shotdist[, fg.percent := as.factor(fg.percent)]
        shotdist[, fg.percent := paste0(fg.percent, '%')]
        distdescr <- c('0-3ft FG%', 
                       '3-10ft FG%', 
                       '10-16ft FG%', 
                       '16ft-3PT FG%',
                       '3PT FG%')
        
        plotdata <- shotdist[, 
                             .(min(distance), max(distance),(max(distance)-min(distance))/2 + min(distance) + 55 ,50), 
                             by = .(grouping, fg.percent)][, distdescr := distdescr]
        # Generate plot
         ggplot(plotdata, aes(V3, V4)) +
            
            coord_cartesian(xlim = c(50,101), ylim = c(26,76)) + 
            # Draw Court
            geom_rect(aes(xmin = 50, xmax = 100, ymin = 25, ymax = 75), fill = 'tan', colour = "black", size = 1, alpha = .7) + # Court Perimeter
            geom_arc(aes(x0 = 100, y0 = 50, r = 6, start = pi, end = 2*pi), size = 1, inherit.aes = FALSE) +  # Half court circle
            geom_rect(aes(xmin = 50, xmax = 69, ymin = 42, ymax = 58), fill = NA, colour = "black", size = 1) + # Key
            geom_arc(aes(x0 = 69, y0 = 50, r = 6, start = 0, end = pi), size = 1, inherit.aes = FALSE) +  # Top of the Key
            geom_rect(aes(xmin = 54, xmax = 54, ymin = 47, ymax = 53), fill = NA, color = 'black', size = 1) + # Backboard
            geom_circle(aes(x0 = 55, y0 = 50, r = .75), colour = "red", size = 1, inherit.aes = FALSE) + # Basket
            geom_arc(aes(x0 = 55, y0 = 50, r = 23.75, start = pi/8, end = 7*pi/8), fill = NA, size = 1, inherit.aes = FALSE) + # 3pt line
            geom_rect(aes(xmin = 50, xmax = 64, ymin = 28, ymax = 28), fill = NA, colour = "black", size = 1) + #Corner 3pt Line
            geom_rect(aes(xmin = 50, xmax = 64, ymin = 72, ymax = 72), fill = NA, colour = "black", size = 1) + #Corner 3pt Line
            geom_arc_bar(aes(x0 = 55, y0 = 50, r0 = V1, r = V2, start = pi/10, end = 9*pi/10, fill = fg.percent), size = 1, alpha = .7, radius = unit(1, 'mm'), inherit.aes = FALSE) +
            scale_fill_brewer(palette = 'YlOrRd') +
            geom_mark_circle(aes( label = distdescr,
                                  description = fg.percent,
                                  size = units(1, 'mm')
                                ),
                                position = position_nudge(x = 0, y = 0),
                                fill = NA,
                                color = 'transparent',
                                size = .1,
                                alpha = 1,
                                n = 100,
                                con.type = 'straight',
                                con.size = 1.5,
                                con.border = 'all',
                                label.fill = 'black',
                                label.margin = margin(c(2,2,10,59)),
                                label.buffer = unit(10, 'mm'),
                                label.colour = 'seashell',
                                
                                label.fontsize = 16,
                                con.cap = 0,
                                con.colour = 'black'
                                ) +
            coord_cartesian(xlim = c(50,100), ylim = c(26,76)) + 
            coord_flip() +
            # Theme
            theme(
                panel.background = element_rect(fill = "transparent",colour = NA),
                plot.margin = unit(c(0,0,0,0), "cm"),
                plot.title = element_text(size = 25, hjust = 0.5, vjust = 0),
                plot.background = element_rect(fill = "transparent", colour = NA),
                line = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text = element_blank(),
                legend.position = 'none',#c(.86, .80),
                legend.box = "vertical",
                legend.background = element_rect(fill = 'transparent'),
                legend.key.size = unit(10, 'mm'),
                legend.title = element_text(size = 25),
                legend.text = element_text(size = 18)
            ) +
             ggtitle(paste0(teamchoice1, ' FG% Across Ranges'))
        
        
    
    })
    
    # Team2 Field Goal Percentage diagram
    output$teamFGdiagram2 <- renderPlot({
        
        # Wait to render plot until team is chosen
        validate(
            need(input$shootingteam2, ''),
        )
        
        teamchoice2 <- input$shootingteam2
        
        # Separte FG%s and calculate FG% on heaves
        shots2 <- d[, c(2, 14:18, 27,28)
        ][, fg_heave := round(fg3_heave / fg3a_heave, 3)
        ][, c('fg3a_heave', 'fg3_heave') := NULL]
        
        t2 <- shots2[team == teamchoice2]
        dists <- list(0:3, 3:10, 10:16, c(16:23,23.75), c(23.75, 24:26))
        shotdist2 <- list()
        for (n in 1:5){
            tindex <- n+1
            fgp <- data.table('distance' = dists[[n]], t2[1, ..tindex], 'grouping' = names(t2)[tindex])
            setnames(fgp, names(fgp), c('distance', 'fg.percent', 'grouping'))
            shotdist2 <- c(shotdist2, list(fgp))
        }
        shotdist2 <- rbindlist(shotdist2)
        shotdist2[, fg.percent := as.factor(fg.percent)]
        shotdist2[, fg.percent := paste0(fg.percent, '%')]
        distdescr <- c('0-3ft FG%', 
                       '3-10ft FG%', 
                       '10-16ft FG%', 
                       '16ft-3PT FG%',
                       '3PT FG%')
        
        plotdata <- shotdist2[, 
                             .(min(distance), max(distance),(max(distance)-min(distance))/2 + min(distance) + 55 ,50), 
                             by = .(grouping, fg.percent)][, distdescr := distdescr]
        # Generate plot
        ggplot(plotdata, aes(V3, V4)) +
            
            coord_cartesian(xlim = c(50,101), ylim = c(26,76)) + 
            # Draw Court
            geom_rect(aes(xmin = 50, xmax = 100, ymin = 25, ymax = 75), fill = 'tan', colour = "black", size = 1, alpha = .7) + # Court Perimeter
            geom_arc(aes(x0 = 100, y0 = 50, r = 6, start = pi, end = 2*pi), size = 1, inherit.aes = FALSE) +  # Half court circle
            geom_rect(aes(xmin = 50, xmax = 69, ymin = 42, ymax = 58), fill = NA, colour = "black", size = 1) + # Key
            geom_arc(aes(x0 = 69, y0 = 50, r = 6, start = 0, end = pi), size = 1, inherit.aes = FALSE) +  # Top of the Key
            geom_rect(aes(xmin = 54, xmax = 54, ymin = 47, ymax = 53), fill = NA, color = 'black', size = 1) + # Backboard
            geom_circle(aes(x0 = 55, y0 = 50, r = .75), colour = "red", size = 1, inherit.aes = FALSE) + # Basket
            geom_arc(aes(x0 = 55, y0 = 50, r = 23.75, start = pi/8, end = 7*pi/8), fill = NA, size = 1, inherit.aes = FALSE) + # 3pt line
            geom_rect(aes(xmin = 50, xmax = 64, ymin = 28, ymax = 28), fill = NA, colour = "black", size = 1) + #Corner 3pt Line
            geom_rect(aes(xmin = 50, xmax = 64, ymin = 72, ymax = 72), fill = NA, colour = "black", size = 1) + #Corner 3pt Line
            geom_arc_bar(aes(x0 = 55, y0 = 50, r0 = V1, r = V2, start = pi/10, end = 9*pi/10, fill = fg.percent), size = 1, alpha = .7, radius = unit(1, 'mm'), inherit.aes = FALSE) +
            scale_fill_brewer(palette = 'YlOrRd') +
            geom_mark_circle(aes( label = distdescr,
                                  description = fg.percent,
                                  size = units(1, 'mm')
            ),
            position = position_nudge(x = 0, y = 0),
            fill = NA,
            color = 'transparent',
            size = .1,
            alpha = 1,
            n = 100,
            con.type = 'straight',
            con.size = 1.5,
            con.border = 'all',
            label.fill = 'black',
            label.margin = margin(c(2,2,10,59)),
            label.buffer = unit(10, 'mm'),
            label.colour = 'seashell',
            
            label.fontsize = 16,
            con.cap = 0,
            con.colour = 'black'
            ) +
            coord_cartesian(xlim = c(50,100), ylim = c(26,76)) + 
            coord_flip() +
            # Theme
            theme(
                panel.background = element_rect(fill = "transparent",colour = NA),
                plot.margin = unit(c(0,0,0,0), "cm"),
                plot.title = element_text(size = 25, hjust = 0.5, vjust = 0),
                plot.background = element_rect(fill = "transparent", colour = NA),
                line = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text = element_blank(),
                legend.position = 'none',
                legend.box = "vertical",
                legend.background = element_rect(fill = 'transparent'),
                legend.key.size = unit(10, 'mm'),
                legend.title = element_text(size = 25),
                legend.text = element_text(size = 18)
            ) +
            ggtitle(paste0(teamchoice2, ' FG% Across Ranges'))
        
    })
    
    
    
##### Team Defense #####    
    
    # Wait until at least one team is selected before generating plot
    output$defenseplot <- renderPlot({
        validate(need(input$defenseteams, ''))
        
        # Store team names and number
        teams <- input$defenseteams
        nteams <- length(teams)
        
        # Build query
        q <- teams %>% 
                str_c('team = \'', ., '\'', collapse = ' OR ') %>%
                paste0('SELECT team, opp_efg_pct, opp_tov_pct, drb_pct FROM team_advanced_data WHERE ',
                       .,
                       ';')

        # Open database connection
        con <- DBI::dbConnect(
            Postgres(),
            user = 'postgres',
            password = 'test',
            host = '34.127.19.215',
            dbname = 'postgres'
        ) 
        
        # Execute query and format result table
        data <- dbGetQuery(con, q) %>%
            setDT()
        
        # Close connection
        dbDisconnect(con)
        
        setnames(data, names(data), tolower(names(data)))
        data[, opp_efg_pct := opp_efg_pct]
        data <- melt.data.table(data, id.vars = 'team')
    
            
            
             ggplot(data, aes(variable, value, fill = team)) +
                 geom_col(position = 'dodge') +
                 labs(x = 'Defensive Stat',
                      y = 'Percent') +
                 scale_fill_discrete(name = 'Team') +
                 scale_x_discrete(labels = c('Opponent Effective Field Goal Percentage', 
                                             'Opponent Turn Over Percentage',
                                             'Defensive Rebound Percentage')) +
                 geom_text(aes(label = paste0(value, '%')),
                           vjust = -.5, position = position_dodge(width = .9),
                           size = 5,
                           color = 'black') +
                 theme_classic() +
                 theme(
                     legend.key.width = unit(5, 'mm'),
                     legend.text = element_text(size = 15),
                     legend.title = element_text(size = 20),
                     plot.title = element_text(size = 22, hjust = .5),
                     axis.text = element_text(size = 15),
                     axis.title.x = element_text(size = 18),
                     axis.title.y = element_text(size = 18),
                     axis.text.x = element_text(angle = 15, hjust = 1)
                 ) +
                 ggtitle('Comparing Team Defense')

     
    
    })
    
    
    output$positionplot <- renderPlotly({
        
        # Require input so user doesn't see an error
        validate(
            need(input$positionstat, ''),
        )
        
        # Store stat
        inputstat <- input$positionstat
        
        # Query and format
        con <- DBI::dbConnect(
            Postgres(),
            user = 'postgres',
            password = 'test',
            host = '34.127.19.215',
            dbname = 'postgres'
        ) 
        
        key <- dbReadTable(con, 'player_data_key') %>%
            setDT()
        
        data <- dbReadTable(con, 'player_data') %>%
            setDT()
        
        # Close connection
        dbDisconnect(con)
        
        data <- data[data[, player != 'Player']]
        data[, 
             pos := fifelse(nchar(pos)>2, 
                            substr(pos, 1, (nchar(pos) - 3)), 
                            pos)
             ]
        
        # Convert stats to numeric, '' entries introduce NAs
        data[, 6:30] <- data[, lapply(.SD, as.numeric), .SDcols = 6:30]
        plotstat <- key[descr == inputstat, stat]
        
        # Generate plot
           g <- ggplot(data, aes(pos, round(get(plotstat), 2))) +
                geom_boxplot(aes(fill = pos), na.rm = TRUE) +
                labs(x = 'Position',
                     y = eval(inputstat)) +
                scale_fill_discrete(name = 'Position',
                                     breaks = c('C', 'PF', 'SF','PG', 'SG'),
                                     labels = c('Center',
                                                'Power Forward',
                                                'Small Forward',
                                                'Point Guard',
                                                'Shooting Guard')) +
                theme_bw() +
                theme(
                    legend.key.width = unit(5, 'mm'),
                    legend.text = element_text(size = 15),
                    legend.title = element_text(size = 20),
                    plot.title = element_text(size = 22, hjust = .5),
                    axis.text = element_text(size = 15),
                    axis.title.x = element_text(size = 18),
                    axis.title.y = element_text(size = 18),
                    axis.text.x = element_text(angle = 0, hjust = 1)
                ) +
                ggtitle('Comparing Positions')
                
                ggplotly(g,
                         tooltip = c('y', 'fill')
                         )
        })
    
    
    output$playertable <- renderDataTable({
        
        # Require input so user doesn't see an error
        validate(
            need(!is.null(input$offensestat) | !is.null(input$defensestat), ''),
        )
        
        # Combine chosen stats
        chosenstats <- c(input$defensestat, input$offensestat)
        
        #  connection to database
        con <- DBI::dbConnect(
            Postgres(),
            user = 'postgres',
            password = 'test',
            host = '34.127.19.215',
            dbname = 'postgres'
        ) 
        
        # Pull the key table
        key <- dbReadTable(con, 'player_data_key')
        setDT(key)
        
        # Extract the STAT associated with the chosen stat descriptions
        chosenstats <- key[descr %in% chosenstats, stat]
        
        # Generate main query
        q <- chosenstats %>%
                str_to_lower(.) %>%
                str_c('ROUND(AVG(', ., ')::numeric, 2) AS avg_',.) %>%
                paste0(., collapse = ', ') %>%
                paste0(' SELECT ', ., ' FROM player_data GROUP BY player;')
        
        # Execute query
        data <- dbGetQuery(con, q) %>%
            setDT()
        
        # Close connection
        dbDisconnect(con)
        
        # Generate table
        DT::datatable(data)
    })
    
    
    ##### Top Players on Top Teams #####
    
    # Open database connection
    con <- DBI::dbConnect(
        Postgres(),
        user = 'postgres',
        password = 'test',
        host = '34.127.19.215',
        dbname = 'postgres'
    ) 
    
    # Pull the player key table
    pkey <- dbReadTable(con, 'player_data_key')
    setDT(pkey)
    
    # Pull the team key table
    tkey <- dbReadTable(con, 'team_advanced_data_key')
    setDT(tkey)
    
    # Close connection
    dbDisconnect(con)
    
    # Set the chart to generate after the submit button is clicked
   output$topteams_topplayers <- renderDataTable({
        
       # Wait until the button is clicked
        validate(
           need(input$teamstat, ''),
           need(input$playerstat, '')
        )
       
        # Store user chosen team and player stats
        tstat <- input$teamstat
        pstat <- input$playerstat
        
        # Use keys to get column names associated with stats
        tstat <- tkey[descr == tstat, stat]
        pstat <- pkey[descr == pstat, stat]
        
        # Store which stats should be ordered in ascending order, i.e. lower stat is better
        tlowstat <- c('losses')
        plowstat <- c('tov', 'pf')
        
        # Store which direction stat should be sorted
        torder <- fifelse(tstat %in% tlowstat, 'ASC', 'DESC')
        porder <- fifelse(pstat %in% plowstat, 'ASC', 'DESC')
        
        # Generate SQL query
        q <- paste0('SELECT * FROM (SELECT team_id, ',
                    pstat,
                    ', ROW_NUMBER() OVER (PARTITION BY team_id ORDER BY ',
                    pstat,
                    ' ',
                    porder,
                    ' NULLS LAST) AS rn FROM player_data) p JOIN (SELECT team_id, ',
                    tstat,
                    ' FROM team_advanced_data ORDER BY ',
                    tstat,
                    ' ',
                    torder,
                    ' LIMIT 5) t USING(team_id) WHERE p.rn <= 5 ORDER BY team_id, ',
                    tstat, 
                    ' ',
                    torder, 
                    ', ',
                    pstat,
                    ' ',
                    porder,
                    ';'
                    )
        
        # Open database connection
        con <- DBI::dbConnect(
            Postgres(),
            user = 'postgres',
            password = 'test',
            host = '34.127.19.215',
            dbname = 'postgres'
        ) 
        
        # Query database
        resulttable <- dbGetQuery(con, q)
        setDT(resulttable)
        
        # Close database connection
        dbDisconnect(con)
        
        # Generate table
        DT::datatable(resulttable, options = list(paging = FALSE) )
        

    })
   
    
   session$onSessionEnded(stopApp)
})

                               