
##### Required Packages #####
library(shiny)
library(data.table)
library(tidyr)
library(RODBC)
library(xml2)
library(rvest) 
library(DT)
library(ggplot2)
library(ggforce)
library(plotly)
library(stringr)
library(bslib)

##### Define Web Data #####


webdata <- data.table('dataname' = c('PLAYER_DATA', 
                                     'TEAM_ADVANCED_DATA', 
                                     'TEAM_SHOOTING'),
                      
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

##### Scrape Function #####
scrapeandwrite.data <- function(webdata){        
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
        setnames(webtable, names(webtable), toupper(vars))
        webtable[ , which(names(webtable) == 'DUMMY') := NULL]
        
        if (row == 1){
            webtable[, 
                     6:ncol(webtable) := lapply(.SD, as.numeric), 
                     .SDcols = (6:ncol(webtable))]
        }
        
        if (row %in% c(2,3)){
            webtable[, TEAM := str_remove(TEAM, '[*]')]
        }
        
        percstats <- names(webtable) %like% 'PCT'
        percstats <- names(webtable)[percstats]
        webtable[, 
                 eval(percstats) := lapply(.SD, 
                                             function(x) fifelse(x < 1, 
                                                                 x*100, 
                                                                 x)), 
                 .SDcols = percstats]        
        
        
        webtablekey <- sapply(webtablekey, function(x) {n <- html_attr(x,'aria-label')})
        k <- data.table('STAT' = vars, 'DESCR' = webtablekey)
        k <- k[STAT != 'DUMMY']
        
        tablename <- webdata[row, dataname]
        scraped[[tablename]] <- webtable
        keyname <- paste0(webdata[row, dataname], "_KEY")
        statkeys[[keyname]] <- k
    } 
    
    
    
    ##### Write Data To Db2 #####
    # Connect using RODBC package
    con <- odbcConnect('DB2', 'fjs08406', 'hVJHsH2WnvB9H3Bm')
    
    for (dname in webdata$dataname){
        dropname <- paste0('FJS08406.', dname)
        sqlDrop(con, sqtable = dropname)
        sqlSave(con, scraped[[dname]], tablename = dname, rownames = FALSE)
        dropkeyname <- paste0('FJS08406.', dname, "_KEY")
        keyname <- paste0(dname, "_KEY")
        sqlDrop(con, sqtable = keyname)
        sqlSave(con, statkeys[[keyname]], tablename = keyname, rownames = FALSE)
    }
    closeAllConnections()
}



shinyServer(function(input, output, session) {

    
    # autoInvalidate <- reactiveTimer(600000)
    # observe({
    #     # Invalidate and re-execute this reactive expression every time the
    #     # timer fires.
    #     autoInvalidate()
    #     
    #     # Do something each time this is invalidated.
    #     # The isolate() makes this observer _not_ get invalidated and re-executed
    #     # when input$n changes.
    #     scrapeandwrite.data(webdata)
    #     print('Data Scraped and Writen to Server')
    # })
    
    
 
    # Set the scrapeandwrite.data() function to invalidate so the data is updated once every 24 hours
    reactive({
        invalidateLater(600000)    # refresh the report every 600k milliseconds (600 seconds)
    scrapeandwrite.data(webdata)                # call our function from above
    print('Data Scraped and Writen to Server')
    })
    
    
    # Pull TEAM_ADVANCED_DATA from db2
    con <- odbcConnect('DB2', 'fjs08406', 'hVJHsH2WnvB9H3Bm')
    teams <- sqlFetch(con, 
                      sqtable = 'FJS08406.TEAM_ADVANCED_DATA', 
                      rownames = FALSE) %>%
                setDT()
    closeAllConnections()
    setnames(teams, names(teams), tolower(names(teams)))
    
    
    ##### Team Standings Table #####
    output$teamstandings <- DT::renderDataTable({
        DT::datatable(teams[,.(team, wins, losses)][order(wins, decreasing = TRUE)], 
                      options = list(pagelength = 10))
    })
   
    
    # Pull TEAM_SHOOTING from db2
    con <- odbcConnect('DB2', 'fjs08406', 'hVJHsH2WnvB9H3Bm')
    teamshooting <- sqlFetch(con, 
                      sqtable = 'FJS08406.TEAM_SHOOTING', 
                      rownames = FALSE) %>%
        setDT()
    closeAllConnections()
    
    ##### Team FG% Diagram #####
    con <- odbcConnect('DB2', 'fjs08406', 'hVJHsH2WnvB9H3Bm')
    d <- sqlFetch(con, 'TEAM_SHOOTING') %>% setDT()
    setnames(d, names(d), tolower(names(d)))
    
    k <-  sqlFetch(con, 'TEAM_SHOOTING_KEY') %>% setDT()
    setnames(k, names(k), tolower(names(k)))
    
    closeAllConnections()
    
    
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
            #Court
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
            #Theme
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
            #Court
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
            #Theme
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
    # Query team data and format results when button is clicked
    observeEvent(input$getdefense, {
        # Store team names and number
        teams <- input$defenseteams
        nteams <- length(teams)
        # Build query
        teamnames <- sapply(teams, function(t){
            paste0('TEAM = ',
                   '\'',
                   t,
                   '\'',
                   ' OR ')}) %>%
            paste0(collapse = '')
        teamnames <- substr(teamnames, 1, (nchar(teamnames)-4))

        q <- paste0('SELECT TEAM, OPP_EFG_PCT, OPP_TOV_PCT, DRB_PCT FROM TEAM_ADVANCED_DATA WHERE ',
                    teamnames,
                    ';')

        # Execute query and format result table
        con <- odbcConnect('DB2', 'fjs08406', 'hVJHsH2WnvB9H3Bm')
        data <- sqlQuery(con, q) %>%
            setDT()
        closeAllConnections()
        setnames(data, names(data), tolower(names(data)))
        data[, opp_efg_pct := opp_efg_pct]
        data <- melt.data.table(data, id.vars = 'team')
        
        # Generate plot
        output$defenseplot <- renderPlot({

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
    
    })
    
    
    output$positionplot <- renderPlotly({
        
        # Require input so user doesn't see an error
        validate(
            need(input$positionstat, ''),
        )
        
        # Store stat
        inputstat <- input$positionstat
        
        # Query and format
        con <- odbcConnect('DB2', 'fjs08406', 'hVJHsH2WnvB9H3Bm')
        key <- sqlFetch(con, 'PLAYER_DATA_KEY') %>%
            setDT()
        setnames(key, names(key), tolower(names(key)))
        data <- sqlFetch(con, 'PLAYER_DATA') %>%
            setDT()
        closeAllConnections()
        setnames(data, names(data), tolower(names(data)))
        data <- data[data[,player != 'Player']]
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
        
        # Generate connection to SQL DB2
        con <- odbcConnect('DB2', 'fjs08406', 'hVJHsH2WnvB9H3Bm')
        
        # Pull the key table
        key <- sqlFetch(con, 'PLAYER_DATA_KEY')
        setDT(key)
        
        # Extract the STAT associated with the chosen stat descriptions
        chosenstats <- key[DESCR %in% chosenstats, STAT]
        
        # Generate main query
        q <- chosenstats %>%
                str_to_upper(.) %>%
                str_c('ROUND(AVG(', ., '), 2) as AVG_',.) %>%
                paste0(., collapse = ', ') %>%
                paste0('select PLAYER, ', ., ' from PLAYER_DATA group by PLAYER;')
        
        # Execute query
        data <- sqlQuery(con, query = q) %>%
            setDT()
        closeAllConnections()
        setnames(data, names(data), tolower(names(data))) 
        DT::datatable(data)
    })
    
    
    session$onSessionEnded(stopApp)
})

                               