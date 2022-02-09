

library(shiny)
library(shinycssloaders)
library(data.table)
library(tidyr)
library(RODBC)
library(xml2)
library(rvest) 
library(DT)
library(ggplot2)
library(ggforce)
# When app is opened, scrape up-to-date NBA data and write to Db2
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
scrapeandwrite.data <- function(webdata, connection){        
    scraped <- list()
    statkeys <- list()
    for (row in 1:nrow(webdata)){
        
        webtable <- webdata[row, urlsource] %>%
            read_html() %>%
            html_elements(css = webdata[row, tableselector]) %>%
            html_table() %>%
            as.data.table()
        
        webtablekey <- webdata[row, urlsource] %>%
            read_html() %>%
            html_elements(css = webdata[row, headerselector]) %>%
            html_children()
        
        vars <- sapply(webtablekey, function(x) {x <- html_attr(x,'data-stat') %>%
            tolower()})
        setnames(webtable, names(webtable), vars)
        webtable[ , which(names(webtable) == 'dummy') := NULL]
        
        webtablekey <- sapply(webtablekey, function(x) {n <- html_attr(x,'aria-label')})
        k <- data.table('stat' = vars, 'description' = webtablekey)
        k <- k[stat != 'dummy']
        
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
 
    # Set the scrapeandwrite.data() function to invalidate so the data is updated once every 24 hours
    reactive({
        invalidateLater(600000)    # refresh the report every 600k milliseconds (600 seconds)
        scrapeandwrite.data(webdata)                # call our function from above
    })
    
    
    # Pull TEAM_ADVANCED_DATA from db2
    con <- odbcConnect('DB2', 'fjs08406', 'hVJHsH2WnvB9H3Bm')
    teams <- sqlFetch(con, 
                      sqtable = 'FJS08406.TEAM_ADVANCED_DATA', 
                      rownames = FALSE) %>%
                as.data.table()
    closeAllConnections()
    
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
        as.data.table()
    closeAllConnections()
    
    ##### Team FG% Diagram #####
    con <- odbcConnect('DB2', 'fjs08406', 'hVJHsH2WnvB9H3Bm')
    d <- sqlFetch(con, 'TEAM_SHOOTING') %>% as.data.table()
    k <-  sqlFetch(con, 'TEAM_SHOOTING_KEY') %>% as.data.table()
    closeAllConnections()
    
    
    # Team1 Field Goal Percentage diagram
    output$teamFGdiagram1 <- renderPlot({
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
        shotdist[, fg.percent := as.factor(fg.percent*100)]
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
            #geom_point(color= 'black') +
            
            
            coord_cartesian(xlim = c(50,100), ylim = c(26,76)) + 
            
            coord_flip() +
            # xlim(50, 101) +
            # ylim(0,100)+
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
        shotdist2[, fg.percent := as.factor(fg.percent*100)]
        shotdist2[, fg.percent := paste0(fg.percent, '%')]
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
            #geom_point(color= 'black') +
            
            
            coord_cartesian(xlim = c(50,100), ylim = c(26,76)) + 
            
            coord_flip() +
            # xlim(50, 101) +
            # ylim(0,100)+
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
            ggtitle(paste0(teamchoice2, ' FG% Across Ranges'))
        
    })
    
    
    session$onSessionEnded(stopApp)
})
