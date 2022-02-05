##### README #####

#' The following script scrapes 2021/2022 NBA season data from 
#' basketball-reference.com. Specifically, the script scrapes the 'Advanced Stats' 
#' and 'Shooting Stats' for team data and scrapes 'Total Stats' for individual players.
#' 
#' The data is lightly cleaned, e.g. removing empty columns, and saved to a SQL
#' database using IBM's Db2 on Cloud service.

##### Packages #####
library(data.table)
library(tidyr)
library(RODBC)
library(xml2)
library(rvest)

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

nba.scrap <- function(webdata){
  stopifnot(is.data.table(webdata))
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
}


##### Write Data To Db2 #####


# Connect using RODBC package
con <- odbcConnect('DB2', 'fjs08406', 'hVJHsH2WnvB9H3Bm')

for (dname in webdata$dataname){
  sqlSave(con, scraped[[dname]], tablename = dname)
  keyname <- paste0(dname, "_KEY")
  sqlSave(con, statkeys[[keyname]], tablename = keyname)
}
closeAllConnections()






