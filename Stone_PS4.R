library(rvest)    
wikiURL <- 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'

## Grab the tables from the page and use the html_table function to extract the tables.
## You need to subset temp to find the data you're interested in (HINT: html_table())

table.nodes <- wikiURL %>% 
  read_html %>%
  html_nodes("table")
table.nodes[[2]]

main.table <- html_table(table.nodes[[2]])
main.table <- main.table[-c(1,2),]

colnames(main.table) <- c("Election Number","Year","Winner","Winner Party",
                          "Winner popular vote (%)","Winner Margin","Runner-up popular vote (%)",
                          "Runner-up Margin","Runner-up","Runner-up Party",
                          "Turnout")







