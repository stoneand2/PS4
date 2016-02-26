##################
### Homework 4 ###
##################

setwd("~/github/PS4")

library(rvest); library(stringr)

wikiURL <- 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'

# Credit to Dave for the starter code

# Getting the tables from the page, finding the nodes with the "table" class
table.nodes <- wikiURL %>% 
  read_html %>%
  html_nodes("table")

# Subsetting all found table nodes to find the one we want, and then using html_table() to read it in
main.table <- html_table(table.nodes[[2]])
main.table <- main.table[-c(1,2),]

# Re-writing column names
colnames(main.table) <- c("Election Number","Year","Winner","Winner Party",
                          "Winner popular vote (%)","Winner Margin (%)","Winner popular vote (total)",
                          "Winner Margin (total)","Runner-up","Runner-up Party",
                          "Turnout")

# Cleaning the data
# Some of the numbers were read into R strangely
main.table$Winner <- str_extract(main.table$Winner, "[[:upper:]][[:alpha:]]+, [[:upper:]][[:lower:]]+")
main.table$"Runner-up" <- str_extract(main.table$"Runner-up", 
                                      "[[:upper:]][[:alpha:]]+, [[:upper:]][[:lower:]]+")

main.table[1:4,"Winner Margin (%)"] <- str_extract(main.table[1:4,"Winner Margin (%)"],
                                               "−.*")
main.table[1:4,"Winner Margin (%)"] <- gsub("−","-",main.table[1:4,"Winner Margin (%)"])
main.table[1:4,"Winner Margin (total)"] <- str_extract(main.table[1:4,"Winner Margin (total)"],
                                               "−.*")
main.table[1:4,"Winner Margin (total)"] <- gsub("−","-",main.table[1:4,"Winner Margin (total)"])
main.table[5:29,"Winner Margin (%)"] <- str_extract(main.table[5:29,"Winner Margin (%)"], 
                                                "[[:digit:]].[[:digit:]]{2}%")


main.table[,"Winner Margin (total)"] <- gsub(",","",main.table[,"Winner Margin (total)"])
main.table[,"Winner Margin (total)"] <- sub("^[0]+", "",main.table[,"Winner Margin (total)"])

n.chars.strings <- nchar(main.table[6:48,"Winner Margin (total)"])
starting <- (n.chars.strings / 2) + 1
main.table[6:48,"Winner Margin (total)"] <- substr(main.table[6:48,"Winner Margin (total)"], 
                                                   start=starting, stop=n.chars.strings)
main.table[5,"Winner Margin (total)"] <- 1898

main.table[,"Winner popular vote (%)"] <- as.numeric(gsub(pattern="%", replacement="", 
                                                x=main.table[,"Winner popular vote (%)"])) / 100

main.table[,"Winner Margin (%)"] <- as.numeric(gsub(pattern="%", replacement="", 
                                                          x=main.table[,"Winner Margin (%)"])) / 100

# Ordering by year
main.table <- main.table[with(main.table, order(Year)),]

pdf(file="trends.pdf")

# Share of popular vote of Democrats and Republicans over time
main.table$"Runner up share (%)" <- main.table[,"Winner popular vote (%)"]  - 
                                        main.table[,"Winner Margin (%)"]
# Democrat winners
plot(main.table$Year[which(main.table$"Winner Party" == "Dem.")], 
     main.table$"Winner popular vote (%)"[which(main.table$"Winner Party" == "Dem.")],
     xaxt="n", 
     ylim=c(0.35, 0.65), 
     pch=17, 
     col="blue", 
     xlab="Year",
     ylab="Popular vote share (%)", 
     main="Figure 1: Popular Vote Shares of Winning D/R Presidential Candidates Over Time", 
     cex.main=1)
axis(side=1, labels=c(1824, 1860, 1900, 1940, 1980, 2016), at=c(1824, 1860, 1900, 1940, 1980, 2016))

lines(sort(main.table$Year[which(main.table$"Winner Party" == "Dem.")]), 
      y = main.table$"Winner popular vote (%)"[which(main.table$"Winner Party" == "Dem.")], 
      type = "l", col="blue", lty=2)
# Republican winners
points(main.table$Year[which(main.table$"Winner Party" == "Rep.")], 
     main.table$"Winner popular vote (%)"[which(main.table$"Winner Party" == "Rep.")],
     pch=16, 
     col="red")
lines(sort(main.table$Year[which(main.table$"Winner Party" == "Rep.")]), 
      y = main.table$"Winner popular vote (%)"[which(main.table$"Winner Party" == "Rep.")], 
      type = "l", col="red", lty=2)

legend("topleft",
       legend=c("Democrats", "Republicans"), 
       pch=c(17,16),
       col=c("blue", "red"), cex=0.8)

dev.off()






