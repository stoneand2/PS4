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





##### THE PLOTS ######

pdf(file="trends.pdf")

par(mar=c(4.5, 5.7, 4.1, 2.1))
layout(matrix(c(1,1,2,2), nrow=2, 2, byrow = TRUE))

# Plot 1: Popular Vote Shares of Winning Democrats and Republicans Over Time

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
abline(.50, 0)

lines(main.table$Year[which(main.table$"Winner Party" == "Dem.")], 
      y = main.table$"Winner popular vote (%)"[which(main.table$"Winner Party" == "Dem.")], 
      type = "l", col="blue", lty=2)
# Republican winners
points(main.table$Year[which(main.table$"Winner Party" == "Rep.")], 
     main.table$"Winner popular vote (%)"[which(main.table$"Winner Party" == "Rep.")],
     pch=16, 
     col="red")
lines(main.table$Year[which(main.table$"Winner Party" == "Rep.")], 
      y = main.table$"Winner popular vote (%)"[which(main.table$"Winner Party" == "Rep.")], 
      type = "l", col="red", lty=2)

legend("topleft",
       legend=c("Democrats", "Republicans"), 
       pch=c(17,16),
       col=c("blue", "red"), cex=0.8)


# Plot 2: Winning Margin Over Time, Differentiating by Margins more/less than 10% of total vote

plot(main.table$Year[which(main.table$"Winner Margin (%)" > .1)], 
     main.table$"Winner Margin (total)"[which(main.table$"Winner Margin (%)" > .1)],
     xaxt="n", 
     yaxt="n",
     xlim=c(1820, 2016),
     ylim=c(-600000,20000000),
     pch=17, 
     col="steelblue4", 
     xlab="Year",
     ylab="Winner margin (millions of votes)", 
     main="Figure 2: Popular Vote Margin of Presidential Candidates Over Time", 
     cex.main=1)
abline(0,0)
axis(side=1, 
     labels=c(1824, 1860, 1900, 1940, 1980, 2016), 
     at=c(1824, 1860, 1900, 1940, 1980, 2016))
axis(side=2, 
     labels=seq(from=-1, to=20, by=2), 
     at=c(seq(from=-1000000, to=20000000, by=2000000)))
lines(main.table$Year, 
      main.table$"Winner Margin (total)", 
      lty=1)
points(main.table$Year[which(main.table$"Winner Margin (%)" <= .1)], 
       main.table$"Winner Margin (total)"[which(main.table$"Winner Margin (%)" <= .1)], 
       pch=16, col="sienna2")

legend("topleft",
       legend=c("Margin > 10% of All Votes", "Margin <= 10% of All Votes"), 
       pch=c(17,16),
       col=c("steelblue4", "sienna2"), cex=0.8)


layout(matrix(c(3,3,4,4), nrow=2, 2, byrow = TRUE))

# Plot 3: 1904-2000 Election Winner Popular Vote Margins as Fraction of Entire U.S. 
# Population at Time of Election

# Loess smoothers fit for Democrats and Republicans

# So, this plot lets us know what percentage of Americans would have needed to vote for the runner-up
# to allow the runner-up to win the popular vote

# Data on U.S. population, 1990-1999
population <- read.csv("popdata1900_1999.csv", header=F, sep="")[,3:4]
# Subtracting one from main.table$Year to match with year election was actually held in
year.of.election <- main.table[21:44, "Year"] - 1

# Function to match total U.S. population data to existing election data
year.to.pop <- function(year){
  population[,2][population[,1] == year]
}
# lapply() to apply function to each election year, unlist() to turn to vector, as.character() to make
# non-factor, gsub() to remove commas, as.numeric() to make numeric
total.pop.election.years <- as.numeric(gsub(",","",as.character(unlist(lapply(year.of.election, 
                                                                              year.to.pop)))))
# Making the election winners' popular vote margins numeric
winners.margin <- as.numeric(main.table[21:44,"Winner Margin (total)"])

# Finding winners' popular vote margin as function of total U.S. population, rounding to 3 decimals
total.as.fraction.population <- round(winners.margin / total.pop.election.years, 3)

# Table of candidates from this era
results.1900s <- main.table[21:44,]
  
# The actual plot 
plot(results.1900s[, "Year"][which(results.1900s$"Winner Party" == "Dem.")], 
     total.as.fraction.population[which(results.1900s$"Winner Party" == "Dem.")],
     xaxt="n", 
     yaxt="n",
     xlim=c(1900, 2000),
     ylim=c(0,0.15),
     pch=15, 
     col="blue", 
     xlab="Year",
     ylab="Winning margin as percentage of 
     total U.S. population", 
     main="Figure 3: Popular Vote Margin of Presidential Candidates (1904-2000) 
     As Percentage of Total U.S. Population", 
     cex.main=1)
points(results.1900s[, "Year"][which(results.1900s$"Winner Party" == "Rep.")], 
       total.as.fraction.population[which(results.1900s$"Winner Party" == "Rep.")], col="red",
       pch=16)
abline(0,0, lty=2)
axis(side=1, 
     labels=c(1900, 1920, 1940, 1960, 1980, 2000), 
     at=c(1900, 1920, 1940, 1960, 1980, 2000))
axis(side=2, 
     labels=seq(from=0, to=0.15, by=0.01), 
     at=seq(from=0, to=0.15, by=0.01))
text(main.table[21:44, "Year"], total.as.fraction.population, 
     labels=main.table[21:44, "Winner"], cex= 0.5, pos=3)

legend("topleft",
       legend=c("Republican Winners", "Democrat Winners"), 
       pch=c(16,17),
       col=c("red", "blue"), cex=0.65, pt.cex = 1)

loess.dems <- loess(total.as.fraction.population[which(results.1900s$"Winner Party" == "Dem.")] ~ 
                      results.1900s[, "Year"][which(results.1900s$"Winner Party" == "Dem.")], 
                    span=0.6)
lines(y=predict(loess.dems), x=results.1900s[, "Year"][which(results.1900s$"Winner Party" == "Dem.")],
      type="l", col="blue")

loess.reps <- loess(total.as.fraction.population[which(results.1900s$"Winner Party" == "Rep.")] ~ 
                      results.1900s[, "Year"][which(results.1900s$"Winner Party" == "Rep.")], 
                    span=0.6)
lines(y=predict(loess.reps), x=results.1900s[, "Year"][which(results.1900s$"Winner Party" == "Rep.")],
      type="l", col="red")


dev.off()


# Scraping electoral college votes won by candidates

