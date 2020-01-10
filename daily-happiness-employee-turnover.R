##Presentation

list.of.packages <- c("tidyverse", "stringr", "lubridate", "ggplot2", "ggthemes", "wesanderson")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(wesanderson)

churn <- read.csv(file = "./data/churn.csv")
votes <- read.csv(file = "./data/votes.csv")

str(churn)
str(votes)

##Clean

#ignore negative employees ids 
churn <- churn %>% filter(employee > 0) 

#delete companies with less than 30 votes
votes <- votes %>% filter(!(companyAlias %in% (churn %>% 
                                                 group_by(companyAlias) %>% 
                                                 summarise(votes = sum(numVotes)) %>% 
                                                 filter(votes < 30))$companyAlias))
churn <- churn %>% filter(!(companyAlias %in% (churn %>% 
                                                 group_by(companyAlias) %>% 
                                                 summarise(votes = sum(numVotes)) %>% 
                                                 filter(votes < 30))$companyAlias))
churn <- droplevels(churn)
votes <- droplevels(votes) 

#nicer Alias
if (all(levels(churn$companyAlias) == levels(votes$companyAlias)) == TRUE) { #check if alias are the same
  levels(churn$companyAlias) <- paste("X", 1:length(levels(churn$companyAlias)), sep = "")
  levels(votes$companyAlias) <- paste("X", 1:length(levels(votes$companyAlias)), sep = "")
} else {
  print("error: alias are different")
}

#new employees ids
churn$employee <- paste(churn$employee, churn$companyAlias, sep = "")
votes$employee <- paste(votes$employee, votes$companyAlias, sep = "")

#date standardization
votes$wday <- factor(str_sub(votes$voteDate, 1, 3), 
                     levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
votes$month <- factor(str_sub(votes$voteDate, 5, 7),
                      levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
votes$voteDate <- ymd(paste(str_sub(votes$voteDate, -4, -1), str_sub(votes$voteDate, 5, 10)))
votes$day <- day(votes$voteDate)
votes$voteYear <- year(votes$voteDate)
votes$nwday <- wday(votes$voteDate, week_start = getOption("lubridate.week.start", 1)) - 1 #0 = Mon, 6 = Sun
votes$yday <- yday(votes$voteDate) #12th february = 43
votes$week <- isoweek(votes$voteDate)
votes[votes$week %in% c(52,53) & votes$month == "Jan", ]$week <- 0
votes$week <- factor(votes$week, levels = c(0:53))
churn$lastParticipationDate <- ymd(paste(str_sub(churn$lastParticipationDate, -4, -1), 
                                         str_sub(churn$lastParticipationDate, 5, 10)))

#delete duplicates
churn <- churn %>% filter(!(employee %in% (churn %>% group_by(employee) %>% 
                                             summarise(count = n()) %>% 
                                             filter(count > 1))$employee))
votes$temp <- paste(votes$employee, votes$voteDate, sep = "") #temporary id employee & voteDate
votes <- votes %>% filter(!(temp %in% (votes %>% group_by(temp) %>% 
                                         summarise(count = n()) %>% 
                                         filter(count > 1))$temp))
votes$temp <- NULL

##Daily Happiness

happinessRate <- votes %>% 
  group_by(companyAlias) %>% 
  summarise(happinessRate = round(mean(vote), 2))

#Happiness Rate by Company
ggplot(happinessRate, aes(reorder(companyAlias, happinessRate), happinessRate, fill = happinessRate)) +
  geom_bar(stat = "identity") +
  scale_fill_gradientn(colors = rev(wes_palette("Zissou1", 34, type = "continuous"))) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, size = 10),
        axis.text.y = element_text(vjust = 0.5)) +
  scale_y_continuous(breaks = seq(0,5,0.5), limits = c(0, 4)) +
  labs(title = "Happiness Rate by companies", x = "Company alias", y = "Average happiness vote")

#hapinessRate Mean by wday
ggplot(votes %>% group_by(wday) %>% summarise(happinessRate = mean(vote), count = n()/n_distinct(voteDate)), 
       aes(x = wday, y = happinessRate, fill = happinessRate)) +
  geom_bar(stat = "identity") + 
  scale_fill_gradientn(colors = rev(wes_palette("Zissou1", 7, type = "continuous"))) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0,5,0.5), limits = c(0, 4)) +
  geom_text(aes(label = round(happinessRate, 2)), vjust = 10, color = "white", fontface = 2) +
  geom_text(aes(label = round(count, 0)), vjust = 20, color = "white") +
  labs(title = "Happiness Rate by working days and the average number of votes", x = "Working day", y = "Average happiness vote") 

#heatmap
ggplot(data = votes %>% filter(voteYear > 2014) %>% group_by(day, wday, month, week, voteYear) %>% 
         summarise(happinessRate = mean(vote)), aes(x = week, y = fct_rev(wday))) + 
  geom_tile(aes(fill = happinessRate)) + 
  scale_fill_gradientn(colors = rev(wes_palette("Zissou1", 50, type = "continuous"))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_grid(voteYear~.) +
  coord_fixed() + 
  labs(title = "Happiness Rate by working days and weeks", x = "Year week", y = "Average happiness vote")

##Employee Turnover 

companies <- merge(votes %>% group_by(companyAlias) %>% summarise(vote = mean(vote), 
                                                                  wday = mean(nwday), 
                                                                  yday = mean(yday),
                                                                  year = mean(voteYear)),
                   churn %>% group_by(companyAlias) %>% 
                     summarise(true = sum(stillExists == "true"), total = n()) %>% 
                     mutate(turnoverRate = round((1 - true / total), 5)),
                   by = c("companyAlias"))[-c(6:7)]
rownames(companies) <- companies$companyAlias
companies <- companies[-1]
round(cor(companies, method = c("pearson")), 3)

ggplot(companies, aes(x = vote, y = turnoverRate)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ log(x), colour = wes_palette("Darjeeling1")[4], fill = wes_palette("Darjeeling1")[5]) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2, 4, 0.5)) +
  labs(title = "Correlation between the turnover rate and happiness rate", x = "Average happiness vote", y = "Turnover rate")

