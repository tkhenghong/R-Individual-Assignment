# TEOH KHENG HONG TP030562
# IRP Individual Assignment

# Installing tidyverse
install.packages("tidyverse")
library(tidyverse)

# Step 1: Import Data
# Read files()
getwd()

setwd("./") #set to the working directory
epldata = read.csv("england-premier-league-2019-to-2020.csv") #based on the current working directory
View(epldata)

class(epldata)
str(epldata)

# Step 2: Cleaning/Pre-processing
# Cleaning/Pre-processing
# Removing the Division, Date, Time, and betting providers' columns
epldata = epldata[3:24];epldata

# Step 3: Data Exploration & Manipulation


# Analyse how many:

# Number of Rows
nrow(epldata)
# Number of Columns
ncol(epldata)

# Teams
teamcolumns = c(epldata[["HomeTeam"]], epldata[["AwayTeam"]]);teamcolumns
teams = sort(unique(teamcolumns));teams

# Number of teams
length(teams)

# Referees
referees = unique(epldata[["Referee"]]);referees

# Number of Referees
length(referees)

# Grouping football teams (shots, goals, shots on target, offsides, yellow cards, red cards)
homeTeamFactor = factor(epldata[["HomeTeam"]]);homeTeamFactor
awayTeamFactor = factor(epldata[["AwayTeam"]]);awayTeamFactor

# Total Goals
# FTHG = Full Time Home Team Goals
# FTAG = Full Time Away Team Goals

homeTeamsSumGoals = tapply(epldata[['FTHG']], homeTeamFactor, sum);homeTeamsSumGoals
awayTeamsSumGoals = tapply(epldata[['FTAG']], awayTeamFactor, sum);awayTeamsSumGoals

# Team total goals
teamSumGoals = homeTeamsSumGoals + awayTeamsSumGoals;teamSumGoals

# View the total goals of each team for whole season
View(teamSumGoals)

# Football team that made the most shots for the whole season
which.max(teamSumGoals)

# Football team that made the least shots for the whole season
which.min(teamSumGoals)

# Average team goals
averageTeamGoals = mean(teamSumGoals)

# Max Goals
# FTHG = Full Time Home Team Goals
# FTAG = Full Time Away Team Goals
# Max goals for each team made when they're as home team in a single match, in ascending alphabetical order
homeTeamsMaxGoals = tapply(epldata[['FTHG']], homeTeamFactor, max);homeTeamsMaxGoals

# Max goals for each team made when they're as away team in a single match, in ascending alphabetical order
awayTeamsMaxGoals = tapply(epldata[['FTAG']], awayTeamFactor, max);awayTeamsMaxGoals

# The team that made most/least goals as home team in a single match
which.max(homeTeamsMaxGoals)
which.min(homeTeamsMaxGoals)

# The team that made most/least goals as away team in a single match
which.max(awayTeamsMaxGoals)
which.min(awayTeamsMaxGoals)

# Average team max goals
averageHomeTeamMaxGoals = mean(homeTeamsMaxGoals);averageHomeTeamMaxGoals
averageAwayTeamMaxGoals = mean(awayTeamsMaxGoals);averageAwayTeamMaxGoals

# Referee
# Create a function to get mode of a vector.

getmode <- function(v) {
  uniqv <- sort(unique(v))
  vtable = table(v)
  return (uniqv[[which.max(vtable)]])
}

# Which referee showed up the most in the whole season
getmode(epldata[['Referee']])

# Full Time Goals
# FTHG = Full Time Home Team Goals
# FTAG = Full Time Away Team Goals
fullTimeHomeSumGoals = tapply(epldata[['FTHG']], homeTeamFactor, sum);fullTimeHomeSumGoals   
fullTimeAwaySumGoals = tapply(epldata[['FTAG']], awayTeamFactor, sum);fullTimeAwaySumGoals

# Which team did most/least goals as the home team?
which.max(fullTimeHomeSumGoals);fullTimeHomeSumGoals[[which.max(fullTimeHomeSumGoals)]]
which.min(fullTimeHomeSumGoals);fullTimeHomeSumGoals[[which.min(fullTimeHomeSumGoals)]]

# Which team did most/least goals as the away team?
which.max(fullTimeAwaySumGoals);fullTimeAwaySumGoals[[which.max(fullTimeAwaySumGoals)]]
which.min(fullTimeAwaySumGoals);fullTimeAwaySumGoals[[which.min(fullTimeAwaySumGoals)]]

# Total Goals of each team in the whole season
fullTimeSumGoals = fullTimeHomeSumGoals + fullTimeAwaySumGoals;fullTimeSumGoals

# Team that did most/least goals in the whole season
which.max(fullTimeSumGoals);fullTimeSumGoals[[which.max(fullTimeSumGoals)]]
which.min(fullTimeSumGoals);fullTimeSumGoals[[which.min(fullTimeSumGoals)]]

# Shots
# HS = Home Team Shots
# AS = Away Team Shots
fullTimeHomeSumShots = tapply(epldata[['HS']], homeTeamFactor, sum);fullTimeHomeSumShots
fullTimeAwaySumShots = tapply(epldata[['AS']], awayTeamFactor, sum);fullTimeAwaySumShots

# Total shots made by teams as home team
fullTimeSumShots = fullTimeHomeSumShots + fullTimeAwaySumShots;fullTimeSumShots

# Which team made most/least shots in the whole season?
which.max(fullTimeSumShots);fullTimeSumShots[[which.max(fullTimeSumShots)]]
which.min(fullTimeSumShots);fullTimeSumShots[[which.min(fullTimeSumShots)]]

names(fullTimeSumShots)[which.max(fullTimeSumShots)]

sumShotsDifference = fullTimeSumShots[[which.max(fullTimeSumShots)]] - fullTimeSumShots[[which.min(fullTimeSumShots)]];sumShotsDifference

# Shots on Target
# HST = Home Team Shots on Target
# AST = Away Team Shots on Target
fullTimeHomeSumShotsOnTarget = tapply(epldata[['HST']], homeTeamFactor, sum);fullTimeHomeSumShotsOnTarget
fullTimeAwaySumShotsOnTarget = tapply(epldata[['AST']], awayTeamFactor, sum);fullTimeAwaySumShotsOnTarget

fullTimeSumShotsOnTarget = fullTimeHomeSumShotsOnTarget + fullTimeAwaySumShotsOnTarget;fullTimeSumShotsOnTarget

# Which team made most/least shots on target in the whole season?
which.max(fullTimeSumShotsOnTarget);fullTimeSumShotsOnTarget[[which.max(fullTimeSumShotsOnTarget)]]
which.min(fullTimeSumShotsOnTarget);fullTimeSumShots[[which.min(fullTimeSumShotsOnTarget)]]

# Average shots on target
mean(fullTimeSumShotsOnTarget)

# Corners
# HC = Home Team Corner
# AC = Away Team Corner
fullTimeHomeSumCorner = tapply(epldata[['HC']], homeTeamFactor, sum);fullTimeHomeSumCorner
fullTimeAwaySumCorner = tapply(epldata[['AC']], awayTeamFactor, sum);fullTimeAwaySumCorner

fullTimeSumCorner = fullTimeHomeSumCorner + fullTimeAwaySumCorner;fullTimeSumCorner

# Team that did most/least corners in thw whole season.
which.max(fullTimeSumCorner);fullTimeSumCorner[[which.max(fullTimeSumCorner)]]
which.min(fullTimeSumCorner);fullTimeSumCorner[[which.min(fullTimeSumCorner)]]

# Average team corners in the whole season.
mean(fullTimeSumCorner)

# Yellow cards
# HY = Home Team Yellow Cards
# AY = Away Team Yellow Cards
homeSumYellowCards = tapply(epldata[['HY']], homeTeamFactor, sum);homeSumYellowCards
awaySumYellowCards = tapply(epldata[['AY']], homeTeamFactor, sum);awaySumYellowCards

sumYellowCards = homeSumYellowCards + awaySumYellowCards ;sumYellowCards

# Team that got the most/least yellow cards in thw whole season.
which.max(sumYellowCards);sumYellowCards[[which.max(sumYellowCards)]]
which.min(sumYellowCards);sumYellowCards[[which.min(sumYellowCards)]]

# Average Yellow cards
mean(sumYellowCards)

# Red cards
# HR = Home Team Red Cards
# AR = Away Team Red Cards
homeSumRedCards = tapply(epldata[['HR']], homeTeamFactor, sum);homeSumRedCards
awaySumRedCards = tapply(epldata[['AR']], homeTeamFactor, sum);awaySumRedCards

sumRedCards = homeSumRedCards + awaySumRedCards;sumRedCards

# Team that got the most/least yellow cards in thw whole season.
which.max(sumRedCards);sumRedCards[[which.max(sumRedCards)]]
which.min(sumRedCards);sumYellowCards[[which.min(sumRedCards)]]

# Average Red cards
mean(sumRedCards)

# Count shots efficiency

# Step 1: Create tibbles
fullTimeShots = tibble(data.frame(teams, shots=fullTimeSumShots));fullTimeShots
fullTimeShotsOnTarget = tibble(data.frame(teams, shots=fullTimeSumShotsOnTarget));fullTimeShotsOnTarget
teamGoals = tibble(data.frame(teams, shots=teamSumGoals));teamGoals
teamCorners = tibble(data.frame(teams, shots=fullTimeSumCorner));teamCorners
yellowCards = tibble(data.frame(teams, shots=yellowCards));yellowCards
redCards = tibble(data.frame(teams, shots=sumRedCards));redCards


# Step 2: Create plots
# Full Time Shots Barplot
ggplot(fullTimeShots, aes(teams,shots, fill=rainbow(length(teams)))) +
  geom_col(show.legend = FALSE) +
  labs(title = 'Full Time Shots', subtitle = 'Total Shots of every team in the whole season', y = 'Shots', x = 'Team Name')

# Full Time Shots on Target Barplot
ggplot(fullTimeShotsOnTarget, aes(teams,shots, fill=rainbow(length(teams)))) +
  geom_col(show.legend = FALSE) +
  labs(title = 'Full Time Shots on Target', subtitle = 'Total Shots on Target of every team in the whole season', y = 'Shots', x = 'Team Name')

# Full Time Goals Barplot
ggplot(teamGoals, aes(teams,shots, fill=rainbow(length(teams)))) +
  geom_col(show.legend = FALSE) +
  labs(title = 'Full Time Goals', subtitle = 'Total Goals of every team in the whole season', y = 'Shots', x = 'Team Name')

# Team Corner Barplot
ggplot(teamCorners, aes(teams,shots, fill=rainbow(length(teams)))) +
  geom_col(show.legend = FALSE) +
  labs(title = 'Team Corners', subtitle = 'Total Corner of every team in the whole season', y = 'Corners', x = 'Team Name')

# Team Yellow Cards Barplot
ggplot(yellowCards, aes(teams,shots, fill=rainbow(length(teams)))) +
  geom_col(show.legend = FALSE) +
  labs(title = 'Team Yellow Cards', subtitle = 'Total Yellow Cards of every team in the whole season', y = 'Yellow Cards', x = 'Team Name')

# Team Red Cards Barplot
ggplot(redCards, aes(teams,shots, fill=rainbow(length(teams)))) +
  geom_col(show.legend = FALSE) +
  labs(title = 'Team Red Cards', subtitle = 'Total Red Cards of every team in the whole season', y = 'Red Cards', x = 'Team Name')




# How many betting providers

# Who won in the end
# Which team shots the most (in home team/away team/together)
# What is the average betting odds in each team (Home goal/Draw/Away team goal) (group by different betting providers)

# More home team or away won the matches in the whole season?







