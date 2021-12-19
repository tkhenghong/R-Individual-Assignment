# TEOH KHENG HONG TP030562
# IRP Individual Assignment

# Load R Data
load('data.RData')

# Installing tidyverse
install.packages("tidyverse")
library(tidyverse)

# Flatten the multi columns in data frame in order to create group bar plot
# Reference: https://stackoverflow.com/a/10213993
install.packages('reshape2')
library(reshape2)

# Step 1: Import Data
# Read files()
getwd()

setwd("/Users/teohkhenghong/Desktop/RProjects/R-Individual-Assignment") #set to the working directory
epldata = read.csv("england-premier-league-2019-to-2020.csv") #based on the current working directory
View(epldata)

class(epldata)
str(epldata)

# Original dataset
# Number of Rows
nrow(epldata)
# Number of Columns
ncol(epldata)

# Step 2: Cleaning/Pre-processing
# Cleaning/Pre-processing
# Removing the Division, Date, Time, and betting providers' columns
epldata = epldata[4:24];epldata

# After removing uneccessary data:
# View the dataset
View(epldata)
# Number of Rows
nrow(epldata)
# Number of Columns
ncol(epldata)

# Step 3: Data Exploration & Manipulation

# Analyse how many:

# Teams
teamcolumns = c(epldata[["HomeTeam"]], epldata[["AwayTeam"]]);teamcolumns
teams = sort(unique(teamcolumns));teams

# Number of teams
length(teams)

# Referees
referees = unique(epldata[["Referee"]]);referees

# Number of Referees
length(referees)

# Referee
# Create a function to get mode of a vector.

getmode <- function(v) {
  uniqv <- sort(unique(v))
  vtable = table(v)
  return (uniqv[[which.max(vtable)]])
}

# Which referee showed up the most in the whole season
getmode(epldata[['Referee']])

# Grouping football teams (shots, goals, shots on target, offsides, yellow cards, red cards)
homeTeamFactor = factor(epldata[["HomeTeam"]]);homeTeamFactor
awayTeamFactor = factor(epldata[["AwayTeam"]]);awayTeamFactor

class(awayTeamFactor)
str(awayTeamFactor)

# Max Goals
# FTHG = Full Time Home Team Goals
# FTAG = Full Time Away Team Goals
# Max goals for each team made when they're as home/away team in a single match, in ascending alphabetical order
homeTeamsMaxGoals = tapply(epldata[['FTHG']], homeTeamFactor, max);homeTeamsMaxGoals
awayTeamsMaxGoals = tapply(epldata[['FTAG']], awayTeamFactor, max);awayTeamsMaxGoals

# The team that made most goals as home team in a single match
which.max(homeTeamsMaxGoals);homeTeamsMaxGoals[[which.max(homeTeamsMaxGoals)]]

# The team that made most goals as away team in a single match
which.max(awayTeamsMaxGoals);awayTeamsMaxGoals[[which.max(awayTeamsMaxGoals)]]

# Average team max goals
averageHomeTeamMaxGoals = mean(homeTeamsMaxGoals);averageHomeTeamMaxGoals
averageAwayTeamMaxGoals = mean(awayTeamsMaxGoals);averageAwayTeamMaxGoals

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

# Team Average goals
averageFullTimeSumGoals = mean(fullTimeSumGoals);averageFullTimeSumGoals

# Teams that are below average goals
# A function to get the array elements that is below average
getBelowAverageArrayNames <- function (x) {
  average <- mean(x)
  # https://stackoverflow.com/a/34584990
  print(paste('Average is: ', average))
  return (names(x[x < average]))
}

getBelowAverageArrayNames(fullTimeSumGoals)

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

# The team that scored the most
# Get the name of the array. Reference: https://stackoverflow.com/a/21422242
names(fullTimeSumShots)[which.max(fullTimeSumShots)];fullTimeSumShots[[which.max(fullTimeSumShots)]]
names(fullTimeSumShots)[which.min(fullTimeSumShots)];fullTimeSumShots[[which.min(fullTimeSumShots)]]

sumShotsDifference = fullTimeSumShots[[which.max(fullTimeSumShots)]] - fullTimeSumShots[[which.min(fullTimeSumShots)]];sumShotsDifference

# Shots on Target
# HST = Home Team Shots on Target
# AST = Away Team Shots on Target
fullTimeHomeSumShotsOnTarget = tapply(epldata[['HST']], homeTeamFactor, sum);fullTimeHomeSumShotsOnTarget
fullTimeAwaySumShotsOnTarget = tapply(epldata[['AST']], awayTeamFactor, sum);fullTimeAwaySumShotsOnTarget

fullTimeSumShotsOnTarget = fullTimeHomeSumShotsOnTarget + fullTimeAwaySumShotsOnTarget;fullTimeSumShotsOnTarget

# Which team made most/least shots on target in the whole season?
names(fullTimeSumShotsOnTarget)[which.max(fullTimeSumShotsOnTarget)];fullTimeSumShotsOnTarget[[which.max(fullTimeSumShotsOnTarget)]]
names(fullTimeSumShotsOnTarget)[which.min(fullTimeSumShotsOnTarget)];fullTimeSumShotsOnTarget[[which.min(fullTimeSumShotsOnTarget)]]

# Difference
shotsOnTargetDifference = fullTimeSumShotsOnTarget[[which.max(fullTimeSumShotsOnTarget)]] - fullTimeSumShotsOnTarget[[which.min(fullTimeSumShotsOnTarget)]];shotsOnTargetDifference

# Average shots on target
mean(fullTimeSumShotsOnTarget)

# Teams that made below average shots on target
belowAverageShotsOnTargetTeamNames = getBelowAverageArrayNames(fullTimeSumShotsOnTarget);belowAverageShotsOnTargetTeamNames
length(belowAverageShotsOnTargetTeamNames)
fullTimeSumShotsOnTarget[belowAverageShotsOnTargetTeamNames]

# Corners
# HC = Home Team Corner
# AC = Away Team Corner
fullTimeHomeSumCorner = tapply(epldata[['HC']], homeTeamFactor, sum);fullTimeHomeSumCorner
fullTimeAwaySumCorner = tapply(epldata[['AC']], awayTeamFactor, sum);fullTimeAwaySumCorner

fullTimeSumCorner = fullTimeHomeSumCorner + fullTimeAwaySumCorner;fullTimeSumCorner

# Team that did most/least corners in the whole season.
names(fullTimeSumCorner)[which.max(fullTimeSumCorner)];fullTimeSumCorner[[which.max(fullTimeSumCorner)]]
names(fullTimeSumCorner)[which.min(fullTimeSumCorner)];fullTimeSumCorner[[which.min(fullTimeSumCorner)]]

# Difference
cornersDifference = fullTimeSumCorner[[which.max(fullTimeSumCorner)]] - fullTimeSumCorner[[which.min(fullTimeSumCorner)]];cornersDifference

# Average team corners in the whole season.
mean(fullTimeSumCorner)

# Below average corners
belowAverageCornersTeamNames = getBelowAverageArrayNames(fullTimeSumCorner);belowAverageCornersTeamNames
length(belowAverageCornersTeamNames)
fullTimeSumCorner[belowAverageCornersTeamNames]

# Yellow cards
# HY = Home Team Yellow Cards
# AY = Away Team Yellow Cards
homeSumYellowCards = tapply(epldata[['HY']], homeTeamFactor, sum);homeSumYellowCards
awaySumYellowCards = tapply(epldata[['AY']], homeTeamFactor, sum);awaySumYellowCards

sumYellowCards = homeSumYellowCards + awaySumYellowCards ;sumYellowCards

# Team that got the most/least yellow cards in the whole season.
which.max(sumYellowCards);sumYellowCards[[which.max(sumYellowCards)]]
which.min(sumYellowCards);sumYellowCards[[which.min(sumYellowCards)]]

# Average Yellow cards
mean(sumYellowCards)

# A function to get the array elements that is above average
getAboveAverageArrayNames <- function (x) {
  average <- mean(x)
  print(paste('Average is: ', average))
  return (names(x[x > average]))
}

# Which team got above average yellow cards?
aboveAverageYellowCardTeamNames = getAboveAverageArrayNames(sumYellowCards);aboveAverageYellowCardTeamNames
length(aboveAverageYellowCardTeamNames)
sumYellowCards[aboveAverageYellowCardTeamNames]

# Red cards
# HR = Home Team Red Cards
# AR = Away Team Red Cards
homeSumRedCards = tapply(epldata[['HR']], homeTeamFactor, sum);homeSumRedCards
awaySumRedCards = tapply(epldata[['AR']], homeTeamFactor, sum);awaySumRedCards

sumRedCards = homeSumRedCards + awaySumRedCards;sumRedCards

# Team that got the most yellow cards in the whole season.
which.max(sumRedCards);sumRedCards[[which.max(sumRedCards)]]

# Teams that got no Red cards in the whole season
sumRedCards[sumRedCards == 0]

# Which team won in the season

# FTR: Full Time Result. H = Home Team Win, A = Away Team Win, D = Draw

# Functions that calculate the points of the home/away teams based on the record values.
calculateHomeTeamPoints = function(x) {
  homeWins = length(x[x == 'H'])
  draws = length(x[x == 'D'])
  homePoints = homeWins * 3
  drawPoints = draws * 1
  points = homePoints + drawPoints
  return (points)
}

calculateAwayTeamPoints = function(x) {
  awayWins = length(x[x == 'A'])
  draws = length(x[x == 'D'])
  awayPoints = awayWins * 3
  drawPoints = draws * 1
  points = awayPoints + drawPoints
  return (points)
}

sumHomeTeamPoints = tapply(epldata[['FTR']], homeTeamFactor, calculateHomeTeamPoints);sumHomeTeamPoints
sumAwayTeamPoints = tapply(epldata[['FTR']], awayTeamFactor, calculateAwayTeamPoints);sumAwayTeamPoints

sumTeamPoints = sumHomeTeamPoints + sumAwayTeamPoints;sumTeamPoints

# Who is the 1st place(won) in the whole season?
sumTeamPoints[which.max(sumTeamPoints)]

# Who is the last place in the whole season?
sumTeamPoints[which.min(sumTeamPoints)]

# Create Visualizations
# Step 1: Create tibbles
fullTimeShots = tibble(data.frame(teams, shots=fullTimeSumShots));fullTimeShots
fullTimeShotsOnTarget = tibble(data.frame(teams, shots=fullTimeSumShotsOnTarget));fullTimeShotsOnTarget
teamGoals = tibble(data.frame(teams, shots=teamSumGoals));teamGoals
teamCorners = tibble(data.frame(teams, corners=fullTimeSumCorner));teamCorners
yellowCards = tibble(data.frame(teams, cards=sumYellowCards));yellowCards
redCards = tibble(data.frame(teams, cards=sumRedCards));redCards

# Step 2: Create plots
# Full Time Shots Barplot
ggplot(fullTimeShots, aes(teams,shots, fill=rainbow(length(teams)))) +
  geom_col(show.legend = FALSE) +
  geom_hline(aes(yintercept=mean(fullTimeSumShots))) +
  labs(title = 'Full Time Shots', subtitle = 'Total Shots of every team in the whole season', y = 'Shots', x = 'Team Name') +
  coord_flip()

# Full Time Shots on Target Barplot
ggplot(fullTimeShotsOnTarget, aes(teams,shots, fill=rainbow(length(teams)))) +
  geom_col(show.legend = FALSE) +
  geom_hline(aes(yintercept=mean(fullTimeSumShotsOnTarget))) +
  labs(title = 'Full Time Shots on Target', subtitle = 'Total Shots on Target of every team in the whole season', y = 'Shots', x = 'Team Name') +
  coord_flip()

# Full Time Goals Barplot
ggplot(teamGoals, aes(teams,shots, fill=rainbow(length(teams)))) +
  geom_col(show.legend = FALSE) +
  geom_hline(aes(yintercept=mean(teamSumGoals))) +
  labs(title = 'Full Time Goals', subtitle = 'Total Goals of every team in the whole season', y = 'Shots', x = 'Team Name') +
  coord_flip()

# Team Corner Barplot
ggplot(teamCorners, aes(teams,corners, fill=rainbow(length(teams)))) +
  geom_col(show.legend = FALSE) +
  geom_hline(aes(yintercept=mean(fullTimeSumCorner))) +
  labs(title = 'Team Corners', subtitle = 'Total Corner of every team in the whole season', y = 'Corners', x = 'Team Name') +
  coord_flip()

# Team Yellow Cards Barplot
ggplot(yellowCards, aes(teams,cards, fill=rainbow(length(teams)))) +
  geom_col(show.legend = FALSE) +
  geom_hline(aes(yintercept=mean(sumYellowCards))) +
  labs(title = 'Team Yellow Cards', subtitle = 'Total Yellow Cards of every team in the whole season', y = 'Yellow Cards', x = 'Team Name') +
  coord_flip()

# Team Red Cards Barplot
ggplot(redCards, aes(teams,cards, fill=rainbow(length(teams)))) +
  geom_col(show.legend = FALSE) +
  geom_hline(aes(yintercept=mean(sumRedCards))) +
  labs(title = 'Team Red Cards', subtitle = 'Total Red Cards of every team in the whole season', y = 'Red Cards', x = 'Team Name') +
  coord_flip()


# Perform point analysis of each team.
calculateHomeWins = function(x) {
  return (length(x[x == 'H']))
}

calculateAwayWins = function(x) {
  return (length(x[x == 'A']))
}

calculateDraws = function(x) {
  return (length(x[x == 'D']))
}

homeTeamWins = tapply(epldata[['FTR']], homeTeamFactor, calculateHomeWins);homeTeamWins
awayTeamWins = tapply(epldata[['FTR']], awayTeamFactor, calculateAwayWins);awayTeamWins
homeTeamDraws = tapply(epldata[['FTR']], homeTeamFactor, calculateDraws);homeTeamDraws
awayTeamDraws = tapply(epldata[['FTR']], awayTeamFactor, calculateDraws);awayTeamDraws

stats = data.frame(teamNames=teams, homeWins=homeTeamWins*3, awayWins=awayTeamWins*3, homeDraws=homeTeamDraws, awayDraws=awayTeamDraws);stats

teamDraws2 = melt(stats);teamDraws2

length(stats$teamNames)

# Stacked barplot
ggplot(teamDraws2, aes(fill=variable, y=value, x=teamNames)) + 
  geom_bar(stat="identity") +
  coord_flip()

# Grouped barplot
ggplot(teamDraws2, aes(fill=variable, y=value, x=teamNames)) + 
  geom_bar(position="dodge", stat="identity") +
  coord_flip()

# Pearson Correlations Relationship between 2 variables

# Shots vs Shots on Target
cor(fullTimeShots$shots, fullTimeShotsOnTarget$shots)

# Shots On Target vs Goals
cor(fullTimeShotsOnTarget$shots, teamGoals$shots)

# Corner vs Goals
cor(teamCorners$corners, teamGoals$shots)

# Yellow cards vs Red cards
cor(yellowCards$cards, redCards$cards)

