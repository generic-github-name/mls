# Explore goal difference with each player on the field


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(stringr)
# --------------------


# -------------------------------------------------------------------------
# Files/directories/lists

# input file
inFile = './webscrape/boxscores_for_every_match.csv'

# match-level file
matchFile = './webscrape/all_matches_in_mls_website.csv'

# output file
outFile = './refs/graphs.pdf'

# formalization function
source('./_common/formalize_team_names.r')
# -------------------------------------------------------------------------


# --------------------------------------------------------------------------------
# Load/prep data

# laod 
data = fread(inFile)

# only mls competitions
matchData = fread(matchFile)
comps = c('MLS Cup', 'MLS Playoffs', 'MLS Regular Season')
data = merge(data, matchData, by=c('team_home','team_away','date'), all.x=TRUE)
data = data[competition %in% comps]
setkey(data, NULL)

# extra variables
data[, home:=team==team_home]
data[home==TRUE, opponent:=team_away]
data[home==FALSE, opponent:=team_home]
data[home==TRUE, gd:=score_home-score_away]
data[home==FALSE, gd:=score_away-score_home]
data[, total_minutes:=sum(minutes), by=c('team','players')]

# keep only players and goals
idVars = c('team','opponent','home','date')
data = data[, c(idVars,'score_home','score_away','gd','players','minutes','total_minutes'), with=FALSE]

# isolate current players
X1 = 90
seattle = data[team=='seattle' & total_minutes>X1]
seattleRoster = unique(seattle[year(date)==2017]$players)
seattle$total_minutes = NULL
seattleAllTime = copy(seattle)
seattle = seattle[players %in% seattleRoster]

# keep only players with more than X minutes
X2 = 180
data = data[total_minutes>X2]
# --------------------------------------------------------------------------------


# --------------------------------------------------------------------------------
# Analyze

# simple averages
seattle[minutes>45, list(gd=mean(gd), sd=sd(gd), minutes=sum(minutes),games=.N), by='players'][order(-gd)]
seattleAllTime[minutes>45, list(gd=mean(gd), sd=sd(gd), minutes=sum(minutes),games=.N), by='players'][order(-gd)]

# --------------------------------------------------------------------------------
