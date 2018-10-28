# touch count and variance
# 9/30/2018


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(RColorBrewer)
library(ggplot2)
# --------------------


# --------------------
# Settings/parameters
p ='Nicolas Lodeiro'
g = 1253013
# --------------------


# ---------------------------------------------------------------
# Files and directories

# input files
inFileDribbles = './webscrape/ASA/2018/dribbles.csv'
inFilePasses = './webscrape/ASA/2018/raw passes.csv'
inFileShots = './webscrape/ASA/2018/raw shots.csv'
inFileDefense = './webscrape/ASA/2018/raw defensive actions.csv'
inFileClearances = './webscrape/ASA/2018/raw clearances.csv'
inFileRecoveries = './webscrape/ASA/2018/raw recoveries.csv'

# output file
graphFile = './touches/graphs.pdf'
# ---------------------------------------------------------------


# -------------------------------------------------------------------------------------
# Load/prep data

# load
dribbles = fread(inFileDribbles)
passes = fread(inFilePasses)
shots = fread(inFileShots)
defense = fread(inFileDefense)
clearances = fread(inFileClearances)
recoveries = fread(inFileRecoveries)

# identify actions
dribbles[, action:='dribble']
passes[, action:='pass']
shots[, action:='shot']
# defense[, action:='defense']

# rename
setnames(dribbles, 'success', 'outcome')
setnames(passes, 'passer', 'player')
setnames(shots, 'shooter', 'player')

# append
data = rbind(dribbles, passes, shots, defense, clearances, recoveries, fill=TRUE)

# subset variables
vars = c('gameID', 'date', 'time', 'player', 'team', 'team.1', 'x', 'y', 'action', 'outcome')
data = data[, vars, with=FALSE]

# format time variable
time = str_split(data$time, ':', 3)
time = data.table(do.call('rbind', time))
time[, V1:=as.numeric(gsub(' ', '', V1))]
time[, V2:=as.numeric(gsub(' ', '', V2))]
m = time$V1 + (time$V2/60)
data[, minute:=m]

# convert x and y to meters from the origin
# assuming a field size of 105x70
data[, x:=(x/100)*105]
data[, y:=(y/100)*70]
# -------------------------------------------------------------------------------------


# --------------------------------------------------------------------
# Analysis

data[player==p & gameID==g]

# count actions
data[, total_actions:=.N, by='player']
data[, game_actions:=.N, by=c('gameID','player')]

# action location variances
data[, x_sd_total:=sd(x), by='player']
data[, y_sd_total:=sd(y), by='player']
data[, x_sd_game:=sd(x), by=c('gameID','player')]
data[, y_sd_game:=sd(y), by=c('gameID','player')]

# distance covered
data = data[order(player, date, minute)]
data[, x_lag:=shift(x), by=c('gameID','player')]
data[, y_lag:=shift(y), by=c('gameID','player')]
data[, distance:=sqrt(((x-x_lag)^2)+((y-y_lag)^2))]
data[, distance_miles:=distance*0.000621371] SOMETHING WRONG HERE

# total touches and touches per game
data[, .N, by='player'][order(-N)][1:20]
data[, mean(game_actions), by='player'][order(-V1)][1:20]
length(unique(data$player))

# variance of touch location
unique(data[total_actions>500, c('player','x_sd_total')])[order(-x_sd_total)][1:20]
unique(data[total_actions>500, c('player','y_sd_total')])[order(-y_sd_total)][1:20]
unique(data[game_actions>50, c('player','x_sd_game')])[order(-x_sd_game)][1:20]
unique(data[game_actions>50, c('player','y_sd_game')])[order(-y_sd_game)][1:20]
# --------------------------------------------------------------------
