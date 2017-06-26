# Stoppage time goals


# set up R
rm(list=ls())
library(data.table)
library(reshape2)
library(ggplot2)
library(RColorBrewer)

# files and directories
inFile = './webscrape/boxscores_for_every_match.csv'
outFile = './stoppage_time_goals/stoppage_time_goals.pdf'

# load
data = fread(inFile)

# subset columns
data = data[, c('player','date','team','team_home','team_away',paste0('goal_',seq(4))), with=FALSE]

# melt goals long
data = melt(data, id.vars=c('player','date','team','team_home','team_away'), value.name='minute')

# drop empty rows
data = data[!is.na(minute) & minute!='']

# convert minutes to numeric
data[, stoppage:=grepl('\\+', minute)]
data[minute=='', minute:=NA]
data[minute=='HT', minute:='45']
data[minute=='EHT', minute:='45']
data[minute=='EOR', minute:='90']
data[, minute:=gsub('\'', '', minute)]
newVar = sapply(data$minute, function(x) eval(parse(text=x)))
data[, minute:=newVar]

# only look at end-of-game stoppage time (not halftime or first half of OT)
data[minute<90 | (minute>105 & minute<120), stoppage:=FALSE]
