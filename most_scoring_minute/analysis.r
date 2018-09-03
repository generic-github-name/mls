# Most-scoring minutes

# -------------------------
# set up R
rm(list=ls())
library(data.table)
library(ggplot2)
# -------------------------


# --------------------------------------------------------
# input/output files
inFileHistorical = './webscrape/boxscores_for_every_match.csv'
inFile2017 = './webscrape/ASA/2017/raw shots.csv'
inFile2018 = './webscrape/ASA/2018/raw shots.csv'
outFile = './most_scoring_minute/graphs.pdf'
tableFile = './most_scoring_minute/ppg_table.csv'
source('./_common/formalize_team_names.r')
# --------------------------------------------------------


# ----------------------------------------------------------------------------------
# load data
oldData = fread(inFileHistorical)
newData = fread(inFile2017)
newData = rbind(newData, fread(inFile2018))

# subset to seattle only in each
oldData = oldData[team=='seattle']
newData = newData[team=='Seattle']

# subset to goals only in each
oldData = melt(oldData, id.vars='date', measure.vars=c('goal_1','goal_2','goal_3','goal_4'))
oldData = oldData[!is.na(value)]
newData = newData[result=='Goal']

# format dates
oldData[, date:=as.Date(date)]
newData[, date:=as.Date(date, '%m/%d/%Y')]

# convert goal minutes to numeric
oldData[, c('minute','et'):=tstrsplit(value, '\'', fixed=TRUE)]
oldData[, et:=gsub('\\+', '', et)]
oldData[, minute:=as.numeric(minute)]
oldData[!is.na(et), minute:=minute+as.numeric(et)]
newData[, c('minute','second'):=tstrsplit(time, ':', fixed=TRUE)]
newData[, minute:=as.numeric(minute)+(as.numeric(second)/60)]
newData[, minute:=round(minute)]

# subset
oldData = oldData[, c('date','minute')]
newData = newData[, c('date','minute')]

# rbind
data = rbind(newData, oldData)
# ----------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------
# Look up most-scoring minute

data[, .N, by='minute'][order(N)]

# ----------------------------------------------------------------------------------

