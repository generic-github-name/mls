# Most fouled and least dispossessed players


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(RColorBrewer)
# --------------------


# -------------------------------------------------------------------------
# Files/directories/lists

# input files
inFileDribbles = './webscrape/ASA/2017/dribbles.csv'
inFilePasses = './webscrape/ASA/2017/raw passes.csv'
inFileFoulsS = './webscrape/ASA/2017/raw fouls suffered.csv'
# -------------------------------------------------------------------------


# ---------------------------------------------------------# Load data


# load
dribbles = fread(inFileDribbles)
passes = fread(inFilePasses)
foulsS = fread(inFileFoulsS)

# identify actions
dribbles[, action:='dribble']
passes[, action:='pass']
foulsS[, action:='foul_suffered']

# rename
setnames(passes, c('success','passer'), c('outcome','player'))
setnames(dribbles, 'success', 'outcome')

# append
data = rbind(dribbles, passes, foulsS, fill=TRUE)
# ---------------------------------------------------------


# ---------------------------------------------------------
# Rank

# identify dispossessions
data[, dispossessed:=(action=='dribble' | action=='pass') & outcome==0]

# rank fouls suffered
fouled = data[action=='foul_suffered', .N, by='player']
fouled[order(-N)]

# rank dispossessions
dispossessed = data[action!='foul_suffered', list(sum(dispossessed), .N), by='player']
dispossessed[, rate:=V1/N]
dispossessed = dispossessed[order(rate)]
dispossessed[, rank:=seq_len(.N)]
dispossessed[N>250][order(-rate)]
dispossessed[player=='Nicolas Lodeiro']
# ---------------------------------------------------------