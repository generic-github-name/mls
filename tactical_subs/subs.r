# analyze the best time to make each sub, regardless of what type of sub is made, controlling for game state (gd)

# ------------------
# Set up R
rm(list=ls())
library(data.table)
library(stringr)
library(ggplot2)
# ------------------

g1=1256487
g2=1256490
# --------------------------------------------
# Files and directories

# sub file
subFile = './webscrape/ASA/2018/playerChanges.csv'

# goals files
shotsFile = './webscrape/ASA/2018/raw shots.csv'
ogFile = './webscrape/ASA/2018/raw owngoals.csv'

# --------------------------------------------


# ---------------------------------------------------------------------------------
# Load/prep gamestate data

# load shots
shots = fread(shotsFile)

# keep only goals
shots = shots[result=='Goal']

# load OGs
ogs = fread(ogFile)

# append goals and owngoals
goals = rbind(shots, ogs)
goals = goals[order(date, team, time)]

# identify game state (because there appear to be errors in hscore and ascore)
goals[team==hteam, home_goal:=1]
goals[team==ateam, away_goal:=1]
goals[, home_score:=cumsum(home_goal), by=gameID]
goals[, away_score:=cumsum(away_goal), by=gameID]
goals[, home_score:=cumsum(ifelse(is.na(home_goal), 0, home_goal)), by=gameID]
goals[, away_score:=cumsum(ifelse(is.na(away_goal), 0, away_goal)), by=gameID]
goals[, hgd:=home_score-away_score]
goals[, agd:=away_score-home_score]

# subset down to just gamestate info
gamestate = goals[, c('date','gameID','time','hteam','ateam','hgd','agd')]

# reshape to team level
idVars = c('date','gameID','time')
gamestate = melt(gamestate, id.vars=idVars)
# gamestate[grepl('team',variable), variable:='team']
# gamestate[grepl('gd',variable), variable:='gd']
dt1 = gamestate[grepl('team',variable),c(idVars, 'variable', 'value'),with=F]
dt2 = gamestate[grepl('gd',variable),c(idVars, 'variable', 'value'),with=F]
gamestate = merge(dt1, dt2, idVars)
gamestate = gamestate[str_sub(variable.x,1,1)==str_sub(variable.y,1,1)]
gamestate$variable.y=NULL
setnames(gamestate, c('variable.x','value.x','value.y'), c('location','team','gd'))
# ---------------------------------------------------------------------------------


# --------------------------------------------
# Load/prep sub data

# load
subs = fread(subFile)

# subset to only one of the on/off
subs = subs[on==1, c('gameID', 'time','team')]
subs[, sub:=1]

# add to the gamestate data
data = rbind(gamestate, subs, fill=TRUE)

# format time variable
time = str_split(data$time, ':', 3)
time = data.table(do.call('rbind', time))
time[, V1:=as.numeric(gsub(' ', '', V1))]
time[, V2:=as.numeric(gsub(' ', '', V2))]
m = time$V1 + (time$V2/60)
data[, minute:=m+1] # +1 for the "zeroeth" minute
data = data[order(date, gameID, minute)]

# isolate the gd at the time of each sub


# figure out the final result of each game

# --------------------------------------------


# --------------------------------------------
# --------------------------------------------


# --------------------------------------------
# --------------------------------------------


# --------------------------------------------
# --------------------------------------------

