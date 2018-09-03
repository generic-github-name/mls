# Track all the actions in order
# 8/14/2017


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(RColorBrewer)
library(ggplot2)
# --------------------


# ----------------------------------------------
# Parameters and settings

# narrow down to one team
t = 'Seattle'

# narrow down to one game
d = '07/05/2017'
# ----------------------------------------------


# ---------------------------------------------------------------
# Files and directories

# input files
inFileDribbles = './webscrape/ASA/2017/dribbles.csv'
inFilePasses = './webscrape/ASA/2017/raw passes.csv'
inFileShots = './webscrape/ASA/2017/raw shots.csv'
inFileFoulsC = './webscrape/ASA/2017/raw fouls committed.csv'
inFileFoulsS = './webscrape/ASA/2017/raw fouls suffered.csv'
inFileDefense = './webscrape/ASA/2017/raw defensive actions.csv'

# output file
graphFile = './player_movement/graphs.pdf'
# ---------------------------------------------------------------


# -------------------------------------------------------------------------------------
# Load/prep data

# load
dribbles = fread(inFileDribbles)
passes = fread(inFilePasses)
shots = fread(inFileShots)
foulsC = fread(inFileFoulsC)
foulsS = fread(inFileFoulsS)
defense = fread(inFileDefense)

# identify actions
dribbles[, action:='dribble']
passes[, action:='pass']
shots[, action:='shot']
foulsC[, action:='foul_committed']
foulsS[, action:='foul_suffered']
defense[, action:='defense']

# append
data = rbind(dribbles, passes, shots, foulsC, foulsS, defense, fill=TRUE)

# subset variables
vars = c('date', 'time', 'player', 'team', 'team.1', 'x', 'y', 'action', 'outcome')
data = data[, vars, with=FALSE]

# subset to just the selected player
data = data[team.1==t & date==d]

# format time variable
time = str_split(data$time, ':', 3)
time = data.table(do.call('rbind', time))
time[, V1:=as.numeric(gsub(' ', '', V1))]
time[, V2:=as.numeric(gsub(' ', '', V2))]
m = time$V1 + (time$V2/60)
data[, minute:=m]

# sort
data = data[order(minute)]
# -------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------
# Expand to minute-level to have the path fade out with inactivity

# expand leaving everything else blank for in-between times
minutes = data.table(minute=seq(0,125))
expandedData = rbind(data, minutes, fill=TRUE)
expandedData = expandedData[order(minute)]

# identify how long it has been since the last activity
expandedData[!is.na(action), most_recent:=minute]
for(i in 2:nrow(expandedData)) {
	if (is.na(expandedData[i]$most_recent)) {
		expandedData[i, most_recent:=expandedData[i-1]$most_recent]
	}
}
expandedData[, time_since_last:=minute-most_recent]

# identify how long it will be until the next activity
expandedData[!is.na(action), next_action:=minute]
for(i in (nrow(expandedData)-1):1) {
	if (is.na(expandedData[i]$next_action)) {
		expandedData[i, next_action:=expandedData[i+1]$next_action]
	}
}
expandedData[, time_until_next:=next_action-minute]

# put the two together
expandedData[time_since_last<=time_until_next, inactivity:=time_since_last]
expandedData[time_since_last>time_until_next, inactivity:=time_until_next]

# interpolate between actions for the path
idx = which(!is.na(expandedData$action))
for(i in seq(length(idx))) {
	i1 = idx[i]
	i2 = idx[i+1]
	if (is.na(i2)) next
	x1 = expandedData[i1]$x
	x2 = expandedData[i2]$x
	y1 = expandedData[i1]$y
	y2 = expandedData[i2]$y
	n = i2-i1+1
	expandedData[i1:i2, x:=seq(x1, x2, length=n)]
	expandedData[i1:i2, y:=seq(y1, y2, length=n)]
}

# drop na
expandedData = expandedData[!is.na(x)]
# -------------------------------------------------------------------------------------


# ----------------------------------------------
# Graph

# colors
cols = brewer.pal(11, 'Spectral')

# store graph
ggplot(expandedData, aes(y=y, x=x, color=minute, alpha=log(inactivity+.5))) + 
	geom_path(size=2) + 
	# geom_point(data=data, aes(shape=action, alpha=NULL, color=NULL)) + 
	scale_color_gradientn('Minute', colors=cols) + 
	scale_alpha_continuous('', range = c(.75, 0), guide='none') + 
	# scale_shape_manual('Action', values=c(7, 19, 4, 8, 10)) + 
	labs(caption='Path faded to show periods of inactivity') + 
	theme_bw()
# ----------------------------------------------


# # --------------------------------
# # Save graphs
# pdf(graphFile, height=6, widatah=9)
# p
# dev.off()
# # --------------------------------
