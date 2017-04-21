# 4/13/2017
# Spatial analysis of defensive actions by player
# Note: cowplot requires R 3.3 or higher


# ------------------
# Set up R
rm(list=ls())
library(data.table)
library(sp)
library(maptools)
library(rgeos)
library(MASS)
library(boot)
library(lme4)
library(ggplot2)
library(cowplot)
library(grid)
library(jpeg)
library(RColorBrewer)
# ------------------


# ------------------------------------------------------------------
# Files, directories and settings

# input file
inFile1 = './webscrape/ASA/raw defensive actions_2016.csv'
inFile2 = './webscrape/ASA/raw defensive actions_2017.csv'

# shapefile of field
outLines = './_common/Soccer_Fields/soccer_field.jpg'

# analytical resolution (pixel size)
r = 1

# output file
outFile = paste0('./defensive_actions/player_surfaces',r,'.pdf')
# ------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Load/prep data

# load
data = fread(inFile1)
data = rbind(data, fread(inFile2))

# format
data[, date:=as.Date(date, '%m/%d/%Y')]
	
# extra terms for ease of use
data[, xy:=x*y]	
data[, x2:=x^2]
data[, y2:=y^2]
data[, x3:=x^3]
data[, y3:=y^3]

# aggregate pixels
data[, x:=round(x/r)*r]
data[, y:=round(y/r)*r]
byVars = c('x','y','x2','y2','x3','team','date','player')
data = data[, list(actions=.N), by=byVars]

# aggregate out date
byVarsP = byVars[byVars!='date']
playerData = data[, list(actions=.N), by=byVarsP]
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Run models

# player-level models (separately because it gets too big)
playerFits = list()
players = unique(data[team=='Seattle' & year(date)==2017]$player)
for(p in seq_along(players)) {
	tmpData = playerData[player==players[p]]
	nGames = length(unique(data[player==players[p]]$date))
	if (nrow(tmpData)<=3) next
	grid = data.table(expand.grid(seq(0,100,by=r), seq(0,100,by=r)))
	setnames(grid, c('x','y'))
	tmpData = merge(tmpData, grid, by=c('x','y'), all=TRUE)
	tmpData[is.na(actions), actions:=0]
	tmpData[, actions_pg:=actions/nGames]
	tmpData[, xy:=x*y]	
	tmpData[, x2:=x^2]
	tmpData[, y2:=y^2]
	tmpData[, x3:=x^3]
	tmpData[, y3:=y^3]
	playerFits[[p]] = tryCatch(glm.nb(actions_pg ~ y*x*y2*x2, tmpData), error=function(e) NULL)
}
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Predict
cfData = data.table(expand.grid(seq(0,100, by=r), seq(0,100, by=r), players))
setnames(cfData, c('x','y','player'))
cfData[, x2:=x^2]
cfData[, y2:=y^2]
cfData[, x3:=x^3]
cfData[, y3:=y^3]
for(p in seq(length(playerFits))) {
	if (is.null(playerFits[[p]])) next
	cfData[player==players[p], pred:=exp(predict(playerFits[[p]], newdata=cfData[player==players[p]]))]
}
cfData = cfData[!is.na(pred)]
cfData[, pred_std:=(pred-mean(pred))/sd(pred), by='player']
q5 = quantile(cfData$pred_std, p=.025)
q95 = quantile(cfData$pred_std, p=.975)
cfData[pred_std<q5, pred_std:=q5]
cfData[pred_std>q95, pred_std:=q95]
cfData[, pred_trimmed:=pred]
cfData[pred<.0001, pred_trimmed:=NA]
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Load field outLine

# load logo and store as grob
lines = readJPEG(outLines)
lines = rasterGrob(lines, interpolate=TRUE)

# manually replace white to transparent
colors = data.table(as.matrix(lines$raster))
for(var in names(colors)) colors[get(var)=='#FFFFFF', (var):='#00000000']
lines$raster = as.raster(as.matrix(colors))
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Graph analysis

# colors
colors = brewer.pal(8, 'RdYlGn')

# factor to make expected actions integer-scale numbers
f = 34

# make heatmaps
p1 = ggplot(cfData, aes(y=y,x=x,fill=pred_trimmed*f)) + 
	geom_tile() + 
	draw_grob(lines, x=-10, y=-9, width=120, height=118) + # this is bad. I have to match the location/scale to the aspect ratio of the output
	facet_wrap(~player) + 	
	scale_fill_gradientn(paste('Actions\n Per',f,'\nGames\n(Per Pixel)'), colors=colors, na.value='grey75') +
	labs(title='Expected Number of Defensive Actions', y='Defensive End') + 
	theme_minimal()	+ 
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_blank(), 
	axis.ticks=element_blank(), plot.title=element_text(hjust=.5, size=16), line = element_blank())
p2 = ggplot(cfData, aes(y=y,x=x,fill=pred_std)) + 
	geom_tile() + 
	draw_grob(lines, x=-10, y=-9, width=120, height=118) + # this is bad. I have to match the location/scale to the aspect ratio of the output
	facet_wrap(~player) + 	
	scale_fill_gradientn('Relative\nActions', colors=colors, na.value='grey75') +
	labs(title='Expected Number of Defensive Actions (Relative to Individual Player)', y='Defensive End') + 
	theme_minimal()	+ 
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_blank(), 
	axis.ticks=element_blank(), plot.title=element_text(hjust=.5, size=16), line = element_blank())
# ---------------------------------------------------------------------------


# -------------------------------
# Save
pdf(outFile, height=6, width=8.4)
p1
p2
dev.off()
# -------------------------------
