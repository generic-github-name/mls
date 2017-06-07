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
inFile3 = './webscrape/all_matches_in_mls_website.csv'

# shapefile of field
outLines = './_common/Soccer_Fields/soccer_field.jpg'

# analytical resolution (pixel size)
r = 1

# output file
outFile = paste0('./defensive_actions/team_surfaces',r,'.pdf')
outFileEst = paste0('./defensive_actions/team_surfaces',r,'.rdata')

# name conversions
source('./_common/convert_from_asa_names.r')
source('./_common/formalize_team_names.r')
# ------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Load/prep data

# load
data = fread(inFile1)
data = rbind(data, fread(inFile2))

# format
data[, date:=as.Date(date, '%m/%d/%Y')]
data = convertTeamNamesASA(data)

# extra terms for ease of use
data[, x2:=x^2]
data[, y2:=y^2]

# aggregate pixels
data[, x:=round(x/r)*r]
data[, y:=round(y/r)*r]
byVars = c('x','y','x2','y2','team','date','player')
data = data[, list(actions=.N), by=byVars]

# bring in goals scored
matchData = fread(inFile3)
matchData = matchData[competition %in% c('MLS Regular Season')]
matchData$competition = NULL
matchData = matchData[date>=min(data$date) & date<=max(data$date)]

# reshape to team level
matchData = melt(matchData, id.vars=c('date','winner'),measure.vars=patterns('team_*','score_*'), value.name=c('team','goals'), variable.name='home')
matchData[, home:=as.character(home)]
matchData[home=='1', home:='away']
matchData[home=='2', home:='home']
matchData[, date:=as.Date(date)]

# merge results (dates aren't perfect so many observations get dropped)
data = merge(data, matchData, by=c('team','date'), all.x=TRUE)
data[is.na(goals), date:=date-1]
data = merge(data, matchData, by=c('team','date'), all.x=TRUE, suffixes=c('','_update'))
data[is.na(goals), c('goals','home','winner'):=list(goals_update, home_update, winner_update)]
data[, c('goals_update','home_update','winner_update'):=list(NULL, NULL, NULL)]

# aggregate out players
byVarsT = byVars[byVars!='player']
teamData = data[, list(actions=sum(actions)), by=byVarsT]
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Run models

# player-level models (separately because it gets too big)
teamFits = list()
teams = unique(data$team)
for(t in seq_along(teams)) {
	print(t)
	tmpData = teamData[team==teams[t]]
	nGames = length(unique(data[team==teams[t]]$date))
	if (nrow(tmpData)<=3) next
	grid = data.table(expand.grid(seq(0,100,by=r), seq(0,100,by=r), unique(tmpData$date)))
	setnames(grid, c('x','y','date'))
	tmpData = merge(tmpData, grid, by=c('x','y','date'), all=TRUE)
	tmpData[is.na(actions), actions:=0]
	tmpData[, actions_pg:=actions/nGames]
	tmpData[, x2:=x^2]
	tmpData[, y2:=y^2]
	teamFits[[t]] = glm.nb(actions_pg ~ y*x*y2*x2*date, tmpData)
}
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Predict
predDates = unique(data$date)
cfData = data.table(expand.grid(seq(0,100, by=r), seq(0,100, by=r), predDates, teams))
setnames(cfData, c('x','y','date','team'))
cfData = merge(cfData, unique(data[,c('team','date'),with=FALSE]), by=c('team','date'))
cfData[, x2:=x^2]
cfData[, y2:=y^2]
for(t in seq_along(teams)) {
	if (is.null(teamFits[[t]])) next
	cfData[team==teams[t], pred:=exp(predict(teamFits[[t]], newdata=cfData[team==teams[t]]))]
}
cfData = cfData[!is.na(pred)]
cfData[, pred_std:=(pred-mean(pred))/sd(pred), by=c('team','date')]
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
plots = list()
for(t in seq_along(teams)) {
	team = teams[t]
	plots[[t]] = ggplot(cfData[team==teams[t]], aes(y=y,x=x,fill=pred*f)) + 
		geom_tile() + 
		draw_grob(lines, x=-10, y=-9, width=120, height=118) + # this is bad. I have to match the location/scale to the aspect ratio of the output
		facet_wrap(~date) + 	
		scale_fill_gradientn(paste('Actions\n Per',f,'\nGames\n(Per Pixel)'), colors=colors, na.value='grey75') +
		labs(title='Expected Number of Defensive Actions', y='Defensive End', subtitle=team) + 
		theme_minimal()	+ 
		theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_blank(), 
		axis.ticks=element_blank(), plot.title=element_text(hjust=.5, size=16), plot.subtitle=element_text(hjust=.5, size=14), 
		line = element_blank())
}
# ---------------------------------------------------------------------------


# -------------------------------
# Save
pdf(outFile, height=6, width=8.4)
for(t in seq_along(teams)) {
	print(plots[[t]])
}
dev.off()

save(cfData, file=outFileEst)
# -------------------------------
