# 4/12/2017
# Spatial analysis of defensive actions for a single team with and without a particular player
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
library(chron)
library(gridExtra)
# ------------------


# --------------------------------------------------------------------------
# Files, directories and settings

# team
te = 'Seattle'

# analytical resolution (pixel size in the models)
r = 5

# prediction resolution (pixel size in the graphs)
r2 = 1

# input files
inFile1 = paste0('./webscrape/ASA/2016/raw defensive actions.csv')
inFile2 = paste0('./webscrape/ASA/2017/raw defensive actions.csv')

# substitution data
boxScoreFile = './webscrape/boxscores_for_every_match.csv'

# shapefile of field
outLines = './_common/Soccer_Fields/soccer_field.jpg'

# output file
outFile = paste0('./defensive_actions/defensive_actions_',r,'_',te,'.pdf')
# --------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Load/prep opta data

# load
# data1 = fread(inFile1)
# data2 = fread(inFile2)
# data = rbind(data1, data2)
data = fread(inFile2)

# subset
data = data[team==te]

# format dates/minutes
data[, date:=as.Date(date, '%m/%d/%Y')]
data[, c('minute', 'second'):=tstrsplit(time, ':', fixed=TRUE)]
data[, minute:=as.numeric(minute) + as.numeric(second)/60]

# extra term for ease of use
# data[, xy:=x*y]	
# data[, x2:=x^2]
# data[, y2:=y^2]
# data[, x3:=x^3]
# data[, y3:=y^3]
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Prep substitution data

# load
boxScores = fread(boxScoreFile)

# subset
vars = c('player','team','date','team_away','team_home','minutes','subbed_in','subbed_out')
boxScores = boxScores[team=='seattle', vars, with=FALSE]

# match names to opta
boxScores[player=='Nélson Valdez', player:='Nelson Valdez']
boxScores[player=='Román Torres', player:='Roman Torres']
boxScores[player=='Hérculez Gómez', player:='Herculez Gomez']
boxScores[player=='Nicolás Lodeiro', player:='Nicolas Lodeiro']
boxScores[player=='Álvaro Fernández', player:='Alvaro Fernandez']

# format dates
boxScores[, date:=as.Date(date, '%Y-%m-%d')]

# keep after 2016
boxScores = boxScores[year(date)>=2017]

# convert to numeric
for(v in c('subbed_in','subbed_out')) {
	boxScores[get(v)=='', (v):=NA]
	boxScores[get(v)=='HT', (v):='45']
	boxScores[get(v)=='EHT', (v):='45']
	boxScores[get(v)=='EOR', (v):='90']
	boxScores[, (v):=gsub('\'', '', get(v))]
	newVar = sapply(boxScores[[v]], function(x) eval(parse(text=x)))
	boxScores[, (v):=newVar]
}

# fix missing substitution boxScores with assumptions
boxScores[minutes<45 & is.na(subbed_in) & is.na(subbed_out), subbed_in:=93-minutes]
boxScores[minutes>=45 & minutes<90 & is.na(subbed_in) & is.na(subbed_out), subbed_out:=minutes]

# add bounds for starters, enders, did not plays
boxScores[minutes>0 & is.na(subbed_in), subbed_in:=0]
boxScores[minutes>0 & is.na(subbed_out), subbed_out:=Inf]
boxScores[minutes==0, subbed_in:=Inf]
boxScores[minutes==0, subbed_out:=-Inf]

# set aside the minute counts for each player and the whole team
boxScores[, total_minutes:=sum(minutes), by='player']
playerMinutes = unique(boxScores[, c('player','total_minutes'), with=FALSE])
minutes = 90*length(unique(boxScores$date))
playerMinutes[,minutes_missed:=minutes-total_minutes]

# keep only players with >m minutes
m = 50
players = unique(playerMinutes[total_minutes>m & minutes_missed>m]$player)
boxScores = boxScores[player %in% players]

# reshape players wide
boxScores = dcast.data.table(boxScores, date ~ player, value.var=c('subbed_in','subbed_out'))

# add bounds for did not plays again now that the dataset is square
for(p in players) {
	boxScores[is.na(get(paste0('subbed_in_', p))), (paste0('subbed_in_', p)):=Inf]
	boxScores[is.na(get(paste0('subbed_out_', p))), (paste0('subbed_out_', p)):=-Inf]

}
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Merge boxscores to opta data

# players in opta data but not boxscores will get counted as not on the field
unique(data$player[!data$player %in% players])

# fix dates that are off by one in opta data
data[!date %in% boxScores$date, date:=date-1]

# merge
data = merge(data, boxScores, by='date')

# identify when each player is on the field
for(p in players) {
	var = gsub(' ', '_', p)
	data[, (var):=minute>=get(paste0('subbed_in_', p)) & minute<=get(paste0('subbed_out_', p))]
}

# # map to grid to assess number of attempts/game in each pixel
# gridData = list()
# for(p in players) {
	# var = gsub(' ', '_', p)	
	# nGames = data[, list('games'=length(unique(date))), by=var]
	# gridData[[p]] = data.table(expand.grid(seq(0.1,100,by=r), seq(0.1,100,by=r), unique(data[[var]])))
	# setnames(gridData[[p]], c('x','y',var))
	# aggregates = data[, c('x','y',var), with=FALSE]
	# aggregates[, x:=round(x/r)*r]
	# aggregates[, y:=round(y/r)*r]
	# aggregates = aggregates[, list(actions=.N), by=c('x','y',var)]
	# gridData[[p]] = merge(gridData[[p]], aggregates, by=c('x','y',var), all.x=TRUE)
	# gridData[[p]] = merge(gridData[[p]], nGames, by=var)
	# gridData[[p]][, actions_pg:=actions/games]
	# gridData[[p]][is.na(actions), actions:=0]
	# gridData[[p]][, x2:=x^2]
	# gridData[[p]][, y2:=y^2]
	# gridData[[p]][, x3:=x^3]
	# gridData[[p]][, y3:=y^3]
# }
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Analyze

# # simple negative binomial for each player
# print('Running model...')
# nbFit = list()
# for(p in players) {
	# var = gsub(' ', '_', p)
	# formula = as.formula(paste0('actions_pg ~ ', var, '*y*x*y2*x2*x3'))
	# nbFit[[p]] = glm.nb(formula, gridData[[p]])
# }

# # predict for every player
# print('Predicting surfaces...')
# cfData = list()
# for(p in players) {
	# var = gsub(' ', '_', p)
	# cfData[[p]] = data.table(expand.grid(seq(0.1,100, by=r2), seq(0.1,100, by=r2), c(TRUE, FALSE)))
	# setnames(cfData[[p]], c('x','y',var))
	# cfData[[p]][, x2:=x^2]
	# cfData[[p]][, y2:=y^2]
	# cfData[[p]][, x3:=x^3]
	# cfData[[p]][, y3:=y^3]
	# cfData[[p]][, pred_nb:=exp(predict(nbFit[[p]], newdata=cfData[[p]]))]
	# cfData[[p]][, wrap:=ifelse(get(var)==TRUE, paste('With', p), paste('Without', p))]

	# # cap estimates to avoid long tails
	# q5 = quantile(cfData[[p]]$pred_nb, p=.05)
	# q95 = quantile(cfData[[p]]$pred_nb, p=.95)
	# cfData[[p]][pred_nb<q5, pred_nb:=q5]
	# cfData[[p]][pred_nb>q95, pred_nb:=q95]
# }

# 2d kernel density
cfData = list()
for(p in players) {
	var = gsub(' ', '_', p)
	
	# player on field
	kdfit2 = kde2d(x=data[get(var)==TRUE]$x, y=data[get(var)==TRUE]$y, h=30, n=50)
	cfData[[p]] = data.table(melt(kdfit2$z))
	setnames(cfData[[p]], c('x','y','pred_nb'))
	cfData[[p]][, (var):=TRUE]
	
	# player off field
	kdfit2 = kde2d(x=data[get(var)==FALSE]$x, y=data[get(var)==FALSE]$y, h=30, n=50)
	tmp = data.table(melt(kdfit2$z))
	setnames(tmp, c('x','y','pred_nb'))
	tmp[, (var):=FALSE]
	cfData[[p]] = rbind(cfData[[p]], tmp)
	cfData[[p]][, wrap:=ifelse(get(var)==TRUE, paste('With', p), paste('Without', p))]
}
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
colors = rev(brewer.pal(8, 'RdYlBu'))

# location/scale parameters for the field lines
x=-10
y=-9
width=120
height=118
pdfw = 8.75
pdfh = 2.65
# factor to make expected actions integer-scale numbers
f = 15

# make heatmaps
plots = list()
for(p in players) {
	var = gsub(' ', '_', p)

	minutes_on = playerMinutes[player==p]$total_minutes
	minutes_off = playerMinutes[player==p]$minutes_missed
	
	p1 = ggplot(cfData[[p]][wrap==paste('With', p)], aes(y=y,x=x,fill=pred_nb*f)) + 
		geom_tile() + 
		draw_grob(lines, x=x, y=y, width=width, height=height) + # this is bad. I have to match the location/scale to the aspect ratio of the output
		scale_fill_gradientn(paste('Actions\n Per',f,'\nGames'), colors=colors) +
		labs(title=paste('With', p), subtitle=paste(minutes_on, 'minutes'), y='Defensive End') + 
		theme_minimal()	+ 
		theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_blank(), 
		axis.ticks=element_blank(), plot.title=element_text(hjust=.5, size=16), plot.subtitle=element_text(hjust=.5), 
		line = element_blank(), 
		strip.text.x = element_text(size=14))

	p2 = ggplot(cfData[[p]][wrap==paste('Without', p)], aes(y=y,x=x,fill=pred_nb*f)) + 
		geom_tile() + 
		draw_grob(lines, x=x, y=y, width=width, height=height) + # this is bad. I have to match the location/scale to the aspect ratio of the output
		scale_fill_gradientn(paste('Actions\n Per',f,'\nGames'), colors=colors) +
		labs(title=paste('Without', p), subtitle=paste(minutes_off, 'minutes'), y='Defensive End') + 
		theme_minimal()	+ 
		theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_blank(), 
		axis.ticks=element_blank(), plot.title=element_text(hjust=.5, size=16), plot.subtitle=element_text(hjust=.5), 
		line = element_blank(), 
		strip.text.x = element_text(size=14))
	
	plots[[p]] = arrangeGrob(p1, p2, ncol=2)
}

# open pdf
pdf(outFile, height=pdfh, width=pdfw)
for(p in players) { 
	grid.draw(plots[[p]])
	grid.newpage()
}
dev.off()
# ---------------------------------------------------------------------------
