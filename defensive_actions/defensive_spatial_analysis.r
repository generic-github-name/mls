# 4/12/2017
# Spatial analysis of defensive actions
# Note: cowplot requires R 3.3 or higher

# example of high press
# NYRB

# example of counter press
# DC United

# example of a little of both
# TFC
# SKC


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


# ------------------------------------------------------
# Files, directories and settings

# input file
inFile = './2016 Stats/raw defensive actions.csv'

# shapefile of field
root = 'C:/Users/davidp6/Google Drive/Personal/Fun Analyses/mls'
outLines = paste0(root, '/_common/Soccer_Fields/soccer_field.jpg')

# analytical resolution (pixel size)
r = 5

# output file
outFile = paste0(root, '/defensive_actions/defensive_actions_',r,'_cubic.pdf')
# ------------------------------------------------------


# ---------------------------------------------------------------------------
# Load/prep data

# load
data = fread(inFile)

# briefly visualize the data
ggplot(data[team=='Seattle'], aes(y=y,x=x,shape=action)) + 
	geom_point() + 
	facet_wrap(~player)
	
# extra term for ease of use
data[, xy:=x*y]	
data[, x2:=x^2]
data[, y2:=y^2]
data[, x3:=x^3]
data[, y3:=y^3]

# map to grid to assess number of attempts/game in each pixel
nGames = data[, list('games'=length(unique(date))), by='team']
gridData = data.table(expand.grid(seq(0,100,by=r), seq(0,100,by=r), unique(data$team)))
setnames(gridData, c('x','y','team'))
aggregates = data[, c('x','y','team'), with=FALSE]
aggregates[, x:=round(x/r)*r]
aggregates[, y:=round(y/r)*r]
aggregates = aggregates[, list(actions=.N), by=c('x','y','team')]
gridData = merge(gridData, aggregates, by=c('x','y','team'), all.x=TRUE)
gridData = merge(gridData, nGames, by='team')
gridData[, actions_pg:=actions/games]
gridData[is.na(actions), actions:=0]
gridData[, x2:=x^2]
gridData[, y2:=y^2]
gridData[, x3:=x^3]
gridData[, y3:=y^3]
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Analyze

# simple negative binomial
nbFit = glm.nb(actions_pg ~ team*y*x*y2*x2*x3, gridData)

# simple glm of success probability (takes about 7 minutes with x3, 1 minute with only x2 and y2)
glmFit = glm(outcome ~ team*y*x*y2*x2*x3, data, family='binomial')

# predict for every team
cfData = data.table(expand.grid(seq(0,100, by=r), seq(0,100, by=r), unique(data$team)))
setnames(cfData, c('x','y','team'))
cfData[, x2:=x^2]
cfData[, y2:=y^2]
cfData[, x3:=x^3]
cfData[, y3:=y^3]
cfData[, pred:=inv.logit(predict(glmFit, newdata=cfData))]
cfData[, pred_nb:=exp(predict(nbFit, newdata=cfData))]

# cap estimates to avoid long tails
q5 = quantile(cfData$pred_nb, p=.1)
q95 = quantile(cfData$pred_nb, p=.9)
cfData[pred_nb<q5, pred_nb:=q5]
cfData[pred_nb>q95, pred_nb:=q95]
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
colors = brewer.pal(8, 'RdYlBu')

# factor to make expected actions integer-scale numbers
f = 34

# make heatmaps
p1 = ggplot(cfData, aes(y=y,x=x,fill=pred_nb*f)) + 
	geom_tile() + 
	draw_grob(lines, x=-10, y=-9, width=120, height=118) + # this is bad. I have to match the location/scale to the aspect ratio of the output
	facet_wrap(~team) + 	
	scale_fill_gradientn(paste('Actions\n Per',f,'\nGames'), colors=colors) +
	labs(title='Expected Number of Defensive Actions', y='Defensive End') + 
	theme_minimal()	+ 
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_blank(), 
	axis.ticks=element_blank(), plot.title=element_text(hjust=.5, size=16), line = element_blank())

# make heatmaps
p2 = ggplot(cfData, aes(y=y,x=x,fill=pred)) + 
	geom_tile() + 
	draw_grob(lines, x=-10, y=-9, width=120, height=118) + # this is bad. I have to match the location/scale to the aspect ratio of the output
	facet_wrap(~team) + 	
	scale_fill_gradientn('Probability', colors=colors) +
	labs(title='Probability of Successful Defensive Action', y='Defensive End') + 
	theme_minimal()	+ 
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_blank(), 
	axis.ticks=element_blank(), plot.title=element_text(hjust=.5, size=16), line = element_blank())
	
# make a "profile" graph as a demo for a couple of teams showing defensive actions over x
p3 = ggplot(aggregates[, sum(actions),by=c('x','team')], aes(y=V1, x=x)) + 
	geom_bar(stat='identity') + 
	facet_wrap(~team) + 
	labs(title='Defensive Actions by Field Position (Length-Wise)', y='Number of Actions in 2016', x='') +
	scale_x_continuous(breaks=c(0, 25, 50, 75, 100), labels=c('Goal Line', '25%', 'Mid Field', '75%', 'End Line')) + 
	theme_bw() + 
	theme(plot.title=element_text(hjust=.5, size=16), axis.title.y=element_text(size=14), 
	axis.title.x=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=9)) 
	
# make a "profile" graph as a demo for a couple of teams showing defensive actions over y
p4 = ggplot(aggregates[, sum(actions),by=c('y','team')], aes(y=V1, x=y)) + 
	geom_bar(stat='identity') + 
	facet_wrap(~team) + 
	labs(title='Defensive Actions by Field Position (Left-Right)', y='Number of Actions in 2016', x='') +
	scale_x_continuous(breaks=c(0, 25, 50, 75, 100), labels=c('Left Sideline', '25%', 'Center', '75%', 'Right Sideline')) + 
	theme_bw() + 
	theme(plot.title=element_text(hjust=.5, size=16), axis.title.y=element_text(size=14), 
	axis.title.x=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=9)) 
	
# open pdf
pdf(outFile, height=6, width=10)
p1
p2
p3
p4
dev.off()
# ---------------------------------------------------------------------------
