# 4/12/2017
# Spatial analysis of defensive actions by player. samples sizes probably too small
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


# --------------------------------------------------------------------------
# Files, directories and settings

# analytical resolution (pixel size in the models)
r = 10

# prediction resolution (pixel size in the graphs)
r2 = 1

# year of analysis
year = 2017

# team to analyze
t = 'Seattle'

# input file
inFile1 = paste0('./webscrape/ASA/2016/raw defensive actions.csv')
inFile2 = paste0('./webscrape/ASA/2017/raw defensive actions.csv')

# shapefile of field
outLines = './_common/Soccer_Fields/soccer_field.jpg'

# output file
outFile = paste0('./defensive_actions/defensive_actions_',r,'_',t,'.pdf')
# --------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Load/prep data

# load
data1 = fread(inFile1)
data2 = fread(inFile2)
data = rbind(data1, data2)

# keep designated team
data = data[team==t]

# drop rare players
data[, N:=.N, by='player']
data = data[N>200]

# sort players
# data[, player:=factor(player, levels=unique(data$player)[order(unique(data$player))], order=TRUE)]

# briefly visualize the data
ggplot(data, aes(y=y,x=x,shape=action)) + 
	geom_point() + 
	facet_wrap(~player)
	
# extra term for ease of use
data[, xy:=x*y]	
data[, x2:=x^2]
data[, y2:=y^2]
data[, x3:=x^3]
data[, y3:=y^3]

# map to grid to assess number of attempts/game in each pixel
nGames = data[, list('games'=length(unique(date))), by='player']
gridData = data.table(expand.grid(seq(0,100,by=r), seq(0,100,by=r), unique(data$player)))
setnames(gridData, c('x','y','player'))
aggregates = data[, c('x','y','player'), with=FALSE]
aggregates[, x:=round(x/r)*r]
aggregates[, y:=round(y/r)*r]
aggregates = aggregates[, list(actions=.N), by=c('x','y','player')]
gridData = merge(gridData, aggregates, by=c('x','y','player'), all.x=TRUE)
gridData = merge(gridData, nGames, by='player')
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
print('Running model...')
nbFit = glm.nb(actions_pg ~ player*y*x*y2*x2, gridData)

# simple glm of success probability (takes about 7 minutes with x3, 1 minute with only x2 and y2)
print('Running probability of successful defensive action model...')
# glmFit = glm(outcome ~ player*y*x*y2*x2, data, family='binomial')

# predict for every player
print('Predicting surfaces...')
cfData = data.table(expand.grid(seq(0,100, by=r2), seq(0,100, by=r2), unique(data$player)))
setnames(cfData, c('x','y','player'))
cfData[, x2:=x^2]
cfData[, y2:=y^2]
cfData[, x3:=x^3]
cfData[, y3:=y^3]
# cfData[, pred:=inv.logit(predict(glmFit, newdata=cfData))]
cfData[, pred_nb:=exp(predict(nbFit, newdata=cfData))]

# cap estimates to avoid long tails
# q5 = quantile(cfData$pred, p=.05)
# q95 = quantile(cfData$pred, p=.95)
# cfData[pred<q5, pred:=q5]
# cfData[pred>q95, pred:=q95]
q5 = quantile(cfData$pred_nb, p=.05)
q95 = quantile(cfData$pred_nb, p=.95)
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
colors = rev(brewer.pal(8, 'RdYlBu'))


# location/scale parameters for the field lines
x=-10
y=-9
width=120
height=118
pdfw = 10
pdfh = 6
# factor to make expected actions integer-scale numbers
f = 34

# make heatmaps
p1 = ggplot(cfData, aes(y=y,x=x,fill=pred_nb*f)) + 
	geom_tile() + 
	draw_grob(lines, x=x, y=y, width=width, height=height) + # this is bad. I have to match the location/scale to the aspect ratio of the output
	facet_wrap(~player, scales='free') + 	
	scale_fill_gradientn(paste('Actions\n Per',f,'\nGames'), colors=colors) +
	labs(title='Expected Number of Defensive Actions', y='Defensive End') + 
	theme_minimal()	+ 
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_blank(), 
	axis.ticks=element_blank(), plot.title=element_text(hjust=.5, size=16), line = element_blank())

# make heatmaps
# p2 = ggplot(cfData, aes(y=y,x=x,fill=pred)) + 
	# geom_tile() + 
	# draw_grob(lines, x=x, y=y, width=width, height=height) + # this is bad. I have to match the location/scale to the aspect ratio of the output
	# facet_wrap(~player) + 	
	# scale_fill_gradientn('Probability', colors=colors) +
	# labs(title='Probability of Successful Defensive Action', y='Defensive End') + 
	# theme_minimal()	+ 
	# theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_blank(), 
	# axis.ticks=element_blank(), plot.title=element_text(hjust=.5, size=16), line = element_blank())
	
# make a "profile" graph as a demo for a couple of players showing defensive actions over x
p3 = ggplot(aggregates[, sum(actions),by=c('x','player')], aes(y=V1, x=x)) + 
	geom_bar(stat='identity') + 
	facet_wrap(~player) + 
	labs(title='Defensive Actions by Field Position (Length-Wise)', y='Number of Actions in 2016', x='') +
	scale_x_continuous(breaks=c(0, 25, 50, 75, 100), labels=c('Goal Line', '25%', 'Mid Field', '75%', 'End Line')) + 
	theme_bw() + 
	theme(plot.title=element_text(hjust=.5, size=16), axis.title.y=element_text(size=14), 
	axis.title.x=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=9)) 
	
# make a "profile" graph as a demo for a couple of players showing defensive actions over y
p4 = ggplot(aggregates[, sum(actions),by=c('y','player')], aes(y=V1, x=y)) + 
	geom_bar(stat='identity') + 
	facet_wrap(~player) + 
	labs(title='Defensive Actions by Field Position (Left-Right)', y='Number of Actions in 2016', x='') +
	scale_x_continuous(breaks=c(0, 25, 50, 75, 100), labels=c('Left Sideline', '25%', 'Center', '75%', 'Right Sideline')) + 
	theme_bw() + 
	theme(plot.title=element_text(hjust=.5, size=16), axis.title.y=element_text(size=14), 
	axis.title.x=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=9)) 

# make a surface of the data
p5 = ggplot(gridData, aes(y=y,x=x,fill=actions_pg*f)) + 
	geom_tile() + 
	draw_grob(lines, x=x, y=y, width=width, height=height) + # this is bad. I have to match the location/scale to the aspect ratio of the output
	facet_wrap(~player) + 	
	scale_fill_gradientn(paste('Actions\n Per',f,'\nGames'), colors=colors) +
	labs(title='Expected Number of Defensive Actions', y='Defensive End') + 
	theme_minimal()	+ 
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_blank(), 
	axis.ticks=element_blank(), plot.title=element_text(hjust=.5, size=16), line = element_blank())

	
# open pdf
pdf(outFile, height=pdfh, width=pdfw)
p1
# p2
p3
p4
p5
dev.off()
# ---------------------------------------------------------------------------
