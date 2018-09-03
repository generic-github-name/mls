# heatmaps using kernel density


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
outFile = paste0('./defensive_actions/defensive_actions_',r,'_',te,'_logit.pdf')
# --------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Load/prep opta data

# load
data1 = fread(inFile1)
data2 = fread(inFile2)
data = rbind(data1, data2)

# subset
data = data[team==te]

# format dates/minutes
data[, date:=as.Date(date, '%m/%d/%Y')]
data[, c('minute', 'second'):=tstrsplit(time, ':', fixed=TRUE)]
data[, minute:=as.numeric(minute) + as.numeric(second)/60]

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
gridData[is.na(actions), actions:=0]
gridData[, actions_pg:=actions/games]
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
X = as.matrix(data[,c('x','y'),with=FALSE])

kdfit2 = kde2d(x=data$x, y=data$y, h=15, n=500)
image(kdfit2)
fit = data.table(melt(kdfit2$z))
setnames(fit, c('x','y','z'))
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

x=-10
y=-9
width=120
height=118

ggplot(fit, aes(y=y,x=x,fill=z)) + 
	geom_tile() + 
	draw_grob(lines, x=x, y=y, width=width, height=height) + # this is bad. I have to match the location/scale to the aspect ratio of the output
	# facet_wrap(~team) + 	
	scale_fill_gradientn(paste('Actions/Game'), colors=colors) +
	labs(title='Expected Number of Defensive Actions', y='Defensive End') + 
	theme_minimal()	+ 
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_blank(), 
	axis.ticks=element_blank(), plot.title=element_text(hjust=.5, size=16), line = element_blank())
	
# ---------------------------------------------------------------------------

