# Assess a team's vulnerabilities based on where its conceded goals originate from and vice-versa
# 8/14/2017


# --------------------
# Set up R
rm(list=ls())
library(reshape2)
library(stringr)
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
# --------------------


# ----------------------------------------------
# Parameters and settings

# narrow down to one team
t = 'Seattle'

# narrow down to one game
# d = '09/24/2017'
# ----------------------------------------------


# ---------------------------------------------------------------
# Files and directories

# input files
inFileDribbles = './webscrape/ASA/2017/dribbles.csv'
inFilePasses = './webscrape/ASA/2017/raw passes.csv'
inFileShots = './webscrape/ASA/2017/raw shots.csv'

# output file
outFile1 = './vulnerabilities_and_strengths/zones_graphic.pdf'
outFile2 = './vulnerabilities_and_strengths/frequency_graphs.pdf'

# shapefile of field
outLines = './_common/Soccer_Fields/soccer_field.jpg'
# ---------------------------------------------------------------


# -------------------------------------------------------------------------------------
# Load/prep data

# load
dribbles = fread(inFileDribbles)
passes = fread(inFilePasses)
shots = fread(inFileShots)

# identify actions
dribbles[, action:='dribble']
passes[, action:='pass']
shots[, action:='shot']

# rename
setnames(shots, 'shooter', 'player')
setnames(passes, c('passer','success'), c('player', 'outcome'))
setnames(dribbles, 'success', 'outcome')

# append
data = rbind(dribbles, passes, shots, fill=TRUE)

# subset to just the selected team
data = data[ateam==t | hteam==t]

# subset to open play goals only
data = data[!patternOfPlay %in% c('Penalty','Corner') | is.na(patternOfPlay)]

# subset variables
vars = c('gameID', 'date', 'time', 'player', 'team', 'team.1', 'x', 'y', 'action', 'outcome', 'result','patternOfPlay','recipient')
data = data[, vars, with=FALSE]

# format time variable
time = str_split(data$time, ':', 3)
time = data.table(do.call('rbind', time))
time[, V1:=as.numeric(gsub(' ', '', V1))]
time[, V2:=as.numeric(gsub(' ', '', V2))]
m = time$V1 + (time$V2/60)
data[, minute:=m]

# make sure no possessions appear to continue after a goal by inserting a "kickoff" action just after every goal
kickoffs = data[result=='Goal']
kickoffs[, action:='kickoff']
kickoffs[, minute:=minute+.00001]
kickoffs[, c('team','team.1') := .(team.1, team)]
kickoffs[, x:=50]
kickoffs[, y:=50]
kickoffs[, result:=NA]
kickoffs[, patternOfPlay:=NA]
data = rbind(data, kickoffs)

# sort
data = data[order(minute)]

# make shot outcomes binary
data[action=='shot', outcome:=ifelse(result=='Goal', 1, 0)]

# identify field "zones" (6x3 grid)
xCuts = seq(0, 100, length.out=7)
yCuts = seq(0, 100, length.out=4)
data[x>=xCuts[1] & x<xCuts[2] & y>=yCuts[1] & y<yCuts[2], zone:=3]
data[x>=xCuts[2] & x<xCuts[3] & y>=yCuts[1] & y<yCuts[2], zone:=6]
data[x>=xCuts[3] & x<xCuts[4] & y>=yCuts[1] & y<yCuts[2], zone:=9]
data[x>=xCuts[4] & x<xCuts[5] & y>=yCuts[1] & y<yCuts[2], zone:=12]
data[x>=xCuts[5] & x<xCuts[6] & y>=yCuts[1] & y<yCuts[2], zone:=15]
data[x>=xCuts[6] & x<=xCuts[7] & y>=yCuts[1] & y<yCuts[2], zone:=18]
data[x>=xCuts[1] & x<xCuts[2] & y>=yCuts[2] & y<yCuts[3], zone:=2]
data[x>=xCuts[2] & x<xCuts[3] & y>=yCuts[2] & y<yCuts[3], zone:=5]
data[x>=xCuts[3] & x<xCuts[4] & y>=yCuts[2] & y<yCuts[3], zone:=8]
data[x>=xCuts[4] & x<xCuts[5] & y>=yCuts[2] & y<yCuts[3], zone:=11]
data[x>=xCuts[5] & x<xCuts[6] & y>=yCuts[2] & y<yCuts[3], zone:=14]
data[x>=xCuts[6] & x<=xCuts[7] & y>=yCuts[2] & y<yCuts[3], zone:=17]
data[x>=xCuts[1] & x<xCuts[2] & y>=yCuts[3] & y<=yCuts[4], zone:=1]
data[x>=xCuts[2] & x<xCuts[3] & y>=yCuts[3] & y<=yCuts[4], zone:=4]
data[x>=xCuts[3] & x<xCuts[4] & y>=yCuts[3] & y<=yCuts[4], zone:=7]
data[x>=xCuts[4] & x<xCuts[5] & y>=yCuts[3] & y<=yCuts[4], zone:=10]
data[x>=xCuts[5] & x<xCuts[6] & y>=yCuts[3] & y<=yCuts[4], zone:=13]
data[x>=xCuts[6] & x<=xCuts[7] & y>=yCuts[3] & y<=yCuts[4], zone:=16]
# -------------------------------------------------------------------------------------


# ----------------------------------------------
# Identify sequences
# An attacking sequence is defined as:
# a) continuous possession
# b) does not contain a "reset", or pass backward (not implemented)

# possessions
for(g in unique(data$gameID)) {
	runs = rle(as.character(data[gameID==g]$team))$lengths
	possessions = seq(length(runs))
	data[gameID==g, possession:=rep(possessions, runs)]
}

# identify sequences that end in goals
data[, goal_sequence:=as.numeric(action=='shot' & outcome==1)]
data[, goal_sequence:=max(goal_sequence), by=c('gameID','possession')]

# collapse to zone-level by identifying the last action insize each zone
# include goal_sequence in the byvars because some possessions appear to continue after the goal
data[, possession_zone:=rev(seq_len(.N)), by=c('gameID','possession','goal_sequence','zone')] 
zoneData = data[possession_zone==1]
zoneData[, zone_id:=rev(seq(.N)), by=c('gameID','possession')]

# label complete sequences
wide = dcast.data.table(zoneData, gameID+possession~zone_id, value.var='zone')
setnames(wide, c('1','2','3'),  c('id1','id2','id3'))
wide[, sequence:=paste(id3,id2,id1)]
wide[, sequence:=gsub(' ','-', sequence)]
wide = wide[,c('gameID','possession','sequence'),with=FALSE]
zoneData = merge(zoneData, wide, by=c('gameID','possession'))

# display example goal
data[minute>51 & minute<51.22 & team=='Salt Lake']
zoneData[team=='Salt Lake' & goal_sequence==1]
# ----------------------------------------------


# ----------------------------------------------
# Analysis

# the last, second to last and third to last zones for each goal sequence scored against us
table(zoneData[goal_sequence==1 & zone_id<=3 & team!=t,c('zone_id','zone'),with=F])
zoneAgg = zoneData[goal_sequence==1 & zone_id<=3 & team!=t, .N, by=c('zone_id','zone')]
zoneAgg[, zone_id:=as.character(zone_id)]
zoneAgg[zone_id=='1', zone_id:='Last']
zoneAgg[zone_id=='2', zone_id:='Second-to-Last']
zoneAgg[zone_id=='3', zone_id:='Third-to-Last']

# tabulate complete sequences
table(zoneData[result=='Goal' & team!=t]$sequence)
sum(table(zoneData[result=='Goal' & team!=t]$sequence))

# the last, second to last and third to last zones for each goal sequence scored by us
table(zoneData[goal_sequence==1 & zone_id<=3 & team==t,c('zone_id','zone'),with=F])
zoneAggFor = zoneData[goal_sequence==1 & zone_id<=3 & team==t, .N, by=c('zone_id','zone')]
zoneAggFor[, zone_id:=as.character(zone_id)]
zoneAggFor[zone_id=='1', zone_id:='Last']
zoneAggFor[zone_id=='2', zone_id:='Second-to-Last']
zoneAggFor[zone_id=='3', zone_id:='Third-to-Last']

# tabulate complete sequences
table(zoneData[result=='Goal' & team==t]$sequence)
sum(table(zoneData[result=='Goal' & team==t]$sequence))
# ----------------------------------------------


# ----------------------------------------------
# Set up graph data

# manually make grid
graphData = data.table(zone=seq(18))
graphData[zone==1,  c('y','x'):=list(mean(c(100, 66.7)), mean(c(0, 16.7)))]
graphData[zone==2,  c('y','x'):=list(mean(c(66.7, 33.3)), mean(c(0, 16.7)))]
graphData[zone==3,  c('y','x'):=list(mean(c(33.3, 0)), mean(c(0, 16.7)))]
graphData[zone==4,  c('y','x'):=list(mean(c(100, 66.7)), mean(c(16.7, 33.3)))]
graphData[zone==5,  c('y','x'):=list(mean(c(66.7, 33.3)), mean(c(16.7, 33.3)))]
graphData[zone==6,  c('y','x'):=list(mean(c(33.3, 0)), mean(c(16.7, 33.3)))]
graphData[zone==7,  c('y','x'):=list(mean(c(100, 66.7)), mean(c(33.3, 50)))]
graphData[zone==8,  c('y','x'):=list(mean(c(66.7, 33.3)), mean(c(33.3, 50)))]
graphData[zone==9,  c('y','x'):=list(mean(c(33.3, 0)), mean(c(33.3, 50)))]
graphData[zone==10, c('y','x'):=list(mean(c(100, 66.7)), mean(c(50, 66.7)))]
graphData[zone==11, c('y','x'):=list(mean(c(66.7, 33.3)), mean(c(50, 66.7)))]
graphData[zone==12, c('y','x'):=list(mean(c(33.3, 0)), mean(c(50, 66.7)))]
graphData[zone==13, c('y','x'):=list(mean(c(100, 66.7)), mean(c(66.7, 83.3)))]
graphData[zone==14, c('y','x'):=list(mean(c(66.7, 33.3)), mean(c(66.7, 83.3)))]
graphData[zone==15, c('y','x'):=list(mean(c(33.3, 0)), mean(c(66.7, 83.3)))]
graphData[zone==16, c('y','x'):=list(mean(c(100, 66.7)), mean(c(83.3, 100)))]
graphData[zone==17, c('y','x'):=list(mean(c(66.7, 33.3)), mean(c(83.3, 100)))]
graphData[zone==18, c('y','x'):=list(mean(c(33.3, 0)), mean(c(83.3, 100)))]

# repeat
graphData = rbind(graphData, graphData, graphData)
graphData[, zone_id:=rep(c('Last','Second-to-Last','Third-to-Last'),each=18)]

# add frequencies
graphDataFor = merge(graphData, zoneAggFor, by=c('zone','zone_id'), all.x=TRUE)
graphDataFor[is.na(N), N:=0]

# add frequencies
graphData = merge(graphData, zoneAgg, by=c('zone','zone_id'), all.x=TRUE)
graphData[is.na(N), N:=0]
# ----------------------------------------------


# ---------------------------------------------------------------------------
# Load field outline

# load logo and store as grob
lines = readJPEG(outLines)
lines = rasterGrob(lines, interpolate=TRUE)

# manually replace white to transparent
colors = data.table(as.matrix(lines$raster))
for(var in names(colors)) colors[get(var)=='#FFFFFF', (var):='#00000000']
lines$raster = as.raster(as.matrix(colors))
# ---------------------------------------------------------------------------


# ----------------------------------------------
# Graph

# colors
cols1 = brewer.pal(11, 'BrBG')
cols2 = rev(brewer.pal(11, 'RdYlBu'))

# graph sizes
x=-10
y=-9
width=120
height=118

# store graph of just the zones
p0 = ggplot(graphData[zone_id=='Last'], aes(y=y,x=x,fill=zone)) + 
		geom_tile() + 
		geom_text(aes(label=zone)) + 
		draw_grob(lines, x=x, y=y, width=width, height=height) + # this is bad. I have to match the location/scale to the aspect ratio of the output
		scale_fill_gradientn(paste('Goals'), colors=cols1) +
		labs(title='Soccer Zones', y='Defensive End') + 
		theme_minimal()	+ 
		theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_blank(), 
		axis.ticks=element_blank(), plot.title=element_text(hjust=.5, size=16), plot.subtitle=element_text(hjust=.5), 
		line = element_blank(), legend.position='none', 
		strip.text.x = element_text(size=14))

# store graph
p1 = ggplot(graphData[zone_id=='Last'], aes(y=y,x=x,fill=N)) + 
		geom_tile() + 
		geom_text(aes(label=paste0(zone, '\n (', N, ' goals)'))) + 
		draw_grob(lines, x=x, y=y, width=width, height=height) + # this is bad. I have to match the location/scale to the aspect ratio of the output
		scale_fill_gradientn(paste(''), colors=cols2) +
		labs(title='Zone of Each Conceded Goal', subtitle='Number of Goals Originating from Each Zone', y='Opposition\'s Defensive End') + 
		theme_minimal()	+ 
		theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_blank(), 
		axis.ticks=element_blank(), plot.title=element_text(hjust=.5, size=16), plot.subtitle=element_text(hjust=.5), 
		line = element_blank(), 
		strip.text.x = element_text(size=14))

# store graph
p2 = ggplot(graphData[zone_id=='Second-to-Last'], aes(y=y,x=x,fill=N)) + 
		geom_tile() + 
		geom_text(aes(label=paste0(zone, '\n (', N, ' goals)'))) + 
		draw_grob(lines, x=x, y=y, width=width, height=height) + # this is bad. I have to match the location/scale to the aspect ratio of the output
		scale_fill_gradientn(paste('Goals'), colors=cols2) +
		labs(title='Zone Before Each Conceded Goal', subtitle='Number of Goals Originating from Each Zone', y='Opposition\'s Defensive End') + 
		theme_minimal()	+ 
		theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_blank(), 
		axis.ticks=element_blank(), plot.title=element_text(hjust=.5, size=16), plot.subtitle=element_text(hjust=.5), 
		line = element_blank(), 
		strip.text.x = element_text(size=14))

# store graph
p3 = ggplot(graphData[zone_id=='Third-to-Last'], aes(y=y,x=x,fill=N)) + 
		geom_tile() + 
		geom_text(aes(label=paste0(zone, '\n (', N, ' goals)'))) + 
		draw_grob(lines, x=x, y=y, width=width, height=height) + # this is bad. I have to match the location/scale to the aspect ratio of the output
		scale_fill_gradientn(paste('Goals'), colors=cols2) +
		labs(title='Second Zone Before Each Conceded Goal', subtitle='Number of Goals Originating from Each Zone', y='Opposition\'s Defensive End') + 
		theme_minimal()	+ 
		theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_blank(), 
		axis.ticks=element_blank(), plot.title=element_text(hjust=.5, size=16), plot.subtitle=element_text(hjust=.5), 
		line = element_blank(), 
		strip.text.x = element_text(size=14))

# store graph
p4 = ggplot(graphDataFor[zone_id=='Last'], aes(y=y,x=x,fill=N)) + 
		geom_tile() + 
		geom_text(aes(label=paste0(zone, '\n (', N, ' goals)'))) + 
		draw_grob(lines, x=x, y=y, width=width, height=height) + # this is bad. I have to match the location/scale to the aspect ratio of the output
		scale_fill_gradientn(paste(''), colors=cols2) +
		labs(title='Zone of Each Scored Goal', subtitle='Number of Goals Originating from Each Zone', y='Sounders Defensive End') + 
		theme_minimal()	+ 
		theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_blank(), 
		axis.ticks=element_blank(), plot.title=element_text(hjust=.5, size=16), plot.subtitle=element_text(hjust=.5), 
		line = element_blank(), 
		strip.text.x = element_text(size=14))

# store graph
p5 = ggplot(graphDataFor[zone_id=='Second-to-Last'], aes(y=y,x=x,fill=N)) + 
		geom_tile() + 
		geom_text(aes(label=paste0(zone, '\n (', N, ' goals)'))) + 
		draw_grob(lines, x=x, y=y, width=width, height=height) + # this is bad. I have to match the location/scale to the aspect ratio of the output
		scale_fill_gradientn(paste('Goals'), colors=cols2) +
		labs(title='Zone Before Each Scored Goal', subtitle='Number of Goals Originating from Each Zone', y='Sounders Defensive End') + 
		theme_minimal()	+ 
		theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_blank(), 
		axis.ticks=element_blank(), plot.title=element_text(hjust=.5, size=16), plot.subtitle=element_text(hjust=.5), 
		line = element_blank(), 
		strip.text.x = element_text(size=14))

# store graph
p6 = ggplot(graphDataFor[zone_id=='Third-to-Last'], aes(y=y,x=x,fill=N)) + 
		geom_tile() + 
		geom_text(aes(label=paste0(zone, '\n (', N, ' goals)'))) + 
		draw_grob(lines, x=x, y=y, width=width, height=height) + # this is bad. I have to match the location/scale to the aspect ratio of the output
		scale_fill_gradientn(paste('Goals'), colors=cols2) +
		labs(title='Second Zone Before Each Scored Goal', subtitle='Number of Goals Originating from Each Zone', y='Sounders Defensive End') + 
		theme_minimal()	+ 
		theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_blank(), 
		axis.ticks=element_blank(), plot.title=element_text(hjust=.5, size=16), plot.subtitle=element_text(hjust=.5), 
		line = element_blank(), 
		strip.text.x = element_text(size=14))
# ----------------------------------------------


# --------------------------------
# Save graphs
pdf(outFile1, height=5, width=7.4)
p0
dev.off()
pdf(outFile2, height=5, width=7.9)
p1
p2
p3
p4
p5
p6
dev.off()
# --------------------------------
