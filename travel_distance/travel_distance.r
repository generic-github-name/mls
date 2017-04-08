# Travel distance

# -----------------
# Set up R
rm(list=ls())
library(data.table)
library(maptools)
library(rgeos)
library(geosphere)
library(ggplot2)
library(grid)
library(cowplot)
library(jpeg)
library(png)
library(lme4)
# -----------------


# ----------------------------------------------------
# Files

# data file
inFile = './webscrape/all_matches_in_mls_website.csv'

# city coordinates
coordFile = './_common/city_coordinates.csv'

# USA shapefile
shapeFile = './_common/state_shape_file.rdata'

# output file
outFile = './travel_distance/travel_distance.pdf'
# ----------------------------------------------------


# ------------------------------------------------------------
# Load/prep data

# load shape data
load(shapeFile)

# fortify shape data
shapeData = data.table(fortify(state_map))

# drop everything outside the contiguous 48
ids = data.table(state_map@data[, c('state', 'state_name'), with=FALSE])
ids = unique(ids) # 2 records for alaska?
ids[, state:=as.character(state)]
ids = ids[!state_name %in% c('Alaska', 'Hawaii')]
shapeData = merge(shapeData, ids, by.x='id', by.y='state')
shapeData = shapeData[order(order)]

# load coordinates
coords = fread(coordFile)

# store teams
teams = unique(coords$team)

# load match data
data = fread(inFile)

# drop future games
data = data[!is.na(winner)]

# drop internationals, friendlies and preseason games
comps = 'MLS Regular Season'
data = data[competition %in% comps]

# format date
data[, date:=as.Date(date)]

# home win
data[winner==team_home, home_win:=1]
data[winner!=team_home, home_win:=0]
data[home_win==TRUE, home_points:=3]
data[winner=='draw', home_points:=1]
data[winner!=team_home & winner!='draw', home_points:=0]
# ------------------------------------------------------------


# --------------------------------------------------------------------
# Compute distances

# set up data table to fill in
distances = data.table(t(combn(teams, 2)))
setnames(distances, names(distances), c('team1', 'team2'))

# loop over teams and compute distance to all other teams
for(t1 in teams) {
	for(t2 in teams) {
		p1 = coords[team==t1, c('long', 'lat'), with=FALSE]
		p2 = coords[team==t2, c('long', 'lat'), with=FALSE]
		d = distHaversine(p1, p2, r=3959)
		distances[team1==t1 & team2==t2, distance:=d]
	}
}
# --------------------------------------------------------------------


# --------------------------------------------------------------------
# Run analysis

# merge distances to results
tmp = copy(distances)
setnames(tmp, c('team1', 'team2'), c('team2', 'team1'))
tmp = rbind(distances, tmp)
data = merge(data, tmp, by.x=c('team_home','team_away'), by.y=c('team1','team2'))

# simple categorical analysis
data[distance<500, dist_cat:='1. <500 miles']
data[distance>=500 & distance<1000, dist_cat:='2. 500-1,000 miles']
data[distance>=1000 & distance<1500, dist_cat:='3. 1,000-1,500 miles']
data[distance>=1500, dist_cat:='4. >1,500 miles']
data[, list(mean=mean(home_win), N=.N), by=dist_cat]
data[, list(mean=mean(home_points), N=.N), by=dist_cat]
data[team_away=='seattle', list(mean=mean(home_win), N=.N), by=dist_cat]
data[!team_home %in% c('colorado','sporting-kc'), list(mean=mean(home_win), N=.N), by=dist_cat]

# by conference
west = c('colorado', 'fc-dallas', 'sporting-kc', 'la-galaxy', 'san-jose', 'real-salt-lake', 'chivas-usa', 'houston', 'seattle', 'portland', 'vancouver')
data[team_home %in% west & team_away %in% west, conference:='West']
data[!team_home %in% west & !team_away %in% west, conference:='East']
data[conference=='West', list(mean=mean(home_win), N=.N), by=dist_cat]
data[conference=='East', list(mean=mean(home_win), N=.N), by=dist_cat]


# compute crude travel times
bus_speed = 55
plane_speed = 500
data[distance<200, travel_time:=distance/bus_speed]
data[distance>=200, travel_time:=distance/plane_speed]

# simple categorical analysis
data[travel_time<1, time_cat:='1. <1 hours']
data[travel_time>=1 & travel_time<2, time_cat:='2. 1-2 hours']
data[travel_time>=2 & travel_time<3, time_cat:='3. 2-3 hours']
data[travel_time>=3, time_cat:='4. >3 hours']
data[, list(mean=mean(home_win), N=.N), by=time_cat][order(time_cat)]
data[team_away=='seattle', list(mean=mean(home_win), N=.N), by=time_cat][order(time_cat)]

# who we play in each category
unique(data[team_away=='seattle' & travel_time>=1 & travel_time<2]$team_home)
unique(data[team_away=='seattle' & travel_time>=2 & travel_time<3]$team_home)
unique(data[team_away=='seattle' & travel_time>=3]$team_home)

# odds of home team winning
lmeFit1 = glmer(home_win ~ travel_time + (1 | team_home) + (1 | team_away), data, family='binomial')
summary(lmeFit1)
lmeFit2 = glmer(home_win ~ travel_time + (1 | team_home), data[team_away=='seattle'], family='binomial')
summary(lmeFit2)
lmeFit3 = glmer(home_win ~ travel_time + (1 | team_away), data[team_home=='seattle'], family='binomial')
summary(lmeFit3)

# get odds ratios from any team playing away and travel time
se = sqrt(diag(vcov(lmeFit1)))[2]
coef = fixef(lmeFit1)[2]
exp(c('est'=coef, 'upper'=coef + 1.96*se, 'lower'=coef - 1.96*se))

# get odds ratios from seattle playing away and travel time
se = sqrt(diag(vcov(lmeFit2)))[2]
coef = fixef(lmeFit2)[2]
exp(c('est'=coef, 'upper'=coef + 1.96*se, 'lower'=coef - 1.96*se))
# --------------------------------------------------------------------


# --------------------------------------------------------------------
# Compute travel time over last 4 weeks per team

# --------------------------------------------------------------------


# --------------------------------------------------------------------
# Graphs

# labels
data[home_win==0, facet1:='Home Team Lost']
data[home_win==1, facet1:='Home Team Won']

# titles
ytitle = 'Games Played (Density)'

# graph histogram of home win stratified by travel distance
p1 = ggplot(data, aes(x=travel_time)) + 
	geom_histogram(aes(y = ..density..), fill='#5A9367', color='grey50') + 
	geom_line(aes(y=..density..), stat='density', size=1.2) + 
	facet_wrap(~facet1) + 
	labs(title='Travel Time and Home Wins', y=ytitle, x='Distance Traveled by Away Team (hours)') +
	theme_bw() + 
	theme(plot.title=element_text(hjust=.5))

# relabel
data[home_win==0, facet2:='Seattle Lost']
data[home_win==1, facet2:='Seattle Won']

# graph histogram of home win stratified by travel distance
p2 = ggplot(data[team_home=='seattle'], aes(x=travel_time)) + 
	geom_histogram(aes(y = ..density..), fill='#206C99', color='grey50') + 
	geom_line(aes(y=..density..), stat='density', size=1.2) + 
	facet_wrap(~facet2) + 
	labs(title='Sounders FC Playing at Home', y=ytitle, x='Distance Traveled by Away Team (hours)') +
	theme_bw() + 
	theme(plot.title=element_text(hjust=.5))

# relabel
data[home_win==1, facet3:='Seattle Lost']
data[home_win==0, facet3:='Seattle Won']

# graph histogram of home win stratified by travel distance
p3 = ggplot(data[team_away=='seattle'], aes(x=travel_time)) + 
	geom_histogram(aes(y = ..density..), fill='#5A9367', color='grey50') + 
	geom_line(aes(y=..density..), stat='density', size=1.2) + 
	facet_wrap(~facet3) + 
	labs(title='Sounders FC Playing Away', y=ytitle, x='Distance Traveled by Sounders (hours)') +
	theme_bw() + 
	theme(plot.title=element_text(hjust=.5))
# --------------------------------------------------------------------


# --------------------------------------------------------------------
# Map distances

# store seattle coordinates
scs = coords[team=='seattle']

# subset to seattle distances
sds = distances[team1=='seattle' | team2=='seattle']
sds[team2=='seattle', team2:=team1]
sds$team1 = NULL
setnames(sds, 'team2', 'team')

# merge coordinates and seattle distances
sds = merge(sds, coords, by='team')
sds[, label:=paste(round(distance), 'mi')]

# don't display nycfc and nyrb
sds = sds[team!='nycfc']

# make map
pmap = ggplot(shapeData, aes(y=lat, x=long, group=group)) + 
	geom_polygon(fill='grey85') + 
	geom_path(color='white', size=.05) + 
	geom_segment(data=coords[team!='seattle'], aes(xend=scs$long, yend=scs$lat, group=NULL), color='grey50') + 
	geom_point(data=coords, aes(group=NULL), shape=18, size=2) + 
	annotate(geom='text', y=sds$lat, x=sds$long, label=sds$label, vjust=sds$vjust, hjust=sds$hjust) + 
	theme_minimal(base_size=16) + 
	coord_map('ortho', orientation = c(39, -98, 0)) + # looks better but doesn't work with images
	scale_x_continuous('', breaks = NULL) + scale_y_continuous('', breaks = NULL)
	
# store output for cowplot to add grobs
pmap = ggdraw(pmap) 
	
# add logos
for(t in teams) {

	# load logo and store as grob
	assign(paste0('logo', t), readJPEG(paste0('./_common/logos/', t, '.jpg')))
	assign(paste0('logo', t), rasterGrob(get(paste0('logo', t)), interpolate=TRUE))

	# manually replace white to transparent
	tmp = get(paste0('logo', t))
	colors = data.table(as.matrix(tmp$raster))
	for(var in names(colors)) colors[get(var)=='#FFFFFF', (var):='#00000000']
	tmp$raster = as.raster(as.matrix(colors))
	assign(paste0('logo', t), tmp)	
	
	# store coordinates
	x = coords[team==t]$x
	y = coords[team==t]$y
	s = coords[team==t]$scale

	# add grob to map
	pmap = pmap + draw_grob(get(paste0('logo', t)), x, y, s, s)
}
# --------------------------------------------------------------------


# --------------------------------------------------------------------
# Save graphs
pdf(outFile, height=6, width=10)
print(p1)
print(p2)
print(p3)
print(pmap)
dev.off()
# --------------------------------------------------------------------
