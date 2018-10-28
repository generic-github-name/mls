# Winningest teams
# idea: visualize each team's all-time mean points and winning percentage. show how those metrics have evolved (cumulatively) over time

# -------------------------
# set up R
rm(list=ls())
library(data.table)
library(stats)
library(RColorBrewer)
library(ggplot2)
# -------------------------


# --------------------------------------------------------
# input/output files
inFileHistorical = './webscrape/all_matches_in_mls_website.csv'
inFile2017 = './webscrape/ASA/2017/Game Information.csv'
inFile2018 = './webscrape/ASA/2018/Game Information.csv'
outFile = './parity/graphs.pdf'
source('./_common/formalize_team_names.r')
# --------------------------------------------------------


# ----------------------------------------------------------------------------------
# load data
oldData = fread(inFileHistorical)
newData = fread(inFile2017)
newData = rbind(newData, fread(inFile2018))

# drop before 2017 from old data
oldData = oldData[year(date)<2017]

# format dates
oldData[, date:=as.Date(date)]
newData[, date:=as.Date(date, '%m/%d/%Y')]

# rename/subset new data to match old data
newNames = c('team_home','team_away','score_home','score_away')
setnames(newData, c('hteam','ateam','hfinal','afinal'), newNames)
newData = newData[, c('date', newNames), with=FALSE]
newData[, winner:=ifelse(score_home>score_away, team_home, team_away)]
newData[score_home==score_away, winner:='draw']
newData[, date:=as.Date(date, '%m/%d/%Y')]
newData[, competition:='MLS Regular Season']

# rbind
data = rbind(newData, oldData)

# keep only post 2009
# data = data[year(date)>=2009]

# drop internationals, friendlies and preseason games
comps = 'MLS Regular Season'
data = data[competition %in% comps]

# compute points earned by the home team
data[, home_points:=0]
data[winner==team_home, home_points:=3]
data[winner=='draw', home_points:=1]
data[, away_points:=0]
data[winner==team_away, away_points:=3]
data[winner=='draw', away_points:=1]

# formalize team names
data = formalizeTeamNames(data)
# ----------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------
# Reshape data (is there a better way?)
i=1
teams = unique(data[competition=='MLS Regular Season']$team_home)
vars = c('winner', 'team_home', 'team_away', 'home_points', 'away_points', 'date')
for(t in teams) {
	tmp = data[team_away==t | team_home==t, vars, with=FALSE]
	tmp[team_home==t, points:=home_points]
	tmp[team_away==t, points:=away_points]
	tmp[, team:=t]
	tmp = tmp[, c('team','points','winner','date'),with=FALSE]
	if (i==1) analysisData = tmp
	if (i>1) analysisData = rbind(analysisData, tmp)
	i=i+1
}
# ----------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------
# Analyze

# identify conference
west = c('Colorado', 'FC Dallas', 'Sporting KC', 'LA Galaxy', 'San Jose', 'Real Salt Lake', 'Chivas USA', 'Houston', 'Seattle Sounders', 'Portland', 'Vancouver', 'Minnesota')
analysisData[team %in% west, conference:='West']
analysisData[!team %in% west, conference:='East']

# count points per team-season
analysisData[, year:=year(date)]
aggData = analysisData[, .(points=sum(points), gp=.N), by=c('team','year')]
aggData = aggData[order(team, year)]


# ---------------------------
# Estimate "franchise parity"
# ---------------------------

# compute the lag of points
aggData[, previous_points:=shift(points), by=team]


# ------------------------
# Estimate "season parity"
# ------------------------

# exclude pre-2000 because they did shootouts and my data doesn't have the info
aggData2000 = aggData[year>=2000]

# compute measures of dispersion in points by season - 
spreads = aggData2000[, .(q25=quantile(points, .25), q75=quantile(points, .75), 
					q05=quantile(points, .05), q95=quantile(points, .95),
 					sd=sd(points), mad=mad(points), min=min(points), 
					max=max(points), median=median(points)), by=year][order(year)]
spreads[, iqr:=q75-q25]
spreads[, inner90:=q95-q05]
spreads[, range:=max-min]

# melt
spreads = melt(spreads, id.vars='year')

# measures of dispersion that matter
dis = c('sd','mad','iqr','inner90','range')
spreads = spreads[variable %in% dis]
# ----------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------
# Display numbers

# regress to compute correlation between points and previous points
lmFit_franchise = lm(points~previous_points, aggData2000)

# estimate with interaction to see if the correlation is increasing over time
lmFit_franchiseInt = lm(points~previous_points*year, aggData2000)

# estimate with fixed effect to rank years
lmFit_franchiseFeff = lm(points~previous_points*factor(year), aggData2000)

# display franchise parity
summary(lmFit_franchise)
summary(lmFit_franchiseInt)
summary(lmFit_franchiseFeff)

# get fitted values for each year from the interaction model
frame = data.table(expand.grid(year=seq(min(aggData2000$year), max(aggData2000$year)), 
	previous_points=c(min(aggData2000$previous_points, na.rm=T), max(aggData2000$previous_points, na.rm=T))))
frame[, fit:=predict(lmFit_franchiseInt, newdata=frame)]
	
# slope on season parity
lmFit_range = lm(value~year, spreads[variable=='range'])
lmFit_inner90 = lm(value~year, spreads[variable=='inner90'])
lmFit_iqr = lm(value~year, spreads[variable=='iqr'])
lmFit_sd = lm(value~year, spreads[variable=='sd'])
lmFit_mad = lm(value~year, spreads[variable=='mad'])

# display season parity
summary(lmFit_range)
summary(lmFit_inner90)
summary(lmFit_iqr)
summary(lmFit_sd)
summary(lmFit_mad)

# is 2018 the worst lowest parity?
spreads[, rank:=rank(-value), by='variable']
spreads[rank<3]
coefs = data.table(coef=names(coef(lmFit_franchiseFeff)), value=coef(lmFit_franchiseFeff))
coefs[grepl(':',coef)][order(-value)]
# ----------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------
# Set up to graph

# colors
colors = brewer.pal(6, 'Accent')[-4]

# label measures of dispersion
spreads[variable=='range', label:='Range (max-min)']
spreads[variable=='inner90', label:='Inner 90th Percentile']
spreads[variable=='iqr', label:='Inter-Quartile Range']
spreads[variable=='sd', label:='Standard Deviation']
spreads[variable=='mad', label:='Median Absolute Deviation']
# ----------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------
# Time series plots

# franchise parity
p1 = ggplot(aggData2000, aes(y=points, x=previous_points)) + 
	geom_point() + 
	geom_smooth(method='lm') + 
	labs(title='Franchise Parity', 
		subtitle='Measured by Correlation between Points and Points in the Previous Season (for the same team)', 
		y='Points', x='Points in Previous Season') + 
	theme_bw()

# franchise parity over time
p2 = ggplot(aggData2000[year!=min(year)], aes(y=points, x=previous_points)) + 
	geom_point() + 
	facet_wrap(~year, scales='free') + 
	geom_smooth(method='lm') + 
	labs(title='Franchise Parity Over Time', 
		subtitle='Measured by Correlation between Points and Points in the Previous Season (for the same team)', 
		y='Points', x='Points in Previous Season') + 
	theme_bw()

# franchise parity over time using the interaction term
aggData2000[, pred:=predict(lmFit_franchiseInt, newdata=aggData2000)]
p3 = ggplot(aggData2000[year!=min(year)], aes(y=points, x=previous_points, color=year, group=year)) + 
	geom_point() + 
	geom_line(data=frame, aes(y=fit)) + 
	labs(title='Franchise Parity Over Time', 
		subtitle='Measured by Correlation between Points and Points in the Previous Season (for the same team)', 
		y='Points', x='Points in Previous Season', color='Year', 
		caption='Each line indicates the correlation with the previous year.\n
		A steaper line indicates less parity from one year to the next.') + 
	theme_bw()

# season parity
p4 = ggplot(spreads, aes(y=value, x=year, color=reorder(label,-value))) + 
	geom_line(size=2) + 
	scale_color_manual(values=colors) + 
	scale_x_continuous(breaks=seq(min(spreads$year), max(spreads$year), by=2)) + 
	labs(title='Season Parity', subtitle='Measured by Point Spread at End of Season', 
		y='Spread', x='', color='Measure of Spread') + 
	theme_bw(base_size=16)

# season parity with smooth fit
p5 = ggplot(spreads, aes(y=value, x=year)) + 
	geom_point() + 
	geom_smooth(method='lm') + 
	facet_wrap(~label, scales='free') + 
	scale_x_continuous(breaks=seq(min(spreads$year), max(spreads$year), by=4)) + 
	labs(title='Season Parity', subtitle='Measured by Point Spread at End of Season', 
		y='Spread', x='', color='Measure of Spread') + 
	theme_bw(base_size=16)

pdf(outFile, height=6, width=10)
p1
p2
p3
p4
p5
dev.off()
# ----------------------------------------------------------------------------------
