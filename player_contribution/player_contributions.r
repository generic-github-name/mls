# Explore goal difference with each player on the field


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(stringr)
# --------------------


# -------------------------------------------------------------------------
# Files/directories/lists

# input file
inFile = './webscrape/boxscores_for_every_match.csv'

# match-level file
matchFile = './webscrape/all_matches_in_mls_website.csv'

# output file
outFile1 = './player_contribution/player_contributions_current.pdf'
outFile2 = './player_contribution/player_contributions_all.pdf'

# formalization function
source('./_common/formalize_team_names.r')
# -------------------------------------------------------------------------


# --------------------------------------------------------------------------------
# Load/prep data

# laod 
data = fread(inFile)

# subset
vars = c('player','team','date','team_away','team_home','minutes','goal_1','goal_2','goal_3','goal_4','subbed_in','subbed_out')
data = data[, vars, with=FALSE]
data = data[team_home=='seattle' | team_away=='seattle']

# fix duplicate names
data[player=='Nélson Valdez', player:='Nelson Valdez']

# convert to numeric
for(v in c(paste0('goal_',seq(4)), 'subbed_in','subbed_out')) {
	data[get(v)=='', (v):=NA]
	data[get(v)=='HT', (v):='45']
	data[get(v)=='EHT', (v):='45']
	data[get(v)=='EOR', (v):='90']
	data[, (v):=gsub('\'', '', get(v))]
	newVar = sapply(data[[v]], function(x) eval(parse(text=x)))
	data[, (v):=newVar]
}

# fix missing substitution data with assumptions
data[minutes<45 & is.na(subbed_in) & is.na(subbed_out), subbed_in:=93-minutes]
data[minutes>=45 & minutes<90 & is.na(subbed_in) & is.na(subbed_out), subbed_out:=minutes]
data[minutes==0, subbed_in:=NA]

# goal times for and away by game
for(d in unique(data$date)) {
	gfor = data[team=='seattle' & date==d]
	goals = c(gfor$goal_1, gfor$goal_2, gfor$goal_3, gfor$goal_4)
	goals = goals[!is.na(goals)]
	against = data[(team_home=='seattle' | team_away=='seattle') & team!='seattle' & date==d]
	goals_a = c(against$goal_1, against$goal_2, against$goal_3, against$goal_4)
	goals_a = goals_a[!is.na(goals_a)]
	
	# compute goals scored while player was on the field per game
	for(p in unique(data[team=='seattle' & date==d]$player)) {
		sin = data[player==p & date==d]$subbed_in
		sout = data[player==p & date==d]$subbed_out
		if (is.na(sin)) sin = 0
		if (is.na(sout)) sout = Inf
		if (data[player==p & date==d]$minutes==0) sout = 0
		goals_on = length(goals[goals>=sin & goals<sout])
		goals_off = length(goals[goals<sin | goals>sout])
		
		# get goals against
		goals_a_on = length(goals_a[goals_a>=sin & goals_a<sout])
		goals_a_off = length(goals_a[goals_a<sin | goals_a>sout])
		data[player==p & date==d, goals_for:=goals_on]
		data[player==p & date==d, goals_against:=goals_a_on]
		data[player==p & date==d, goals_for_off:=goals_off]
		data[player==p & date==d, goals_against_off:=goals_a_off]
	}
}
data[minutes==0, goals_for:=NA]
data[minutes==0, goals_against:=NA]

# only mls competitions
matchData = fread(matchFile)
comps = c('MLS Cup', 'MLS Playoffs', 'MLS Regular Season', 'U.S. Open Cup')
data = merge(data, matchData, by=c('team_home','team_away','date'), all.x=TRUE)
data = data[competition %in% comps]
setkey(data, NULL)

# now keep only seattle (now that we've recorded goals against)
data = data[team=='seattle']

# extra variables
data[, gd:=goals_for-goals_against]
data[, gd_off:=goals_for_off-goals_against_off]
data[, total_minutes:=sum(minutes), by=c('team','player')]
data[, mean_gd:=mean(gd, na.rm=TRUE), by=c('team','player')]

# keep only players and goals
idVars = c('team','date')
data = data[, c(idVars,'goals_against','goals_for','goals_against_off','goals_for_off','gd','gd_off','player','minutes','total_minutes'), with=FALSE]

# identify current players
currentPlayers = unique(data[year(date)==2017 & total_minutes>300]$player)
# --------------------------------------------------------------------------------


# --------------------------------------------------------------------------------
# Analyze

# simple averages among current players 
means = data[total_minutes>300 & player %in% currentPlayers, list(
							goals_for=sum(goals_for, na.rm=TRUE), 
							goals_against=sum(goals_against, na.rm=TRUE), 
							gd=mean(gd, na.rm=TRUE), 
							sd=sd(gd, na.rm=TRUE), 
							minutes=sum(minutes, na.rm=TRUE),
							games=.N), by='player'][order(-gd)]
means 

# simple averages across all data
allMeans = data[total_minutes>300, list(
							goals_for=sum(goals_for, na.rm=TRUE), 
							goals_against=sum(goals_against, na.rm=TRUE), 
							gd=mean(gd, na.rm=TRUE), 
							sd=sd(gd, na.rm=TRUE), 
							minutes=sum(minutes, na.rm=TRUE),
							games=.N), by='player'][order(-gd)]
							
# simple averages among current players when they're off the field
meansOff = data[total_minutes>300 & player %in% currentPlayers, list(
							goals_for_off=sum(goals_for_off, na.rm=TRUE), 
							goals_against_off=sum(goals_against_off, na.rm=TRUE), 
							gd_off=mean(gd_off, na.rm=TRUE), 
							sd=sd(gd, na.rm=TRUE), 
							minutes=sum(minutes, na.rm=TRUE),
							games=.N), by='player'][order(-gd_off)]
meansOff 

# tabulate results by player
data[, result:=paste0(goals_for, '-', goals_against)]
table(data[player %in% currentPlayers & result!='NA-NA', c('player','result'), with=FALSE])

# regression for current players (to show that it's not significant)
lmFit = lm(gd ~ player, data[total_minutes>300 & player %in% currentPlayers])
est = data.table(cbind(coef(lmFit),confint(lmFit)))
est[, player:=gsub('player','',names(coef(lmFit)))]
est[order(-V1)]
# --------------------------------------------------------------------------------


# --------------------------------------------------------------------------------
# Make a nice graph
p1 = ggplot(data=means, aes(y=gd, x=reorder(player, -means$gd), ymax=gd+(.1*sd), ymin=gd-(.1*sd))) + 
	geom_bar(stat='identity', fill='#08519c') + 
	geom_text(data=means[gd>=0], aes(label=minutes, x=player), vjust=-.3) + # labels above
	geom_text(data=means[gd<0], aes(label=minutes, x=player), vjust=1) + # labels below
	labs(title='Player Contributions', y='Mean Goal Differential\nwith Player on Field', x='', caption='Bar Labels: Total Minutes Played') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16))
	
p2 = ggplot(data=allMeans, aes(y=gd, x=reorder(player, -allMeans$gd))) + 
	geom_bar(stat='identity', fill='#08519c') + 
	geom_text(data=allMeans[gd>=0], aes(label=minutes, x=player), vjust=-.3, size=2) + # labels above
	geom_text(data=allMeans[gd<0], aes(label=minutes, x=player), vjust=.95, size=2) + # labels below
	labs(title='Player Contributions', y='Mean Goal Differential\nwith Player on Field', x='', caption='Bar Labels: Total Minutes Played') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=10), plot.title=element_text(hjust=.5, size=16))
	
p3 = ggplot(data=meansOff, aes(y=gd_off, x=reorder(player, meansOff$gd_off))) + 
	geom_bar(stat='identity', fill='#CB181D') + 
	geom_text(data=meansOff[gd_off>=0], aes(label=minutes, x=player), vjust=-.3) + # labels above
	geom_text(data=meansOff[gd_off<0], aes(label=minutes, x=player), vjust=1) + # labels below
	labs(title='Player Contributions', y='Mean Goal Differential\nwith Player off Field', x='', caption='Bar Labels: Total Minutes Played') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16))
	
	
# ggplot(data[player %in% currentPlayers], aes(y=gd, x=reorder(player, -mean_gd))) + 
	# geom_bar(data=means, aes(y=gd,x=reorder(player, -means$gd)), stat='identity', fill='#08519c') + 
	# geom_jitter(height=.1, width=0, shape='_', size=8, alpha=.4) + 
	# labs(title='Player Contributions', y='Goal Differential with Player on Field', x='', caption='Individual GDs Jittered for Display') + 
	# theme_bw() + 
	# theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16), plot.caption=element_text(size=8))

# --------------------------------------------------------------------------------


# --------------------------------------------------------------------------------
# Save separate files to have different aspect ratios
pdf(outFile1, height=5.5, width=6.5)
p1
p3
dev.off()
pdf(outFile2, height=6, width=10)
p2
dev.off()
# --------------------------------------------------------------------------------
