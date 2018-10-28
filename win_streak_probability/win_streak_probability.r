# Winningest teams
# idea: compute each team's lifetime ppg and simulate the probability a 9-game win streak

# -------------------------
# set up R
rm(list=ls())
set.seed(1)
library(data.table)
library(ggplot2)
# -------------------------


# --------------------------------------------------------
# input/output files
inFileHistorical = './webscrape/all_matches_in_mls_website.csv'
inFile2017 = './webscrape/ASA/2017/Game Information.csv'
inFile2018 = './webscrape/ASA/2018/Game Information.csv'
outFile = './win_streak_probability/graphs.pdf'
tableFile = './win_streak_probability/table.csv'
source('./_common/formalize_team_names.r')
# --------------------------------------------------------


# --------------------------------------------------------
# Parameters

# number of sims
S = 1000
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

# keep only current teams
currentTeams = unique(data[year(date)==2018]$team_home)
data = data[team_home %in% currentTeams | team_away %in% currentTeams]
# ----------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------
# Reshape to team-level (is there a better way?)
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

# compute lifetime ppg
ppg = analysisData[, .(win_pct=mean(points==3), N=.N), by='team']
# ----------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------
# Simulate

# sounders only
n = ppg[team=='Seattle Sounders']$N
p = ppg[team=='Seattle Sounders']$win_pct
# p=.5

# simulate from binomial
sims = lapply(1:S, function(x) rbinom(n, 1, p))
sims = data.table(do.call('cbind', sims))

# identify runs of wins
win_streaks = lapply(names(sims), function(x) { 
	streaks = rle(as.character(sims[[x]]))$lengths
	streak_idx = seq(length(streaks))
	streak_idx = rep(streak_idx, streaks)
	streak_idx[sims[[x]]==0] = NA
	return(streaks[unique(na.omit(streak_idx))])
})

# transform into data table
for(i in seq(S)) { 
	tmp = data.table(sim=i, win_streaks=win_streaks[[i]])
	if (i==1) sim_results = tmp
	if (i>1) sim_results = rbind(sim_results, tmp)
}
# ----------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------
# Analysis 

# number of streaks of each length across the thousand sims
table(sim_results$win_streaks)

# probability of each length streak

# best run in each sim
sim_results[, max:=max(win_streaks), by='sim']

# how many of the sims had a 9+ streak in them?
maxes = unique(sim_results[,c('sim','max')])[, .N, by='max']
sum(maxes[max>=9]$N)

# probability of 9, 10 and 11-game streaks occuring by chance alone in 325 games
sum(maxes[max>=9]$N)/S
sum(maxes[max>=10]$N)/S
sum(maxes[max>=11]$N)/S
# ----------------------------------------------------------------------------------
