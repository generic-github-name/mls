# Analyze ref bias
# outcomes: cards given and pks against, analyses: does salazar (repeat for others) give more cards to some teams than others? and does he give more cards than other refs?


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
outFile = './refs/graphs.pdf'

# formalization function
source('./_common/formalize_team_names.r')
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Load/prep data

# laod 
data = fread(inFile)

# only mls competitions
matchData = fread(matchFile)
comps = c('MLS Cup', 'MLS Playoffs', 'MLS Regular Season')
matchData = matchData[, competitions %in% comps]
data = merge(data, matchData, by=c('team_home','team_away','date'), all.x=TRUE)
data = data[competition %in% comps]
setkey(data, NULL)

# isolate ref tenure
tenures = data[, c('date','ref1'), with=FALSE]
tenures = tenures[, list(start=min(date), end=max(date)), by='ref1']
tenures[, tenure:=paste(start, '-', end)]
tenures = tenures[, c('ref1','tenure'), with=FALSE]

# parse pks
data[str_count(pks, ',')==0, pks:=paste0(pks,', ')]
data[str_count(pks, ',')==1, pks:=paste0(pks,', ')]
data[is.na(pks), pks:='NA,NA,NA']
pksVars = data.table(do.call('rbind', str_split(data$pks, ', ')))
for(v in names(pksVars)) {
	pksVars[, (v):=as.numeric(get(v))]
	pksVars[is.na(get(v)), (v):=0]
}
pksVars[, total_pks:=V1+V2+V3]
pksVars = pksVars[, 'total_pks', with=FALSE]
data = cbind(data, pksVars)

# turn it into pks against by matching up who played who
total_pks = copy(data[, c('team_home','team_away','team','total_pks','ref1','date'), with=FALSE])
total_pks[, team:=ifelse(team==team_home, team_away, team_home)]
total_pks = total_pks[, list(total_pks=sum(total_pks)), by=c('team_home','team_away','team','ref1','date')]

# make card variables
data[, yellow:=card=='yellow']
data[, red:=card %in% c('red','secondyellow')]

# count cards and pks by ref-team
byVars = c('ref1', 'team')
frequencies = data[, list('yellows'=sum(yellow, na.rm=TRUE), 'reds'=sum(red, na.rm=TRUE), 'fouls'=sum(fouls, na.rm=TRUE), 'fouled'=sum(fouled, na.rm=TRUE)), by=byVars]
total_pks = total_pks[,list('pks'=sum(total_pks, na.rm=TRUE), 'games'=.N), by=byVars]

# merge cards and pks
frequencies = merge(frequencies, total_pks, by=byVars)

# add ref tenures
frequencies = merge(frequencies, tenures, by='ref1')

# make "harmful calls"
frequencies[, harmful_calls:=reds+pks]

# collapse frequencies to team and ref levels
sumVars = c('yellows','reds','pks','harmful_calls','fouls','fouled','games')
teamFreqs = frequencies[, lapply(.SD, sum), .SDcols=sumVars, by='team']
refFreqs = frequencies[, lapply(.SD, sum), .SDcols=sumVars, by=c('ref1','tenure')]

# formalize for tables
teamFreqs = formalizeTeamNames(teamFreqs)
refFreqs = formalizeTeamNames(refFreqs)
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Basic analysis

# cards/pks per 10 games by team
r = 10
teamFreqs[, ypg:=round(yellows/games*r,1)]
teamFreqs[, rpg:=round(reds/games*r,1)]
teamFreqs[, pkpg:=round(pks/games*r,1)]
teamFreqs[, hpg:=round(harmful_calls/games*r,1)]
teamFreqs[order(-hpg)] # don't forget, it's games since 2013 only

# cards/pks per 10 games by ref among sounders only
frequencies[, ypg:=round(yellows/games*r,1)]
frequencies[, rpg:=round(reds/games*r,1)]
frequencies[, pkpg:=round(pks/games*r,1)]
frequencies[, hpg:=round(harmful_calls/games*r,1)]
frequencies[team=='seattle' & games>3][order(-hpg)]

# cards/pks per 10 games by ref
refFreqs[, ypg:=round(yellows/games*r,1)]
refFreqs[, rpg:=round(reds/games*r,1)]
refFreqs[, pkpg:=round(pks/games*r,1)]
refFreqs[, hpg:=round(harmful_calls/games*r,1)]
refFreqs[games>35][order(-hpg)]

# every team's worst enemy
ranks = frequencies[order(team, -hpg)]
ranks = ranks[games>3]
ranks[, ref_rank:=seq_len(.N), by='team']
ranks[ref_rank==1]
table(ranks[ref_rank==1]$ref1)

# fouled-foul ratio by ref-team
frequencies[, foul_ratio:=fouled/fouls]
frequencies[team=='seattle' & games>3][order(foul_ratio)]
ranks = frequencies[order(team, foul_ratio)]
ranks = ranks[games>3]
ranks[, ref_rank:=seq_len(.N), by='team']
ranks[ref_rank==1]
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Run Regressions

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Prep Regression Output

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Graph

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Return output

# -------------------------------------------------------------------------
