# Analyze ref bias
# outcomes: cards given and pks against, analyses: does salazar (repeat for others) give more cards to some teams than others? and does he give more cards than other refs? maybe control for total fouls by team


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(MASS)
library(boot)
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
matchData = matchData[competition %in% comps]
data = merge(data, matchData, by=c('team_home','team_away','date'), all.x=TRUE)
data = data[competition %in% comps]
setkey(data, NULL)

# isolate ref tenure
tenures = data[, c('date','ref1'), with=FALSE]
tenures = tenures[, list(start=min(date), end=max(date)), by='ref1']
tenures[, tenure:=paste(start, '-', end)]
tenures = tenures[, c('ref1','tenure'), with=FALSE]

# count pks
for(p in seq(4)) {
	var = paste0('pk_',p)
	data[get(var)=='', (var):=NA]
	data[, (var):=as.numeric(!is.na(get(var)))]
}
data[, total_pks:=pk_1+pk_2+pk_3+pk_4]

# turn it into pks against by matching up who played who
total_pks = copy(data[, c('team_home','team_away','team','total_pks','ref1','date'), with=FALSE])
total_pks[, team:=ifelse(team==team_home, team_away, team_home)]
total_pks = total_pks[, list(total_pks=sum(total_pks)), by=c('team_home','team_away','team','ref1','date')]

# make card variables
data[, yellow:=!is.na(yellow) & yellow!='']
data[, straightred:=!is.na(straightred) & straightred!='']
data[, secondyellow:=!is.na(secondyellow) & secondyellow!='']

# count cards and pks by match NOT FINISHED
byVars = c('ref1', 'team', 'opposition','date')
data[, opposition:=ifelse(team==team_home, team_away, team_home)]
matchTotals = data[, list('yellows'=sum(yellow, na.rm=TRUE), 'straightreds'=sum(straightred, na.rm=TRUE), 'secondyellows'=sum(secondyellow, na.rm=TRUE), 'fouls'=sum(fouls, na.rm=TRUE), 'fouled'=sum(fouled, na.rm=TRUE)), by=byVars]


# count cards and pks by ref-team
byVars = c('ref1', 'team')
frequencies = data[, list('yellows'=sum(yellow, na.rm=TRUE), 'straightreds'=sum(straightred, na.rm=TRUE), 'secondyellows'=sum(secondyellow, na.rm=TRUE), 'fouls'=sum(fouls, na.rm=TRUE), 'fouled'=sum(fouled, na.rm=TRUE)), by=byVars]
total_pks = total_pks[,list('pks'=sum(total_pks, na.rm=TRUE), 'games'=.N), by=byVars]

# merge cards and pks
frequencies = merge(frequencies, total_pks, by=byVars)

# add ref tenures
frequencies = merge(frequencies, tenures, by='ref1')

# make "harmful calls"
frequencies[, harmful_calls:=straightreds+secondyellows+pks]

# make team totals
frequencies[, total_fouls:=sum(fouls), by='team']
frequencies[, total_games:=sum(games), by='team']
frequencies[, fouls_per_game:=total_fouls/total_games]

# collapse frequencies to team and ref levels
sumVars = c('yellows','straightreds','secondyellows','pks','harmful_calls','fouls','fouled','games')
teamFreqs = frequencies[, lapply(.SD, sum), .SDcols=sumVars, by='team']
refFreqs = frequencies[, lapply(.SD, sum), .SDcols=sumVars, by=c('ref1','tenure')]

# game-level data
byVars = c('ref1', 'team_home', 'team_away', 'date')
sdCols = c('fouls', 'fouled', 'straightred', 'secondyellow', 'yellow', 'pk_1', 'pk_2', 'pk_3', 'pk_4')
matchData = data[team==team_home, lapply(.SD, sum), .SDcols=sdCols, by=byVars]
tmp = data[team==team_away, lapply(.SD, sum), .SDcols=sdCols, by=byVars]
setnames(matchData, c('team_home', 'team_away'), c('team','opposition'))
setnames(tmp, c('team_away', 'team_home'), c('team','opposition'))
matchData = rbind(matchData, tmp)
matchData[, pks:=pk_1+pk_2+pk_3+pk_4]
matchData[, harmful_calls:=straightred+secondyellow+pks]

# add opposing team's foul tendency
tmp = teamFreqs[, c('team','fouls','games'), with=FALSE]
tmp[, fouls_per_game:=fouls/games]
tmp = tmp[,c('team','fouls_per_game'),with=FALSE]
setnames(tmp, c('opposition','opposition_fouls_per_game'))
matchData = merge(matchData, tmp, 'opposition')

# formalize for tables
teamFreqs = formalizeTeamNames(teamFreqs)
refFreqs = formalizeTeamNames(refFreqs)
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Basic analysis

# cards/pks per 10 games by team
r = 10
teamFreqs[, ypg:=round(yellows/games*r,1)]
teamFreqs[, rpg:=round(straightreds/games*r,1)]
teamFreqs[, pkpg:=round(pks/games*r,1)]
teamFreqs[, hpg:=round(harmful_calls/games*r,1)]
teamFreqs[order(-hpg)] # don't forget, it's games since 2013 only

# cards/pks per 10 games by ref among sounders only
frequencies[, ypg:=round(yellows/games*r,1)]
frequencies[, sypg:=round(secondyellows/games*r,1)]
frequencies[, rpg:=round(straightreds/games*r,1)]
frequencies[, pkpg:=round(pks/games*r,1)]
frequencies[, hpg:=round(harmful_calls/games*r,1)]
frequencies[team=='seattle' & games>3][order(-hpg)]

# cards/pks per 10 games by ref
refFreqs[, ypg:=round(yellows/games*r,1)]
refFreqs[, rpg:=round(straightreds/games*r,1)]
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
# Run Regressions to control for team foul tendency

# run at individual game level to control for opposing team tendency
glmFit = glm.nb(harmful_calls ~ ref1 + opposition_fouls_per_game, data=matchData[team=='seattle'])

# # run at aggreagate level for simplicity
# nbFits = list()
# i=1
# for(t in unique(frequencies$team)) {
	# nbFits[[i]] = glm.nb(harmful_calls ~ ref1 + fouls, frequencies[team==t])
	# i=i+1
# }

# glm(harmful_calls ~ ref1*team, data=frequencies[games>2])
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Prep Regression Output

# # get output
# glmOutput = data.table(exp(cbind(coef(glmFit), confint(glmFit))))
# setnames(glmOutput, c('est','lower','upper'))
# glmOutput[, var:=names(coef(glmFit))]
# glmOutput[, var:=gsub('ref1','',var)]

# # identify reference ref
# refs = unique(matchData[team=='seattle']$ref1)
# reference = refs[!refs %in% glmOutput$var]
# intercept = glmOutput[var=='(Intercept)']$est
# glmOutput[!grepl('opposition',var) & var!='(Intercept)', est:=est+intercept]
# glmOutput[!grepl('opposition',var) & var!='(Intercept)', lower:=lower+intercept]
# glmOutput[!grepl('opposition',var) & var!='(Intercept)', upper:=upper+intercept]
# glmOutput[var=='(Intercept)', var:=reference]

# predict
predData = data.table(ref1=unique(matchData[team=='seattle']$ref1), opposition_fouls_per_game=mean(matchData$opposition_fouls_per_game))
predData[, expected_calls:=predict(glmFit, newdata=predData)]
# predData[, c('lower','upper'):=predict(glmFit, prediction.interval=TRUE, newdata=predData)]
predData[, se:=predict(glmFit, se.fit=TRUE, newdata=predData)$se.fit]
predData[, lower:=expected_calls-(1.96*se)]
predData[, upper:=expected_calls+(1.96*se)]
predData[, expected_calls:=exp(expected_calls)]
predData[, lower:=exp(lower)]
predData[, upper:=exp(upper)]

# add frequency
tmp = frequencies[team=='seattle', c('ref1','games'), with=FALSE]
predData = merge(predData, tmp, 'ref1')

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Graph

# truncate upper bounds
t = 2.5
predData[upper>t, upper:=Inf]

ggplot(predData, aes(y=expected_calls, ymax=upper, ymin=lower, x=reorder(ref1, -expected_calls))) + 
	geom_bar(stat='identity', fill='#08519c') + 
	geom_errorbar(color='gray40', width=.5, size=.5) + 
	geom_text(aes(label=games)) + 
	labs(title='Referee Tendencies', y='Expected Game-Changing Calls Per Game', x='') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16))

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Return output

# -------------------------------------------------------------------------
