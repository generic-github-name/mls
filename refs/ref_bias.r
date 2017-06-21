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

# only mls competitions and current teams
matchData = fread(matchFile)
comps = c('MLS Cup', 'MLS Playoffs', 'MLS Regular Season')
matchData = matchData[competition %in% comps]
data = merge(data, matchData, by=c('team_home','team_away','date'), all.x=TRUE)
data = data[competition %in% comps]
setkey(data, NULL)
currentTeams = unique(data[year(date)==2017]$team)
data = data[team %in% currentTeams]
matchData = matchData[team_away %in% currentTeams & team_home %in% currentTeams]

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

# count games by referee to exclude rare refs in regressions
matchData[, games_reffed:=.N, by='ref1']
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

# estimate harmful calls per game by ref and team, then compare expected to observed to see if there's bias
frequencies[hpg==0, hpg_offset:=.25]
frequencies[is.na(hpg_offset), hpg_offset:=hpg]
glmFit = glm.nb(hpg ~ ref1 + team, data=frequencies)

# predict
frequencies[, hpg_pred:=predict(glmFit)]
frequencies[, hpg_pred_se:=predict(glmFit, newdata=frequencies, se.fit=TRUE)$se]
frequencies[, hpg_pred_lower:=hpg_pred-(1.96*hpg_pred_se)]
frequencies[, hpg_pred_upper:=hpg_pred+(1.96*hpg_pred_se)]
frequencies[, hpg_pred:=exp(hpg_pred)]
frequencies[, hpg_pred_lower:=exp(hpg_pred_lower)]
frequencies[, hpg_pred_upper:=exp(hpg_pred_upper)]
frequencies = formalizeTeamNames(frequencies)

# regression to show each referee's tendencies
glmFitRef = glm.nb(harmful_calls ~ ref1, data=matchData[games_reffed>4])

# regression to show each team's tendencies
glmFitTeam = glm.nb(harmful_calls ~ team, data=matchData)

# predict average HCPG for referees
predsRef = data.table(ref1=unique(matchData[games_reffed>4]$ref1))
predsRef[, hpg_pred:=predict(glmFitRef, newdata=predsRef)]
predsRef[, hpg_pred_se:=predict(glmFitRef, newdata=predsRef, se.fit=TRUE)$se]
predsRef[, hpg_pred_lower:=hpg_pred-(1.96*hpg_pred_se)]
predsRef[, hpg_pred_upper:=hpg_pred+(1.96*hpg_pred_se)]
predsRef[, hpg_pred:=exp(hpg_pred)*r]
predsRef[, hpg_pred_lower:=exp(hpg_pred_lower)*r]
predsRef[, hpg_pred_upper:=exp(hpg_pred_upper)*r]

# predict average HCPG for teams
predsTeam = data.table(team=unique(matchData$team))
predsTeam[, hpg_pred:=predict(glmFitTeam, newdata=predsTeam)]
predsTeam[, hpg_pred_se:=predict(glmFitTeam, newdata=predsTeam, se.fit=TRUE)$se]
predsTeam[, hpg_pred_lower:=hpg_pred-(1.96*hpg_pred_se)]
predsTeam[, hpg_pred_upper:=hpg_pred+(1.96*hpg_pred_se)]
predsTeam[, hpg_pred:=exp(hpg_pred)*r]
predsTeam[, hpg_pred_lower:=exp(hpg_pred_lower)*r]
predsTeam[, hpg_pred_upper:=exp(hpg_pred_upper)*r]
predsTeam = formalizeTeamNames(predsTeam)

# count how often a ref is significantly above the line
frequencies[, biased:=hpg_pred_upper<hpg]
frequencies[games>3, sum(biased), by='ref1'][order(-V1)]
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Graph

# colors
fillColor1 = brewer.pal(8, 'GnBu')[8]
fillColor2 = brewer.pal(8, 'YlGn')[8]
teamColors = c('Atlanta United'='#9D2235', 'Chicago'='#102141', 'Colorado'='#862633', 'Columbus'='#FFF200', 'DC United'='#000000', 'FC Dallas'='#BF0D3E', 'Houston'='#F68712', 'LA Galaxy'='#00245D', 'Montreal'='#00529B', 'New England'='#C63323', 'NYCFC'='#69ACE5', 'NY Red Bulls'='#ED1E36', 'Orlando'='#612B9B', 'Philadelphia'='#B1872D', 'Portland'='#004812', 'Real Salt Lake'='#B30838', 'San Jose'='#0D4C92', 'Seattle Sounders'='#5D9732', 'Sporting KC'='#93B1D7', 'Toronto FC'='#E31937', 'Vancouver'='#00245E', 'Minnesota'='#8CD2F4')

# labels
predsRef[, lab:=round(hpg_pred, 1)]
predsTeam[, lab:=round(hpg_pred, 1)]
frequencies[, lab:=paste0(ref1, '\n', team)]

# graph each referee's tendencies
p1 = ggplot(predsRef, aes(y=hpg_pred, x=reorder(ref1, hpg_pred), ymin=hpg_pred_lower, ymax=hpg_pred_upper)) + 
	geom_bar(stat='identity', fill=fillColor1) + 
	geom_errorbar(width=.5) + 
	geom_text(data=predsRef, aes(label=lab), hjust=1.25, color='grey90', fontface='bold') + 
	coord_flip() + 
	labs(title=paste('Average Game-Changing Calls per', r, 'Games'), y='', x='') +
	theme_minimal() + 
	theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), 
		panel.grid.major.y = element_blank(), legend.position='none', 
		plot.title=element_text(hjust=.5, size=16), axis.title.y=element_text(size=14))

# graph each team's tendencies
p2 = ggplot(predsTeam, aes(y=hpg_pred, x=reorder(team, hpg_pred), ymin=hpg_pred_lower, ymax=hpg_pred_upper)) + 
	geom_bar(stat='identity', fill=fillColor2) + 
	geom_errorbar(width=.5) + 
	geom_text(data=predsTeam, aes(label=lab), hjust=1.25, color='grey90', fontface='bold') + 
	coord_flip() + 
	labs(title=paste('Average Game-Changing Calls per', r, 'Games'), y='', x='') +
	theme_minimal() + 
	theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), 
		panel.grid.major.y = element_blank(), legend.position='none', 
		plot.title=element_text(hjust=.5, size=16), axis.title.y=element_text(size=14))

# graph expected HCPG vs observed
p3 = ggplot(frequencies[games>3], aes(x=hpg_pred, xmin=hpg_pred_lower, xmax=hpg_pred_upper, y=hpg)) + 
	geom_point(color=fillColor1) + 
	geom_point(data=frequencies[team=='Seattle Sounders' & games>3], fill='#5D9732', size=2.5, shape=22, color='black') + 
	geom_abline(slope=1, intercept=0) + 
	geom_errorbarh(data=frequencies[hpg-hpg_pred>5.5 & games>3], height=.15) + 
	geom_text(data=frequencies[hpg-hpg_pred>5.5 & games>3], aes(label=lab), size=2.5) + 
	labs(title=paste('Expected vs Actual Game-Changing Calls per', r, 'Games'), 
		y='Actual Game-Changing Calls', x='Expected Game-Changing Calls') + 
	theme_bw() + 
	theme(plot.title=element_text(hjust=.5, size=16), axis.title.y=element_text(size=14), 
	axis.title.x=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=9)) 

# graph expected HCPG vs observed
p4 = ggplot(frequencies[team=='Seattle Sounders' & games>3], aes(x=hpg_pred, y=hpg)) + 
	geom_abline(slope=1, intercept=0) + 
	geom_text(aes(label=paste0(ref1, ' (', games, ')')), size=2.5) + 
	labs(title=paste('Expected vs Actual Game-Changing Calls per', r, 'Games'), 
		y='Actual Game-Changing Calls', x='Expected Game-Changing Calls') + 
	theme_bw() + 
	theme(plot.title=element_text(hjust=.5, size=16), axis.title.y=element_text(size=14), 
	axis.title.x=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=9))

# graph expected HCPG vs observed with error bars
refList = c('Silviu Petrescu', 'Allen Chapman', 'Ricardo Salazar', 'Baldomero Toledo', 'Alan Kelly')
p5 = ggplot(frequencies[team=='Seattle Sounders' & games>3], aes(x=hpg_pred, xmin=hpg_pred_lower, xmax=hpg_pred_upper, y=hpg)) + 
	geom_abline(slope=1, intercept=0) + 
	geom_text(aes(label=paste0(ref1, ' (', games, ')')), size=2.5) + 
	geom_errorbarh(data=frequencies[team=='Seattle Sounders' & ref1 %in% refList], height=.15, color='grey50') + 
	geom_point(data=frequencies[team=='Seattle Sounders' & ref1 %in% refList], color=fillColor1, shape=21) + 
	labs(title=paste('Expected vs Actual Game-Changing Calls per', r, 'Games'), 
		y='Actual Game-Changing Calls', x='Expected Game-Changing Calls') + 
	theme_bw() + 
	theme(plot.title=element_text(hjust=.5, size=16), axis.title.y=element_text(size=14), 
	axis.title.x=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=9))

# numbers that go along with the graph
frequencies[team=='Seattle Sounders', c('hpg', 'hpg_pred', 'games','ref1'), with=F][order(hpg-hpg_pred)]
	
# histogram of residuals
p6 = ggplot(frequencies[games>3], aes(x=hpg-hpg_pred)) + 
	geom_histogram(fill=fillColor2, color='grey75') + 
	labs(title=paste('Difference Between Expected and Actual Game-Changing Calls per', r, 'Games'), 
		y='Number of Games', x='Difference') + 
	theme_bw() + 
	theme(plot.title=element_text(hjust=.5, size=16), axis.title.y=element_text(size=14), 
	axis.title.x=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=9)) 

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Return output
pdf(outFile, height=6, width=9)
p1
p2
p3
p4
p5
p6
dev.off()
# -------------------------------------------------------------------------
