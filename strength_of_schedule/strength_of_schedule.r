# --------------------------------------------------------
# Estimate strength of schedule based on home and away ppg
# --------------------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(stats)
library(nnet)
library(RColorBrewer)
library(ggplot2)
# --------------------


# ----------------------------------------------
# Parameters and settings


# ----------------------------------------------


# --------------------------------------------------
# Files and directories

# input files
resultsFile = './webscrape/ASA/2018/Game Information.csv'
fixtureFile = './webscrape/fixtures2018.csv'

# output files
graphFile1 = './strength_of_schedule/strength_of_schedule.pdf'
graphFile2 = './strength_of_schedule/simulated_outcomes.pdf'
tableFile = './strength_of_schedule/Western Conference Expected Rank 2018.csv'

# functions
source('./_common/formalize_team_names.r')
# --------------------------------------------------


# ----------------------------------------------------------------------
# Compute ppg home and away by team so far

# load
results = fread(resultsFile)

# standardize team names
results = formalizeTeamNames(results)

# date format
results[, date:=as.Date(date, '%m/%d/%Y')]

# identify points
results[hfinal>afinal, points_home:=3]
results[hfinal==afinal, points_home:=1]
results[hfinal<afinal, points_home:=0]
results[afinal>hfinal, points_away:=3]
results[afinal==hfinal, points_away:=1]
results[afinal<hfinal, points_away:=0]

# aggregate points home and away for each team
homeResults = results[, list(ppg_home=mean(points_home)), by='hteam']
awayResults = results[, list(ppg_away=mean(points_away)), by='ateam']

# merge ppg to game-specific results
results = results[, c('date','ateam','hteam','points_home', 'points_away'), with=FALSE]
results = merge(results, homeResults, by='hteam')
results = merge(results, awayResults, by='ateam')

# manually add the latest week
# ----------------------------------------------------------------------


# ---------------------------------------------------------------------------------
# Merge results to fixtures

# load
fixtures = fread(fixtureFile)

# subset columns
fixtures = fixtures[, c('team_home','team_away','date'), with=FALSE]

# format date
fixtures[, date:=as.Date(date, '%A, %b %d %Y')]

# standardize team names
fixtures = formalizeTeamNames(fixtures)

# make sure no completed matches are in the fixtures
fixtures = fixtures[date>max(results$date)]

# merge
fixtures = merge(fixtures, homeResults, by.x='team_home', by.y='hteam', all.x=TRUE)
fixtures = merge(fixtures, awayResults, by.x='team_away', by.y='ateam', all.x=TRUE)
# ---------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------
# Sum ppg of the opposing team across each team

# set up data table for strength
strength = data.table()
for(t in unique(c(unique(fixtures$team_home), unique(fixtures$team_away)))) {
	fixtures[, opposition:=ifelse(team_home==t, ppg_away, ppg_home)]
	m = mean(fixtures[team_home==t | team_away==t]$opposition)
	n = nrow(fixtures[team_home==t | team_away==t])
	strength = rbind(strength, data.table(team=t, mean_opposition_points=m, games_remaining=n))
} 

# identify conference
west = c('Colorado', 'FC Dallas', 'Sporting KC', 'LA Galaxy', 'San Jose', 'Real Salt Lake', 'Chivas USA', 'Houston', 'Seattle Sounders', 'Portland', 'Vancouver', 'Minnesota United', 'LAFC')
strength[team %in% west, conference:='West']
strength[!team %in% west, conference:='East']

# round
strength[, lab:=round(mean_opposition_points,2)]
# ---------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------------------------------
# Simulate rest of season

# run regression
mFit = multinom(factor(points_home)~ppg_away*ppg_home, results)

# predict
inSample = cbind(results, predict(mFit, type='probs', newdata=results))
sims = cbind(fixtures, predict(mFit, type='probs', newdata=fixtures))
setnames(sims, c('0','1','3'), c('p_loss', 'p_draw', 'p_win'))
setnames(inSample, c('0','1','3'), c('p_loss', 'p_draw', 'p_win'))
probs = copy(sims)

# evaluate in-sample fit
inSample[, correct_prediction:=p_win>p_draw & p_win>p_loss & points_home==3]
inSample[p_draw>p_win & p_draw>p_loss & points_home==1, correct_prediction:=TRUE]
inSample[p_loss>p_win & p_loss>p_draw & points_home==0, correct_prediction:=TRUE]
table(inSample$correct_prediction)

# simulate
nDraws = 1000
draws = data.table()
for(i in seq(nrow(sims))) {
	var = paste0('result',i)
	tmpDraws = rmultinom(nDraws, 1, prob=as.numeric(sims[i,c('p_loss', 'p_draw', 'p_win'),with=FALSE]))
	tmpDraws = data.table(t(tmpDraws))
	tmpDraws[V1==1, result:=0]
	tmpDraws[V2==1, result:=1]
	tmpDraws[V3==1, result:=3]
	draws = rbind(draws, t(tmpDraws$result))
} 
sims = cbind(sims, draws)

# determine results in simulations
drawVars = paste0('V',seq(nDraws))
idVars = names(sims)[!names(sims) %in% drawVars]
sims = melt(sims, id.vars=idVars, variable.name='draw_number', value.name='points_home')
sims[points_home==3, points_away:=0]
sims[points_home==1, points_away:=1]
sims[points_home==0, points_away:=3]
sims[, draw_number:=as.numeric(gsub('V', '', draw_number))]

# aggregate to total points
totalPoints = data.table()
for(t in unique(c(unique(fixtures$team_home), unique(fixtures$team_away)))) {
	results[, team_points:=ifelse(hteam==t, points_home, points_away)]
	sims[, team_points:=ifelse(team_home==t, points_home, points_away)]
	agg = results[hteam==t | ateam==t, list(team_points=sum(team_points))]
	simAgg = sims[team_home==t | team_away==t, list(remaining_points=sum(team_points)), by='draw_number']
	simAgg[, total_points:=agg[[1]]+remaining_points]
	simAgg[, team:=t]
	totalPoints = rbind(totalPoints, simAgg)
}

# rank teams by conference
totalPoints[team %in% west, conference:='West']
totalPoints[!team %in% west, conference:='East']
totalPoints[, conf_rank:=frank(-total_points), by=c('conference','draw_number')]
totalPoints[, ss_rank:=frank(-total_points), by=c('draw_number')]
# ----------------------------------------------------------------------------------------------------------


# --------------------------------------------------------------------
# Print some statistics from simulations

# expected points for the Sounders
totalPoints[team=='Seattle Sounders', mean(total_points+3)] # add 3 because the results data are behind by one game

# upper/lower points for the Sounders
totalPoints[team=='Seattle Sounders', quantile(total_points+3, c(.025, .975))] # add 3 because the results data are behind by one game
totalPoints[team=='Seattle Sounders', quantile(total_points+3, c(.1, .9))] # add 3 because the results data are behind by one game
totalPoints[team=='Seattle Sounders', quantile(total_points+3, c(.25, .75))] # add 3 because the results data are behind by one game

# proportion of simulations where the Sounders make the playoffs
nrow(totalPoints[team=='Seattle Sounders' & conf_rank<=6.5])/nDraws

# proportion of simulations where the Sounderscome in top 2
nrow(totalPoints[team=='Seattle Sounders' & conf_rank<=2.5])/nDraws

# proportion of sims where the Sounders are #1 in the west
nrow(totalPoints[team=='Seattle Sounders' & conf_rank<=1.5])/nDraws

# proportion of sims where the Sounders win the supporters shield
nrow(totalPoints[team=='Seattle Sounders' & ss_rank<=1.5])/nDraws

# average and upper/lower quantiles final position for the Sounders
totalPoints[team=='Seattle Sounders', mean(conf_rank)]
totalPoints[team=='Seattle Sounders', quantile(conf_rank, c(.25, .75))]
totalPoints[team=='Seattle Sounders', quantile(conf_rank, c(.10, .90))]
totalPoints[team=='Seattle Sounders', quantile(conf_rank, c(.05, .95))]

# average final SS position for the Sounders
totalPoints[team=='Seattle Sounders', mean(ss_rank)]
totalPoints[team=='Seattle Sounders', quantile(ss_rank, c(.20, .80))]
totalPoints[team=='Seattle Sounders', quantile(ss_rank, c(.05, .95))]

# total playoff berths by team
totalPoints[, sum(conf_rank<=6.5)/nDraws, by='team'][order(-V1)]

# total SSs by team
totalPoints[, sum(ss_rank<=1.5)/nDraws, by='team'][order(-V1)]

# expected points/rankings averaged across all sims
table = totalPoints[conference=='West', list(conf_rank=mean(conf_rank), total_points=mean(total_points)), by='team'][order(-total_points)]
setnames(table, c('Team', 'Average Finish', 'Average Points'))
table
write.csv(table, tableFile, row.names=FALSE)

# remaining probs
probs[team_home=='Seattle Sounders' | team_away=='Seattle Sounders', c('team_home','team_away','date','p_loss','p_draw','p_win'), with=F][order(date)]

# remaining matchups
probs[team_home=='Seattle Sounders' | team_away=='Seattle Sounders', c('team_home','team_away','date','p_loss','p_draw','p_win'), with=F][order(date)]

# how many times did the sounders continue the streak to 9?
dates = unique(sims[team_away=='Seattle Sounders' | team_home=='Seattle Sounders'][order(date)]$date)
sims[(team_away=='Seattle Sounders' | team_home=='Seattle Sounders'), points_sounders:=ifelse(team_away=='Seattle Sounders', points_away, points_home)]
tmp = sims[team_away=='Seattle Sounders' | team_home=='Seattle Sounders']
tmp[, location:=ifelse(team_home=='Seattle Sounders', 'home', 'away')]
tmp = dcast.data.table(tmp, draw_number~date, value.var='points_sounders')
tmp[, mean(get('2018-09-16')==3)]
tmp[, mean(get('2018-09-16')==3 & get('2018-09-20')==3)]
tmp[, mean(get('2018-09-16')==3 & get('2018-09-20')==3 & get('2018-09-24')==3)]
tmp[, mean(get('2018-09-16')==3 & get('2018-09-20')==3 & get('2018-09-24')==3 & get('2018-09-29')==3)]
tmp[, mean(get('2018-09-16')==3 & get('2018-09-20')==3 & get('2018-09-24')==3 & 
			get('2018-09-29')==3 & get('2018-10-09')==3)]
tmp[, mean(get('2018-09-16')==3 & get('2018-09-20')==3 & get('2018-09-24')==3 & 
			get('2018-09-29')==3 & get('2018-10-09')==3 & get('2018-10-18')==3)]
tmp[, mean(get('2018-09-16')==3 & get('2018-09-20')==3 & get('2018-09-24')==3 & 
			get('2018-09-29')==3 & get('2018-10-09')==3 & get('2018-10-18')==3 & get('2018-10-21')==3)]
tmp[, mean(get('2018-09-16')==3 & get('2018-09-20')==3 & get('2018-09-24')==3 & 
			get('2018-09-29')==3 & get('2018-10-09')==3 & get('2018-10-18')==3 & get('2018-10-21')==3 & 
			get('2018-10-28')==3)]
			
# sims in which the sounders lose next weekend's game?
badscenariodraws = unique(sims[team_home=='Seattle Sounders' & date=='2018-09-29' & points_home==0]$draw_number)
badscenarios = sims[draw_number %in% badscenariodraws]
totalPointsbad = data.table()
for(t in unique(c(unique(fixtures$team_home), unique(fixtures$team_away)))) {
	results[, team_points:=ifelse(hteam==t, points_home, points_away)]
	badscenarios[, team_points:=ifelse(team_home==t, points_home, points_away)]
	agg = results[hteam==t | ateam==t, list(team_points=sum(team_points))]
	simAgg = badscenarios[team_home==t | team_away==t, list(remaining_points=sum(team_points)), by='draw_number']
	simAgg[, total_points:=agg[[1]]+remaining_points]
	simAgg[, team:=t]
	totalPointsbad = rbind(totalPointsbad, simAgg)
}
totalPointsbad[team %in% west, conference:='West']
totalPointsbad[!team %in% west, conference:='East']
totalPointsbad[, conf_rank:=frank(-total_points), by=c('conference','draw_number')]
totalPointsbad[, ss_rank:=frank(-total_points), by=c('draw_number')]
tablebad = totalPointsbad[conference=='West', list(conf_rank=mean(conf_rank), total_points=mean(total_points)), by='team'][order(-total_points)]
setnames(tablebad, c('Team', 'Average Finish', 'Average Points'))
tablebad
			
# sims in which the sounders lose the next two games?
badscenariodraws1 = unique(sims[team_home=='Seattle Sounders' & date=='2018-09-29' & points_home==0]$draw_number)
badscenariodraws2 = unique(sims[team_home=='Seattle Sounders' & date=='2018-10-09' & points_home==0]$draw_number)
badscenariodraws = badscenariodraws1[badscenariodraws1 %in% badscenariodraws2]
badscenarios = sims[draw_number %in% badscenariodraws]
totalPointsbad = data.table()
for(t in unique(c(unique(fixtures$team_home), unique(fixtures$team_away)))) {
	results[, team_points:=ifelse(hteam==t, points_home, points_away)]
	badscenarios[, team_points:=ifelse(team_home==t, points_home, points_away)]
	agg = results[hteam==t | ateam==t, list(team_points=sum(team_points))]
	simAgg = badscenarios[team_home==t | team_away==t, list(remaining_points=sum(team_points)), by='draw_number']
	simAgg[, total_points:=agg[[1]]+remaining_points]
	simAgg[, team:=t]
	totalPointsbad = rbind(totalPointsbad, simAgg)
}
totalPointsbad[team %in% west, conference:='West']
totalPointsbad[!team %in% west, conference:='East']
totalPointsbad[, conf_rank:=frank(-total_points), by=c('conference','draw_number')]
totalPointsbad[, ss_rank:=frank(-total_points), by=c('draw_number')]
tablebad = totalPointsbad[conference=='West', list(conf_rank=mean(conf_rank), total_points=mean(total_points)), by='team'][order(-total_points)]
setnames(tablebad, c('Team', 'Average Finish', 'Average Points'))
tablebad
# --------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------------------------
# Graph the strength of remaining schedule by team

# set up to graph
fillColors = c('#6d819c', '#55967e')

# caption
cap = expression(paste(bold('Opponents\' Mean PPG '), '| Games Remaining'))

# range for plot
sd = round(sd(strength$mean_opposition_points),1)
min = min(strength$mean_opposition_points)-sd
max = max(strength$mean_opposition_points)+sd

# graph
strengthPlot = ggplot(strength, aes(x=reorder(team, mean_opposition_points), y=mean_opposition_points, fill=conference)) + 
	geom_bar(aes(y=rep(max(strength$mean_opposition_points), nrow(strength))), stat='identity', fill='#e4e7ec') + 
	geom_bar(stat='identity') + 
	geom_text(aes(label=lab), hjust=1.25, color='grey90', fontface='bold') + 
	geom_text(aes(label=games_remaining), hjust=-.25, color='grey10') + 
	coord_flip(ylim=c(min, max)) + 
	scale_y_continuous(labels=c('Easier', '', '', 'Harder'), breaks=c(min, min+sd, min+sd+sd, max-sd)) + 
	scale_fill_manual('', values=fillColors) + 
	labs(title='Strength of Schedule', x='', y='', caption=cap) +
	theme_minimal() + 
	theme(plot.title=element_text(hjust=.5, size=16), 
		axis.ticks.x=element_blank(), panel.grid = element_blank(), 
		axis.text.y=element_text(size=14), plot.caption=element_text(size=7))
# ---------------------------------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------------------------
# Graph simulation results

pointsPlot = ggplot(totalPoints[conference=='West'], aes(x=total_points)) + 
	# geom_line(aes(y=..density..), stat='density', size=1.5, color=fillColors[1]) + 
	geom_histogram(fill=fillColors[1]) + 
	# geom_dotplot(fill=fillColors[1], color=NA) + 
	facet_wrap(~team, scales='free_y', nrow=2) + 
	scale_x_continuous(breaks = scales::pretty_breaks(n=2)) +
	labs(title='Simulated Points at End of Season',subtitle=paste('Distribution of', nDraws, 'Simulated Finishes to 2017 Season'), 
		y='Number of Finishes', x='Points at End of Season') + 
	theme_bw() + 
	theme(plot.title=element_text(hjust=.5, size=16), plot.subtitle=element_text(hjust=.5, size=12), 
		axis.text.x=element_text(size=12), axis.text.y=element_blank(), axis.ticks.y=element_blank())

confRankPlot = ggplot(totalPoints[conference=='West'], aes(x=conf_rank)) + 
	# geom_line(aes(y=..density..), stat='density', size=1.5, color=fillColors[2]) + 
	geom_histogram(fill=fillColors[2]) + 
	geom_vline(xintercept=6.5, color='red') + 
	# geom_dotplot(fill=fillColors[2], color=NA) + 
	facet_wrap(~team, scales='free_y', nrow=2) + 
	scale_x_reverse(breaks=scales::pretty_breaks(n=4)) + 
	labs(title='Simulated Standings at End of Season',subtitle=paste('Distribution of', nDraws, 'Simulated Finishes to 2017 Season'), 
		y='Number of Finishes', x='Position at End of Season') + 
	theme_bw() + 
	theme(plot.title=element_text(hjust=.5, size=16), plot.subtitle=element_text(hjust=.5, size=12), 
		axis.text.x=element_text(size=12), axis.text.y=element_blank(), axis.ticks.y=element_blank())

		
# ggplot(totalPoints[team=='Seattle Sounders'][draw_number%in%sample(seq(1000),100)], aes(x=conf_rank)) + 
	# geom_dotplot(fill=fillColors[2], method="histodot")
	
	
# ---------------------------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# Graph probabilities of win

# melt data to team-level
sims[, gameID:=seq_len(.N), by='draw_number']
idVars = names(sims)[!names(sims) %in% c('team_away','team_home')]
melt = melt(sims[draw_number==1], id.vars=idVars, variable.name='location', value.name='team')
melt2 = melt[, c('team','location','gameID')]
setnames(melt2, 'team', 'opponent')
melt2[, location:=ifelse(location=='team_away', 'team_home', 'team_away')]
melt = merge(melt, melt2, by=c('location','gameID'))
melt[, gameID:=seq_len(.N), by='team']

# the probabilities are the probabilities for the home team, reverse away games now that it's team-level
melt[location=='team_away', tmp:=p_win]
melt[location=='team_away', p_win:=p_loss]
melt[location=='team_away', p_loss:=tmp]
melt$tmp = NULL

# set up colors
divergingColors = rev(c(fillColors[2], '#769F7D', '#e5da99', '#df671c'))

# graph
western_pwins = ggplot(melt[team %in% west], aes(x=date, y=team, fill=p_win, label=opponent)) + 
	geom_tile() + 
	# geom_label() + 
	scale_fill_gradientn('Probability\nof Win', colors=divergingColors) + 
	labs(y='', x='Game Date', title='Remaining Fixtures') + 
	theme_bw(base_size=16)
	
# graph ties
western_pdraws = ggplot(melt[team %in% west], aes(x=date, y=team, fill=p_draw+p_win)) + 
	geom_tile() + 
	scale_fill_gradientn('Probability\nof Win or Draw', colors=divergingColors) + 
	labs(y='', x='Game Date', title='Remaining Fixtures') + 
	theme_bw(base_size=16)
# ------------------------------------------------------------------------------------------


# --------------------------------
# Save graphs
pdf(graphFile1, height=6, width=6)
strengthPlot
dev.off()
pdf(graphFile2, height=5, width=10)
pointsPlot
confRankPlot
western_pwins
dev.off()
# --------------------------------
