# Analyze unbeaten streaks and whether the sounders were part of the streak
# output files saved in the current working directory

# set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(RColorBrewer)

# input/output files
inFile = './all_matches_in_mls_website.csv'
outFile = './streak_graphs.pdf'

# load data
data = fread(inFile)

# look at only MLS regular season that has already happened
data = data[competition=='MLS Regular Season' & !is.na(winner)]

# drop two matches with erroneous dates (found manually. there could be more!)
data = data[!which(team_away=='ny-red-bulls' & team_home=='atlanta' & date==as.Date('2017-03-11'))] # these appeared with two dates
data = data[!which(team_away=='colorado' & team_home=='ny-red-bulls' & date==as.Date('2017-03-19'))]

# identify teams
teams = unique(data$team_home)

# function to identify streaks including everyone they beat along the way
idStreaks = function(homeOnly=FALSE, t=NA) {
	# isolate streaks for the current team
	if (homeOnly) team_data = data[team_home==t]
	if (!homeOnly) team_data = data[team_away==t | team_home==t]
	team_data = team_data[order(date, team_home)]
	team_data[, unbeaten:=winner %in% c(t, 'draw')]
	tmp_streaks = rle(as.character(team_data$unbeaten))$lengths
	streak_idx = seq(length(tmp_streaks))
	streak_idx = rep(streak_idx, tmp_streaks)
	tmp_streaks = rep(tmp_streaks, tmp_streaks)
	team_data[, streak_length:=tmp_streaks]
	team_data[, streak_id:=streak_idx]
	team_streaks = team_data[unbeaten==TRUE | shift(unbeaten)==TRUE] # get streaks including the first loss after the streak
	team_streaks[, game_id:=seq_len(.N), by=streak_id]
	
	# isolate list of opponents during each streak
	if (nrow(team_streaks)>0) {
		opponents = team_streaks[, c('team_away', 'team_home', 'streak_id', 'game_id'), with=FALSE]
		opponents = melt(opponents, id.vars=c('streak_id', 'game_id'))
		opponents = opponents[value!=t]
		opponents = dcast.data.table(opponents, streak_id ~ game_id)
		ncol = ncol(opponents)-1
		setnames(opponents, as.character(seq(ncol)), paste0('opponent', seq(ncol)))
	}
	
	# collapse to just start/end/who broke it
	streak_start = team_streaks[game_id==1 & unbeaten==TRUE, c('date', 'streak_id', 'streak_length'), with=FALSE]
	setnames(streak_start, 'date', 'streak_start')
	streak_end = team_streaks[game_id==streak_length & unbeaten==TRUE, c('date', 'streak_id'), with=FALSE]
	setnames(streak_end, 'date', 'streak_end')
	streak_breaker = team_streaks[game_id==1 & unbeaten==FALSE, c('date', 'streak_id', 'winner'), with=FALSE]
	setnames(streak_breaker, c('date', 'winner'), c('streak_broken', 'streak_breaker'))
	streak_breaker[, streak_id:=streak_id-1]
	team_streaks = merge(streak_start, streak_end, 'streak_id')
	team_streaks = merge(team_streaks, streak_breaker, 'streak_id', all.x=TRUE)
	if (nrow(team_streaks)>0) team_streaks = merge(team_streaks, opponents, by='streak_id')
	team_streaks$team = t
	
	# return
	return(team_streaks)
}

# identify unbeaten streaks
streaks = NULL
for(t in teams) {
	
	# run function
	team_streaks = idStreaks(homeOnly=FALSE, t=t)
	
	# add the current team's streaks to the list
	streaks = rbind(streaks, team_streaks, fill=TRUE)
}

# identify conference
west = c('colorado', 'fc-dallas', 'sporting-kc', 'la-galaxy', 'san-jose', 'real-salt-lake', 'chivas-usa', 'houston', 'seattle', 'portland', 'vancouver')
streaks[team %in% west, conference:='West']
streaks[!team %in% west, conference:='East']

# relabel streak ID's
streaks[, streak_id:=as.numeric(as.factor(paste0(team, streak_id)))]

# reduce to streaks of K or more
K = 8
streaks = streaks[streak_length>=K]

# throw out older streaks
# streaks = streaks[year(streak_end)>2009]

# estimate the probability that each team will end a streak that's in progress
analysisData = melt(streaks, id.vars=c('team', 'streak_id', 'streak_broken', 'streak_length', 'streak_breaker'), measure.vars=names(streaks)[grepl('opponent', names(streaks))])
analysisData[, opponent:=as.numeric(gsub('opponent', '', variable))]
analysisData = analysisData[opponent>=8]
analysisData[opponent==streak_length+1, value:=streak_breaker]
analysisData = analysisData[!is.na(value)]
analysisData[, ended:=opponent==streak_length+1]
analysisData[value=='seattle']
analysisData[value=='nycfc']

# table of success rates
successRates = analysisData[, mean(ended), by='value']
successRates[order(-V1)]

# do we face teams who end up making long runs?
analysisData[, mean(streak_length), by='value'][order(-V1)]

# make a specified team the reference team
refTeam = 'seattle'
analysisData[value==refTeam, value:=paste0('1', refTeam)]

# run regression
glmFit = glm(ended ~ factor(value), 'binomial', analysisData[value!='nycfc'])
probs = data.table(p_end=exp(coef(glmFit)), p_lower=exp(confint(glmFit))[,1], p_upper=exp(confint(glmFit))[,2], team=names(coef(glmFit)))
probs[is.na(p_lower), p_upper:=NA]
probs[p_upper>15, p_upper:=15]
probs[, team:=gsub('factor\\(value)', '', team)]
probs[, team:=gsub('\\(', '', team)]
probs[, team:=gsub(')', '', team)]

# graph
ggplot(probs, aes(y=p_end, ymin=p_lower, ymax=p_upper, x=reorder(team, -p_end))) + 
	geom_bar(stat='identity') + 
	geom_errorbar(color='gray30') + 
	geom_hline(yintercept=1, color='red', size=2, lty='dashed') + 
	annotate(y=1.3, x=Inf, hjust=1, geom='text', label='Better than Sounders') + 
	annotate(y=.8, x=Inf, hjust=1, geom='text', label='Worse than Sounders') + 
	labs(title='Odds of an Ongoing Streak Ending\nRelative to the Seattle Sounders', y='Odds Ratio', x='') + 
	theme_bw() + 
	theme(axis.text.x=element_text(angle=315, hjust=0), plot.title=element_text(hjust=.5))
