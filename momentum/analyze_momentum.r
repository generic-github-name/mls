# ----------------------------------------------
# 11/11/2017
# Analysis of momentum
# Idea: probability of winning should be a function how good both teams are plus the result of the previous game (if momentum is real)
# Gaps between games need ot matter though, maybe consider having a 2-week gap to be like coming of a loss?
# ----------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(np)
library(RColorBrewer)
library(ggplot2)
# --------------------


# --------------------------------------------------------
# Parameters

# size of a gap in games that constitutes a break in momentum (days)
g = 8
# --------------------------------------------------------


# --------------------------------------------------------
# input/output files
inFileHistorical = './webscrape/all_matches_in_mls_website.csv'
inFile2017 = './webscrape/ASA/2017/Game Information.csv'
outFile = './historical_trends/graphs.pdf'
tableFile = './historical_trends/ppg_table.csv'
source('./_common/formalize_team_names.r')
# --------------------------------------------------------


# ----------------------------------------------
# Load/prep data

# load data
oldData = fread(inFileHistorical)
newData = fread(inFile2017)

# format dates
oldData[, date:=as.Date(date)]
newData[, date:=as.Date(date, '%m/%d/%Y')]

# drop before 2017 from old data
oldData = oldData[year(date)<2017]

# rename/subset new data to match old data
newNames = c('team_home','team_away','score_home','score_away')
setnames(newData, c('hteam','ateam','hfinal','afinal'), newNames)
newData = newData[, c('date', newNames), with=FALSE]
newData[, winner:=ifelse(score_home>score_away, team_home, team_away)]
newData[score_home==score_away, winner:='draw']
newData[, date:=as.Date(date, '%m/%d/%Y')]

# only mls and playoffs
oldData = oldData[competition %in% c('MLS Regular Season','MLS Playoffs')]
newData[, competition:='MLS Regular Season']

# rbind
data = rbind(newData, oldData)

# formalize team names
data = formalizeTeamNames(data)

# ----------------------------------------------

# ----------------------------------------------------------------------
# Compute ppg home and away by team so far

# identify points
data[score_home>score_away, points_home:=3]
data[score_home==score_away, points_home:=1]
data[score_home<score_away, points_home:=0]
data[score_away>score_home, points_away:=3]
data[score_away==score_home, points_away:=1]
data[score_away<score_home, points_away:=0]

# aggregate points home and away for each team
data[, year:=year(date)]
homeResults = data[, list(ppg_home=mean(points_home)), by=c('team_home', 'year')]
awayResults = data[, list(ppg_away=mean(points_away)), by=c('team_away', 'year')]

# merge ppg to game-specific results
data = merge(data, homeResults, by=c('team_home', 'year'))
data = merge(data, awayResults, by=c('team_away', 'year'))
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# Identify previous result in a variety of ways and "reshape" to team perspective

# make sure its sorted
data = data[order(date)]

# loop over teams
i=1
for(t in unique(data$team_home)) {
	tmpData = data[team_home==t | team_away==t]
	tmpData[, team:=t]
	tmpData[, home:=team_home==t]
	tmpData[, opponent:=ifelse(home==TRUE, team_away, team_home)]
	tmpData[, goals:=ifelse(home==TRUE, score_home, score_away)]
	tmpData[, opponent_goals:=ifelse(home==TRUE, score_away, score_home)]
	tmpData[, ppg:=ifelse(home==TRUE, ppg_home, ppg_away)]
	tmpData[, opponent_ppg:=ifelse(home==TRUE, ppg_away, ppg_home)]
	tmpData = tmpData[, c('team','date','winner','home','opponent','goals','opponent_goals','ppg','opponent_ppg'), with=FALSE]
	tmpData[, win:=as.numeric(team==winner)]
	
	# identify current run of form (consecutive wins)
	tmp_streaks = rle(as.character(tmpData$win))$lengths
	streak_idx = seq(length(tmp_streaks))
	streak_idx = rep(streak_idx, tmp_streaks)
	
	# identify the streak that they're currenly on, not the one they will ultimately reach
	streak_distance = ave(streak_idx, streak_idx, FUN=seq_along)
	tmpData[, current_run:=streak_distance]
	tmpData[win==FALSE, current_run:=NA]
	
	# shift the streak to identify the streak that they went into the game with, not the one that resulted
	tmpData[, current_run:=shift(current_run)]
	
	# "coming off a win" indicator
	tmpData[, coming_off_win:=shift(winner==t)]
	tmpData[, gap:=as.numeric(date-shift(date))]
	tmpData[gap>g, coming_off_win:=FALSE]
	tmpData[is.na(coming_off_win), coming_off_win:=FALSE]

	# categorical result of previous game
	tmpData[shift(winner==t), previous_result:='Win']
	tmpData[shift(winner=='draw'), previous_result:='Draw']
	tmpData[shift(winner!=t) & shift(winner!='draw'), previous_result:='Loss']
	tmpData[gap>g, previous_result:='Break']

	# add to new dataset
	if (i==1) analysisData = tmpData 
	if (i>1) analysisData = rbind(analysisData, tmpData)
	i=i+1
}
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------------------
# Analyze

# whether "coming_off_win" effects odds of winning
glmFit1 = glm(win ~ ppg + opponent_ppg + coming_off_win, 'binomial', analysisData)

# whether previous_result effects odds of winning
glmFit2 = glm(win ~ ppg + opponent_ppg + previous_result, 'binomial', analysisData)

# whether having a long gap affects odds of continuing a streak
glmFit3 = glm(win ~ ppg + opponent_ppg + gap, 'binomial', analysisData[current_run>3])
npFit1 = npcdensbw(win ~ gap, data=analysisData[current_run>3])
npFit2 = npconmode(win ~ gap, data=analysisData[current_run>3])
# ----------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------
# Counterfactual to show results

# isolate range of ppg
min = round(min(analysisData$ppg),2)
max = round(max(analysisData$ppg), 2)
s = seq(min, max, by=.01)

# expand grid
cfData = data.table(expand.grid(ppg=s, opponent_ppg=s))
n = nrow(cfData)
cow = rep(c(TRUE, FALSE), each=n)
cfData = rbind(cfData, cfData)
cfData[, coming_off_win:=cow]

# predict 
cfData[, odds:=predict(glmFit1, newdata=cfData)]
cfData[, se:=predict(glmFit1, newdata=cfData, se.fit=TRUE)$se.fit]
cfData[, upper:=odds+1.96*se]
cfData[, lower:=odds-1.96*se]
cfData[, odds:=exp(odds)]
cfData[, upper:=exp(upper)]
cfData[, lower:=exp(lower)]
# ----------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------
# Graph counterfactual

# set up to graph
graphData = copy(cfData)
graphData[, coming_off_win:=ifelse(coming_off_win==TRUE, 'Coming off Win', 'Not C')]

# graph highlighting a realistic example like the next game
shome = round(homeResults[team_home=='Seattle Sounders' & year==2017]$ppg_home, 2)
saway = round(awayResults[team_away=='Seattle Sounders' & year==2017]$ppg_away, 2)
hhome = round(homeResults[team_home=='Houston' & year==2017]$ppg_home, 2)
haway = round(awayResults[team_away=='Houston' & year==2017]$ppg_away, 2)
ggplot(cfData[ppg==hhome & opponent_ppg==saway], aes(y=1/odds, ymin=1/lower, ymax=1/upper, x=coming_off_win)) + 
	geom_bar(stat='identity') + 
	geom_errorbar()
	labs(tite='Seattle Sounders at Houston Dynamo', y='Odds of Winning', x='Momentum') + 
	theme_bw() + 
	theme(axis.text.x=c(''))

# line graph
# ggplot()

# graph how gaps affect streaks in a non-parametric way
plots = list()
for(i in seq(7)) {
	plots[[i]] = ggplot(analysisData[current_run>i & gap<25, mean(win), by='gap'], aes(y=V1, x=gap)) + 
		geom_point() + 
		geom_smooth() + 
		labs(y='Probability of Win', x='Length of Rest', title=paste(i,'Games'))
}
grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], plots[[7]], top=textGrob('Length of Streak Going into the Game'))
# ----------------------------------------------------------------------------------
