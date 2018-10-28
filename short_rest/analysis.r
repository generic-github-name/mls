# --------------------------------------------------------
# Some analysis of whether short rest periods impact probability of points
# [code incomplete]
# --------------------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(np)
library(boot)
library(RColorBrewer)
library(ggplot2)
library(gridExtra)
# --------------------


# ----------------------------------------------
# Parameters and settings


# ----------------------------------------------


# --------------------------------------------------
# Files and directories

# input files
resultsFile2015 = './webscrape/ASA/2015/Game Information.csv'
resultsFile2016 = './webscrape/ASA/2016/Game Information.csv'
resultsFile2017 = './webscrape/ASA/2017/Game Information.csv'
resultsFile2018 = './webscrape/ASA/2018/Game Information.csv'

# output files
graphFile = './strength_of_schedule/strength_of_schedule.pdf'

# functions
source('./_common/formalize_team_names.r')
# --------------------------------------------------


# ----------------------------------------------------------------------
# Compute ppg home and away by team so far

# load
results2015 = fread(resultsFile2015)
results2016 = fread(resultsFile2016)
results2017 = fread(resultsFile2017)
results2018 = fread(resultsFile2018)

# append
results = rbind(results2015, results2016, fill=TRUE)
results = rbind(results, results2017, fill=TRUE)
results = rbind(results, results2018, fill=TRUE)

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

# compute the number of days since last game per team
for(t in unique(c(results$ateam, results$hteam))) { 
	dates = results[ateam==t | hteam==t]$date
	dates = rev(dates[order(dates)])
	rests = as.numeric(dates - dates[2:length(dates)])
	rests[length(rests)]=NA
	i=1
	for(d in dates) { 
		results[hteam==t & date==d, home_rest:=rests[i]]
		results[ateam==t & date==d, away_rest:=rests[i]]
		i=i+1
	}
}
results[, home_short_rest:=home_rest<5]
results[, away_short_rest:=away_rest<5]

# identify wins
results[, home_win:=ifelse(points_home==3,1,0)]
results[, away_win:=ifelse(points_away==3,1,0)]

# glm
fith = glm(home_win~home_rest+I(home_rest^2)+I(home_rest^3), data=results, family='binomial')
fita = glm(away_win~away_rest+I(away_rest^2)+I(away_rest^3), data=results, family='binomial')

# non-parametric options
# fit = npcdensbw(formula=factor(home_win)~home_rest, data=results)
# fit = npconmode(home_win~home_rest, data=results)

# evaluate home rest
evalh = data.table(home_rest=seq(2,15))
evalh = evalh[, away_rest:=mean(results$away_rest, na.rm=TRUE)]
evalh[, prediction:=predict(fith, newdata=evalh)]
evalh[, se:=predict(fith, newdata=evalh, se.fit=TRUE)$se.fit]
evalh[, upper:=prediction+1.95*se]
evalh[, lower:=prediction-1.95*se]
evalh[, prediction:=inv.logit(prediction)]
evalh[, upper:=inv.logit(upper)]
evalh[, lower:=inv.logit(lower)]
p1 = ggplot(evalh, aes(y=prediction, x=home_rest, ymin=lower, ymax=upper)) + 
	geom_ribbon(fill='grey75') + 
	geom_line() + 
	geom_point(data=results[home_rest<=15, .(home_win=mean(home_win)), by=home_rest], aes(y=home_win, x=home_rest), inherit.aes=FALSE) + 
	labs(title='Away Team', y='Win Percentage', x='Days of Rest') +
	theme_bw()
	
# evaluate away rest
evala = data.table(away_rest=seq(2,15))
evala = evala[,home_rest:=mean(results$home_rest, na.rm=TRUE)]
evala[, prediction:=predict(fita, newdata=evala)]
evala[, se:=predict(fita, newdata=evala, se.fit=TRUE)$se.fit]
evala[, upper:=prediction+1.95*se]
evala[, lower:=prediction-1.95*se]
evala[, prediction:=inv.logit(prediction)]
evala[, upper:=inv.logit(upper)]
evala[, lower:=inv.logit(lower)]
p2 = ggplot(evala, aes(y=prediction, x=away_rest, ymin=lower, ymax=upper)) + 
	geom_ribbon(fill='grey75') + 
	geom_line() + 
	geom_point(data=results[away_rest<=15, .(away_win=mean(away_win)), by=away_rest], aes(y=away_win, x=away_rest), inherit.aes=FALSE) + 
	labs(title='Away Team', y='Win Percentage', x='Days of Rest') +
	theme_bw()
	
# combine
grid.arrange(p1, p2)


