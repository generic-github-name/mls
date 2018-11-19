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
graphFile = './short_rest/short_rest.pdf'

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
results = results[!is.na(home_rest)]
results[, home_short_rest:=home_rest<5]
results[, away_short_rest:=away_rest<5]

# exclude rest >31 days because that's just carryover from the previous season
results = results[home_rest<31]

# identify wins
results[, home_win:=ifelse(points_home==3,1,0)]
results[, away_win:=ifelse(points_away==3,1,0)]
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# Analysis 

# glm
fith = glm(home_win~home_rest, data=results, family='binomial')
fita = glm(away_win~away_rest, data=results, family='binomial')

# changepoint detection on the home team 
# This is my broke-ass version of DA Stephens introduction to Bayesian changepoint detection in 1994 (App. Stat.) and
# Paul Fearnhead Exact and efficient Bayesian inference for multiple changepoint problems, Stat Comput (2006)
j=1
for(i in seq(from=min(results$home_rest)+1, to=22)) { 
	fit = glm(home_win~home_rest, data=results[home_rest<i], family='binomial')
	tmp = data.table(window_start=min(results$home_rest), window_end=i, 
		coef=names(coef(fit)), coef(fit), confint(fit), type='Too Short Rest')
	if(j==1) coefs = copy(tmp)
	if(j>1) coefs = rbind(coefs, tmp)
	j=j+1
}
for(i in seq(from=min(results$home_rest)+1, to=21)) { 
	fit = glm(home_win~home_rest, data=results[home_rest>i], family='binomial')
	tmp = data.table(window_start=i, window_end=max(results$home_rest), 
		coef=names(coef(fit)), coef(fit), confint(fit), type='Too Long Rest')
	coefs = rbind(coefs, tmp)
}
setnames(coefs, c('window_start','window_end','coef','est','lower','upper','type'))

# spline with cutpoints at 5 and 15 (the most likely cutpoints according to above)
results[, s1:=(home_rest>=5)*home_rest]
results[, s2:=(home_rest>=15)*home_rest]
fith_spline = glm(home_win~home_rest+s1+s2, data=results, family='binomial')
# ----------------------------------------------------------------------



# -----------------------------------------------------------------------------
# Graph

# evaluate home rest
evalh = data.table(home_rest=seq(2,30))
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
	geom_point(data=results[home_rest<=30, .(home_win=mean(home_win)), by=home_rest], aes(y=home_win, x=home_rest), inherit.aes=FALSE) + 
	labs(title='Home Team', y='Win Percentage', x='Days of Rest') +
	theme_bw()
	
# evaluate away rest
evala = data.table(away_rest=seq(2,30))
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
	geom_point(data=results[away_rest<=30, .(away_win=mean(away_win)), by=away_rest], aes(y=away_win, x=away_rest), inherit.aes=FALSE) + 
	labs(title='Away Team', y='Win Percentage', x='Days of Rest') +
	theme_bw()
	
# combine
grid.arrange(p1, p2)

# graph changepoint results
p3 = ggplot(coefs[coef=='home_rest' & type=='Too Short Rest'], aes(y=est, x=window_end, ymax=upper, ymin=lower)) + 
	geom_pointrange() + 
	geom_hline(yintercept=0, color='red') +
	labs(title='Slope with different degrees of short rest', y='Coefficient (expected win percentage)', x='End of window') + 
	theme_bw()
	
# graph changepoint results
p4 = ggplot(coefs[coef=='home_rest' & type=='Too Long Rest'], aes(y=est, x=window_start, ymax=upper, ymin=lower)) + 
	geom_pointrange() + 
	geom_hline(yintercept=0, color='red') +
	labs(title='Slope with different degrees of long rest', y='Coefficient (expected win percentage)', x='Start of window') + 
	theme_bw()
	
# graph showing the selected changepoints
p5 = ggplot(results[home_rest<=5, .(home_win=mean(home_win)), by=home_rest], aes(y=home_win, x=home_rest)) + 
	geom_point() + 
	geom_smooth(method='lm') + 
	geom_point(data=results[home_rest>=5 & home_rest<=15, .(home_win=mean(home_win)), by=home_rest]) + 
	geom_smooth(method='lm', data=results[home_rest>=5 & home_rest<=15, .(home_win=mean(home_win)), by=home_rest]) + 
	theme_bw()
	
# fuck it, just show a loess
aggh = results[home_rest<=14 & home_rest>=2, .(home_win=mean(home_win), N=.N), by=home_rest]
agga =results[away_rest<=14 & away_rest>=2, .(away_win=mean(away_win), N=.N), by=away_rest]
p6 = ggplot(aggh, aes(y=home_win, x=home_rest, size=N)) + 
	geom_point() + 
	geom_smooth(aes(weight=N), show.legend=FALSE) + 
	scale_x_continuous(breaks=seq(3,16, by=2)) + 
	labs(title='Win Percentage Compared to Duration of Rest', subtitle='Home Teams Only', 
		y='Average Win Percentage', x='Days of Rest', size='Number of Games\nSince 2015') + 
	theme_bw()
p7 = ggplot(agga, aes(y=away_win, x=away_rest, size=N)) + 
	geom_point() + 
	geom_smooth(aes(weight=N), show.legend=FALSE) + 
	scale_x_continuous(breaks=seq(2,16, by=2)) + 
	labs(title='Win Percentage Compared to Duration of Rest', subtitle='Away Teams Only', 
		y='Average Win Percentage', x='Days of Rest', size='Number of Games\nSince 2015') + 
	theme_bw()
	
# -----------------------------------------------------------------------------
