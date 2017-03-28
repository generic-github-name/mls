# Home field advantage
# idea: estimate who has the strongest home field advantage. logistic regression where the outcome is win/loss (what to do with draws? maybe the outcome is points?) and independent variables are 1) a K-game moving average of points and 2) dummy for home/away. The coefficient on variable 2 is the excess risk of winning at home!

# -------------------------
# set up R
rm(list=ls())
library(data.table)
library(zoo) # for rollmean
library(ggplot2)
# -------------------------


# --------------------------------------------------------
# input/output files
inFile = '../mls_webscrape/all_matches_in_mls_website.csv'
outFile = './home_field_advnatage_graphs_2012_plus.pdf'
source('../home_field_advantage/formalize_team_names.r')
# --------------------------------------------------------


# ----------------------------------------------------------------------------------
# load data
data = fread(inFile)

# drop future games
data = data[!is.na(winner)]

# drop internationals, friendlies and preseason games
comps = c('2015/16 CONCACAF Champions League', '2016/17 CONCACAF Champions League', 
			'MLS Cup', 	'MLS Playoffs', 'MLS Regular Season', 'U.S. Open Cup')
data = data[competition %in% comps]

# format date
data[, date:=as.Date(date)]

# compute points earned by the home team
data[, home_points:=0]
data[winner==team_home, home_points:=3]
data[winner=='draw', home_points:=1]
data[, away_points:=0]
data[winner==team_away, away_points:=3]
data[winner=='draw', away_points:=1]

# formalize team names
data = formalizeTeamNames(data)

# current teams
currentTeams = unique(data[year(date)==2016]$team_home)
# ----------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------
# compute K-game moving average of points to capture how good each team is at any given time
K = 20 # even numbers only
A = .25 # bandwidth for loess, higher is more smooth
i=1
teams = unique(data[competition=='MLS Regular Season' & year(date)<2017]$team_home)
for(t in teams) {
	# set up team-level data
	tmp = data[team_home==t | team_away==t]
	tmp[team_home==t, points:=home_points]
	tmp[team_away==t, points:=away_points]
	tmp[, team:=t]
	tmp[team_home!=t, home:=0]
	tmp[team_home==t, home:=1]
	tmp[, win:=points==3]
	
	# compute moving average
	tmp[, lag:=shift(points, n=K/2)] # get the 5-game lag so that the moving average can look forward and backward (not just forward)
	tmp[, forward_ma:=rollmean(points, K)] # get the forward-looking moving average
	tmp[is.na(lag), lag:=forward_ma] # impute the beginning of the time series with the forward moving average
	tmp[, ma:=rollmean(lag, K)]
	tmp[nrow(tmp):1, backward_ma:=rollmean(points, K)] # get the backward-looking moving average (this breaks the 'by' function)
	tmp[nrow(tmp):nrow(tmp)-5, ma:=backward_ma] # impute MA with the backward-looking MA for the last 5 rows

	# run loess as a alternative to MA
	l = loess(points~as.numeric(as.factor((date))), data=tmp, span=A)
	preds = predict(l)
	tmp[, loess:=preds]

	# run loess as a alternative to MA
	l = loess(points~as.numeric(as.factor((date))), data=tmp, span=A)
	preds = predict(l)
	tmp[, loess:=preds]

	idVars = c('team', 'date', 'home', 'points', 'win')
	if (i==1) analysisData = tmp[, c(idVars, 'ma', 'loess'), with=FALSE] 
	if (i>1) analysisData = rbind(analysisData, tmp[, c(idVars, 'ma', 'loess'), with=FALSE])
	i=i+1
}
# -------------------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------
# graph the moving average to check validity
p1a = ggplot(analysisData, aes(x=date, y=points)) + 
	geom_point(alpha=.1) + 
	geom_line(aes(y=ma, color='Moving Average'), size=.5) + 
	geom_line(aes(y=loess, color='Loess'), size=.5) + 
	facet_wrap(~team) + 
	scale_color_manual('', values=c('Moving Average'='#DD6E42', 'Loess'='blue')) + 
	labs(y='Points', x='', title='') + 
	theme_bw()+
	theme(strip.text.x=element_text(size=14))
	
p1b = ggplot(analysisData[team %in% c('Seattle Sounders','LA Galaxy','Portland')], aes(x=date, y=points)) + 
	geom_point(alpha=.1) + 
	geom_line(aes(y=ma, color='Moving Average'), size=.5) + 
	geom_line(aes(y=loess, color='Loess'), size=.5) + 
	facet_wrap(~team) + 
	scale_color_manual('', values=c('Moving Average'='#DD6E42', 'Loess'='blue')) + 
	labs(y='Points', x='', title='') + 
	theme_bw() + 
	theme(strip.text.x=element_text(size=14))
# ----------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------
# look at simple percentages home and away

# across the whole league
data[, mean(home_points==3)]
data[, mean(home_points==1)]
data[, mean(home_points==0)]

# goal differential
data[, gd:=score_home-score_away]
data[team_home %in% currentTeams, list(mean=mean(gd), sd=sd(gd)), by='team_home'][order(-mean)]

# compute overall, home and away win percentages (wide and long)
percentages_overall = analysisData[, mean(win), by='team']
percentages_long = analysisData[, mean(win), by=c('team','home')]
percentages = dcast.data.table(percentages_long, team~home)
percentages_long[, home:=ifelse(home==1, 'At Home', 'Away')]
setnames(percentages, c('1', '0'), c('home_pct', 'away_pct'))
percentages[, ratio:=home_pct/away_pct]
percentages_long[, home_pct:=max(V1), by='team'] # assumes everyone is better at home, which is true

# prep percentages
percentages = percentages[team %in% currentTeams]
percentages_long = percentages_long[team %in% currentTeams]
percentages_overall = percentages_overall[team %in% currentTeams]

# print to screen
percentages[order(-home_pct)]

# graph comparison of home and away percentages
p2 = ggplot(percentages_long, aes(y=V1*100, x=reorder(team, -home_pct), fill=home, shape=NULL, size=NULL)) + 
	geom_bar(stat='identity', position='dodge') + 
	geom_point(data=percentages_overall, aes(y=V1*100, x=team, group=team, color='Overall', shape='Overall', size='Overall', fill=NULL)) + 
	labs(title='Win Percentage at Home vs Away', y='Percentage of Games Won (All Competitions)', x='') + 
	scale_fill_manual('Win Percentage', values=c('#5A9367', '#969696')) + 
	scale_color_manual('', values=c('Overall'='black')) + 
	scale_size_manual(name='', values=c('Overall'=10)) +
	scale_shape_manual(name='', values=c('Overall'=95)) +
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16)) + 
	guides(shape=guide_legend(override.aes=list(fill='white')), fill=guide_legend(override.aes=list(shape=32))) # fill and sape get mixed up, remove the shapes from the fill guides and vice versa

# graph ratio of home and away percentages
p3 = ggplot(percentages, aes(y=ratio, x=reorder(team, -ratio))) + 
	geom_bar(stat='identity', fill='#206C99') + 
	geom_hline(yintercept=1, color='grey15', size=1, lty='solid') + 
	labs(title='Win Percentage at Home vs Away', y='Home-to-Away Win Ratio', x='') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16))
# ----------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------
# run regression
glmFit = glm(win ~ ma + home:team, family='binomial', data=analysisData)
summary(glmFit)

# prep estimates
estimates = data.table(exp(cbind(coef(glmFit), confint(glmFit))))
setnames(estimates, names(estimates), c('est', 'lower', 'upper'))
estimates[, coef:=names(coef(glmFit))]
estimates[grepl('ma', coef), variable:='ma']
estimates[grepl('home', coef), variable:='home']
estimates[, team:=gsub('team', '', coef)]
estimates[, team:=gsub('home:', '', team)]

# current teams
currentEstimates = estimates[team %in% currentTeams]
currentEstimates = droplevels(currentEstimates)

# formalize team names
estimates = formalizeTeamNames(estimates)
currentEstimates = formalizeTeamNames(currentEstimates)

# best team among current list
currentBestTeam = currentEstimates[est==max(est)]$team
# FFBF00
# graph home field advantage odds
p4 = ggplot(currentEstimates[variable=='home'], aes(y=est, ymin=lower, ymax=upper, x=reorder(team, -est))) + 
	geom_bar(stat='identity', fill='grey75') + 
	geom_errorbar(color='gray40', width=.5, size=1) + 
	geom_hline(yintercept=1, color='grey5', size=1.25, lty='solid') + 
	annotate(y=1.2, x=currentBestTeam, hjust=0, geom='text', label='Better at home', color='grey20') + 
	annotate(y=.85, x=currentBestTeam, hjust=0, geom='text', label='Worse at home', color='grey20') + 
	labs(title='Excess Likelihood of Winning at Home', y='Home Field Advantage (Odds Ratio)', x='') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16))

# best team including old teams
bestTeam = estimates[est==max(est)]$team
	
# graph home field advantage odds including old teams
p5 = ggplot(estimates[variable=='home'], aes(y=est, ymin=lower, ymax=upper, x=reorder(team, -est))) + 
	geom_bar(stat='identity', fill='grey75') + 
	geom_errorbar(color='gray40', width=.5, size=1) + 
	geom_hline(yintercept=1, color='grey5', size=1.25, lty='solid') + 
	annotate(y=1.3, x=bestTeam, hjust=0, geom='text', label='Better at home', color='grey20') + 
	annotate(y=.8, x=bestTeam, hjust=0, geom='text', label='Worse at home', color='grey20') + 
	labs(title='Excess Likelihood of Winning at Home\nIncluding Former Teams', y='Home Field Advantage (Odds Ratio)', x='') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16))
# ----------------------------------------------------------------------------------


# -------------
pdf(outFile, height=6, width=10)
p1a
p1b
p2
p3
p4
p5
dev.off()
# -------------
