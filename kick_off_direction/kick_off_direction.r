# ----------------------------------------------
# 6/28/2017
# Analyze crowd-sourced kickoff direction data
# ----------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(googlesheets)
library(data.table)
library(tools)
library(nnet)
library(MASS)
library(RColorBrewer)
library(ggplot2)
# --------------------


# ----------------------------------------------
# Files and directories

# data location
dataLocation = gs_title('Crowd Sourced Sounders Data')

# data by half
inFile = './webscrape/boxscores_for_every_match.csv'

# output file
outFile = './kick_off_direction/graphs.pdf'
# ----------------------------------------------


# ----------------------------------------------
# Load/prep data

# load
data = gs_read(ss=dataLocation, ws='Sheet1')

# format
data = as.data.table(data)
setnames(data, as.character(data[2]))
setnames(data, c('Score Home', 'Score Away', 'X7'), c('Score_Home', 'Score_Away', 'Direction'))

# subset observations
data = data[!is.na(Direction) & !is.na(Winner)]

# format variables
data[, Date:=as.Date(Date, '%m/%d/%Y')]
data[, Direction:=tolower(Direction)]
data[, Score_Home:=as.numeric(Score_Home)]
data[, Score_Away:=as.numeric(Score_Away)]

# additional variables
data[, Win:=Winner=='seattle']
data[Winner=='seattle', Outcome:='Win']
data[Winner=='draw', Outcome:='Draw']
data[!Outcome %in% c('Win','Draw'), Outcome:='Loss']
data[, Goal_Difference:=Score_Home-Score_Away]
# ----------------------------------------------


# ----------------------------------------------
# Load/prep boxscores for 1st half goals

# load
boxScores = fread(inFile)

# subset rows/columns
boxScores = boxScores[team_home=='seattle']
setnames(boxScores, 'date', 'Date')
boxScores = boxScores[, c('Date', 'team', paste0('goal_', seq(4))), with=FALSE]

# reshape long
boxScores = melt(boxScores, id.vars=c('Date','team'), value.name='minute', variable.name='goal')

# convert minutes to numeric
boxScores[minute=='', minute:=NA]
boxScores[minute=='HT', minute:='45']
boxScores[minute=='EHT', minute:='45']
boxScores[minute=='EOR', minute:='90']
boxScores[grepl('90\'+', minute), minute:='90']
boxScores[grepl('45\'+', minute), minute:='45']
boxScores[, minute:=as.numeric(gsub('\'','',minute))]
boxScores = boxScores[!is.na(minute)]
boxScores[, goal:=as.numeric(gsub('goal_','',goal))]

# collapse to halfs
boxScores[minute<=45 | (minute>90 & minute<=105), half:=1]
boxScores[is.na(half), half:=2]
boxScores = boxScores[, list(Goals=sum(goal)), by=c('Date', 'team','half')]
boxScores[team=='seattle', Score_Home1:=sum(Goals), by=c('Date','half')]
boxScores[team!='seattle', Score_Away1:=sum(Goals), by=c('Date','half')]
boxScores = boxScores[, list(Score_Home1=sum(Score_Home1, na.rm=TRUE), Score_Away1=sum(Score_Away1, na.rm=TRUE)), by=c('Date', 'half')]


# keep only half 1
boxScores = boxScores[half==1]

# merge
boxScores[, Date:=as.Date(Date)]
data = merge(data, boxScores, by='Date', all.x=TRUE)
# ----------------------------------------------


# ------------------------------------------------------------
# Run analyses

# odds of winning based on direction
glmFit1 = glm(Win~Direction, 'binomial', data)

# multinomial logit of w/l/d
glmFit2 = multinom(Outcome~Direction, data)

# correlations between direction and team to prove it's not necessary to control for anything
glmFit3 = glm(Direction=='left'~Opponent, 'binomial', data)

# correlations between direction and competition to prove it's not necessary to control for anything
glmFit4 = glm(Direction=='left'~Competition, 'binomial', data)

# average goals for, against, difference by direction
glmFit5 = glm.nb(Score_Home~Direction, data)
glmFit6 = glm.nb(Score_Away~Direction, data)
glmFit7 = lm(Goal_Difference~Direction, data)

# average goals for in the first half
glmFit8 = glm.nb(Score_Home1~Direction, data)
# ------------------------------------------------------------


# ------------------------------------------------------------
# Predict

# set up data
predData = data.table(Direction=unique(data$Direction))

# predict from important regressions

# 1
predData[, win_odds:=predict(glmFit1, newdata=predData)]
predData[, win_odds_se:=predict(glmFit1, newdata=predData, se.fit=TRUE)$se]
predData[, win_odds_lower:=win_odds - (1.96*win_odds_se)]
predData[, win_odds_upper:=win_odds + (1.96*win_odds_se)]
predData[, win_odds:=exp(win_odds)]
predData[, win_odds_lower:=exp(win_odds_lower)]
predData[, win_odds_upper:=exp(win_odds_upper)]
predData$win_odds_se=NULL

# 7
predData[, goal_difference:=predict(glmFit7, newdata=predData)]
predData[, goal_difference_se:=predict(glmFit7, newdata=predData, se.fit=TRUE)$se]
predData[, goal_difference_lower:=goal_difference - (1.96*goal_difference_se)]
predData[, goal_difference_upper:=goal_difference + (1.96*goal_difference_se)]
predData$goal_difference_se=NULL

# 8
predData[, first_half_goals:=predict(glmFit8, newdata=predData)]
predData[, first_half_goals_se:=predict(glmFit8, newdata=predData, se.fit=TRUE)$se]
predData[, first_half_goals_lower:=first_half_goals - (1.96*first_half_goals_se)]
predData[, first_half_goals_upper:=first_half_goals + (1.96*first_half_goals_se)]
predData[, first_half_goals:=exp(first_half_goals)]
predData[, first_half_goals_lower:=exp(first_half_goals_lower)]
predData[, first_half_goals_upper:=exp(first_half_goals_upper)]
predData$first_half_goals_se=NULL
# ------------------------------------------------------------


# ------------------------------------------------------------
# Set up to graph
graphData = melt(predData, id.vars='Direction')
graphData[grepl('lower',variable), est:='lower']
graphData[grepl('upper',variable), est:='upper']
graphData[is.na(est), est:='mid']
graphData[, variable:=gsub('_lower', '', variable)]
graphData[, variable:=gsub('_upper', '', variable)]
graphData = dcast.data.table(graphData, Direction+variable~est)
graphData[upper>5, upper:=5] # cap at 5 for visual purposes

graphData[variable=='first_half_goals', variable:='Average First Half Goals']
graphData[variable=='win_odds', variable:='Odds of Winning']
graphData[variable=='goal_difference', variable:='Average Goal Difference']
graphData[Direction=='left', Direction:='Toward North\nEnd Faithful']
graphData[Direction=='right', Direction:='Toward ECS\nSection']
# ------------------------------------------------------------


# ----------------------------------------------
# Graph

# colors
cols = brewer.pal(6, 'Paired')

# store graph
p = ggplot(graphData, aes(y=mid, ymin=lower, ymax=upper, x=Direction)) + 
	geom_bar(stat='identity', fill=cols[4]) + 
	geom_errorbar(color='grey45', width=.25) + 
	facet_wrap(~variable, ncol=3, scales='free') + 
	labs(title='Sounders Expected Success by Kickoff Direction',y='',x='') + 
	theme_bw() + 
	theme(axis.text.x=element_text(size=12), axis.text.y=element_text(size=14), 
		plot.title=element_text(hjust=.5, size=16))
# ----------------------------------------------


# --------------------------------
# Save graphs
pdf(outFile, height=6, width=9)
p
dev.off()
# --------------------------------
