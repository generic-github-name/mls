# Explore the times when a player "should have shot it"
# Idea: look at unsuccessful dribbles/passes in high-xG zones


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(nnet) # for multinomial logit
library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(grid)
library(jpeg)
# --------------------


# ------------------------------------------------------------
# Files/directories/settings

# settings
include2016 = TRUE
emphasizeSeattle = TRUE

# 2016 data files
dribbleFile2016 = './webscrape/ASA/2016/dribbles.csv'
passFile2016 = './webscrape/ASA/2016/raw passes.csv'
shotFile2016 = './webscrape/ASA/2016/raw shots.csv'

# 2017 data files
dribbleFile2017 = './webscrape/ASA/2017/dribbles.csv'
passFile2017 = './webscrape/ASA/2017/raw passes.csv'
shotFile2017 = './webscrape/ASA/2017/raw shots.csv'

# shapefile of field
outLines = './_common/Soccer_Fields/soccer_field.jpg'

# output file
outFile = './shots_not_taken/followup_analysis.pdf'
# ------------------------------------------------------------


# --------------------------------------------------------------------------------------
# Load/prep data

# load passes, shots and dribbles
if (include2016) { 
	dribbles2016 = fread(dribbleFile2016)
	passes2016 = fread(passFile2016)
	shots2016 = fread(shotFile2016)
}
dribbles2017 = fread(dribbleFile2017)
passes2017 = fread(passFile2017)
shots2017 = fread(shotFile2017)

# append the two years
if (include2016) { 
	dribbles = rbind(dribbles2016, dribbles2017)
	passes = rbind(passes2016, passes2017)
	shots = rbind(shots2016, shots2017)
}
if (!include2016) { 
	dribbles = dribbles2017
	passes =  passes2017
	shots = shots2017
}

# only look at shots in open play on the ground
shots = shots[grepl('foot', bodypart) & patternOfPlay %in% c('Regular', 'Fastbreak')]

# convert y to distance from center
dribbles[, y:=abs(y-50)]
passes[, y:=abs(y-50)]
shots[, y:=abs(y-50)]

# binary result
shots[, goal:=result=='Goal']

# combine all actions for later
setnames(passes, 'passer', 'player')
setnames(shots, 'shooter', 'player')
dribbles[, action:='Dribble']
passes[, action:='Pass']
shots[, action:='Shot']
actions = rbind(dribbles, passes, shots, fill=TRUE)

# identify game state
actions = actions[hscore>=0 & ascore>=0]
actions[hscore==ascore, state:='Tied']
actions[team==hteam & hscore>ascore, state:='Winning']
actions[team==hteam & hscore<ascore, state:='Losing']
actions[team!=hteam & hscore<ascore, state:='Winning']
actions[team!=hteam & hscore>ascore, state:='Losing']
# --------------------------------------------------------------------------------------


# ------------------------------------------------------------
# Compute probability of shot/dribble/pass/dribble among all actions
# conditional on the game state
glmFit = glm(goal ~ x + y, 'binomial', shots)
actions[, xg:=exp(predict(glmFit, actions))]
mnFitT = multinom(action ~ state*team*xg, actions)
mnFit = multinom(action ~ state*xg, actions)
# ------------------------------------------------------------


# ------------------------------------------------------------
# Set up counterfactual
cfDataAgg = data.table(unique(actions$state))
cfDataAgg[, xg:=.25]
setnames(cfDataAgg, c('state', 'xg'))
probs = predict(mnFit, cfDataAgg, type='probs')
cfDataAgg = cbind(cfDataAgg, probs)
cfDataLongAgg = melt(cfDataAgg, id.vars=c('state'))

cfData = data.table(expand.grid(unique(actions$team), unique(actions$state)))
cfData[, xg:=.25]
# cfData = cfData[!is.na(state)]
setnames(cfData, c('team','state', 'xg'))
probs = predict(mnFitT, cfData, type='probs')
cfData = cbind(cfData, probs)
cfDataLong = melt(cfData, id.vars=c('state','team'))
# ------------------------------------------------------------


# ------------------------------------------------------------
# Graph
west = c('Colorado', 'FC Dallas', 'Kansas City', 'L.A. Galaxy', 'San Jose', 'Salt Lake', 'Houston', 'Seattle', 'Portland', 'Vancouver', 'Minnesota United')
cfDataLong[team %in% west, conference:='West']
cfDataLong[!team %in% west, conference:='East']

colors = brewer.pal(6, 'Paired')[c(2,3,5)]

p0 = ggplot(cfDataLongAgg[variable=='Shot' & conference=='West'], aes(y=value, x=state, fill=state)) + 
	geom_bar(stat='identity', position='dodge') + 
	labs(title='Shot Probability by Team and Game State', subtitle='Western Conference', y='Probability of Shooting', x='') + 
	scale_fill_manual('Game State', values=colors) + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16))

p1 = ggplot(cfDataLong[variable=='Shot' & conference=='West'], aes(y=value, x=team, fill=state)) + 
	geom_bar(stat='identity', position='dodge') + 
	labs(title='Shot Probability by Team and Game State', subtitle='Western Conference', y='Probability of Shooting', x='') + 
	scale_fill_manual('Game State', values=colors) + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16))

p2 = ggplot(cfDataLong[variable=='Shot' & conference=='East'], aes(y=value, x=team, fill=state)) + 
	geom_bar(stat='identity', position='dodge') + 
	labs(title='Shot Probability by Team and Game State', subtitle='Eastern Conference', y='Probability of Shooting', x='') + 
	scale_fill_manual('Game State', values=colors) + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16))

pdf(outFile, height=6, width=10)
p1
p2
dev.off()
# ------------------------------------------------------------

