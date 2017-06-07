# predict results using defensive surfaces


# ------------------
# Set up R
rm(list=ls())
library(data.table)
library(MASS)
library(boot)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
# ------------------


# ------------------------------------------------------------------
# Files, directories and settings

# input file
inFile1 = './defensive_actions/team_surfaces1.rdata'
inFile2 = './webscrape/all_matches_in_mls_website.csv'

# output file
outFile = './defensive_actions/result_predictions.pdf'

# name conversions
source('./_common/formalize_team_names.r')
# ------------------------------------------------------------------


# ------------------------------------------------------------------
# Load/prep data

# load
load(inFile1)

# bring in goals scored
matchData = fread(inFile2)
matchData = matchData[competition %in% c('MLS Regular Season')]
matchData$competition = NULL
matchData = matchData[date>=min(cfData$date) & date<=max(cfData$date)]
matchData[, opposition:=paste0(team_home, team_away)]

# reshape to team level
matchData = melt(matchData, id.vars=c('date','winner','opposition'),measure.vars=patterns('team_*','score_*'), value.name=c('team','goals'), variable.name='home')
matchData[, home:=as.character(home)]
matchData[home=='1', home:='away']
matchData[home=='2', home:='home']
matchData[, date:=as.Date(date)]
for(i in seq(nrow(matchData))) matchData[i, opposition:=gsub(team, '', opposition)]
matchData = matchData[!duplicated(matchData[, c('team','date'),with=F])] # errors on MLS database???

# merge to surfaces (including opposition)
data = merge(cfData, matchData, by=c('team','date'))
data = merge(data, cfData, by.x=c('opposition','date','x','y'), by.y=c('team','date','x','y'), suffixes=c('','_opposition'))
# ------------------------------------------------------------------


# ------------------------------------------------------------------
# Example of a match-up
p1 = ggplot(data[team=='seattle' & date=='2016-05-07'], aes(y=y,x=x,fill=pred)) + 
	geom_tile() + 
	# draw_grob(lines, x=-10, y=-9, width=120, height=118) + # this is bad. I have to match the location/scale to the aspect ratio of the output
	scale_fill_gradientn(paste('Actions\n Per\nPixel'), colors=brewer.pal(8, 'RdYlGn')) +
	labs(title='Expected Number of Defensive Actions (Seattle 2016-05-07)', y='Defensive End') + 
	theme_minimal()	+ 
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_blank(), 
	axis.ticks=element_blank(), plot.title=element_text(hjust=.5, size=16), line = element_blank())

p2 = ggplot(data[team=='seattle' & date=='2016-05-07'], aes(y=y,x=x,fill=pred_opposition)) + 
	geom_tile() + 
	# draw_grob(lines, x=-10, y=-9, width=120, height=118) + # this is bad. I have to match the location/scale to the aspect ratio of the output
	scale_fill_gradientn(paste('Actions\n Per\nPixel'), colors=brewer.pal(8, 'RdYlGn')) +
	labs(title='Expected Number of Defensive Actions (Opposition)', y='Defensive End') + 
	theme_minimal()	+ 
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_blank(), 
	axis.ticks=element_blank(), plot.title=element_text(hjust=.5, size=16), line = element_blank())
grid.arrange(p1, p2)
# ------------------------------------------------------------------


# ------------------------------------------------------------------
# Use the surfaces to predict goals
nbFit = glm.nb(goals ~ y*x*pred*pred_opposition, data)
data[team=='seattle', predGoals:=exp(predict(nbFit))]
aggData = data[team=='seattle', c('team','date','goals','predGoals'),with=FALSE]
aggData = aggData[, list(predicted_goals=mean(predGoals), sd=sd(predGoals)), by=c('team','date','goals')]
# ------------------------------------------------------------------


# ------------------------------------------------------------------
# Display predictions vs the truth
ggplot(aggData, aes(y=predicted_goals, x=goals)) + 
	geom_jitter(alpha=.5) + 
	theme_bw()
# ------------------------------------------------------------------