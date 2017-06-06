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
include2016 = FALSE
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
outFile = './shots_not_taken/passes_and_dribbles_2016_2017.pdf'
if (!include2016) outFile = gsub('_2016', '', outFile)
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

# # append passes and dribbles to get non-shots
# nonshots = rbind(passes, dribbles, fill=TRUE)
# --------------------------------------------------------------------------------------


# ------------------------------------------------------------
# Use shots data to evaluate "pseudo-expected goals" 
# (just based on location b/c thats all we know about passes/dribbles)

ggplot(shots, aes(x=x,y=y,shape=goal,color=goal)) + 
	geom_point()

glmFit = glm(goal ~ x + y, 'binomial', shots)
# ------------------------------------------------------------


# ---------------------------------------------------------------------------
# Graph pseudo-expected goals estimates

# load logo and store as grob
lines = readJPEG(outLines)
lines = rasterGrob(lines, interpolate=TRUE)

# manually replace white to transparent
rastercells = data.table(as.matrix(lines$raster))
for(var in names(rastercells)) rastercells[get(var)=='#FFFFFF', (var):='#00000000']
lines$raster = as.raster(as.matrix(rastercells))

# graph colors
colors = brewer.pal(8, 'RdYlGn')

# graph for validation
newdata = data.table(expand.grid(seq(0,100,5), seq(0,100,5)))
setnames(newdata, c('x','y'))
newdata[, log_odds:=predict(glmFit, newdata)]
tmp = copy(newdata)
tmp[, y:=-y]
newdata = rbind(newdata, tmp)
newdata[, xg:=exp(log_odds)]
pxg = ggplot(newdata, aes(x=x,y=y,fill=xg)) + 
	geom_tile() + 
	draw_grob(lines, x=-10, y=-9, width=120, height=118) + # this is bad. I have to match the location/scale to the aspect ratio of the output
	scale_fill_gradientn(paste('Odds'), colors=colors, na.value='grey75') +
	labs(title='Odds of Scoring', y='Defensive End') + 
	theme_minimal()	+ 
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_blank(), 
	axis.ticks=element_blank(), plot.title=element_text(hjust=.5, size=16), plot.subtitle=element_text(hjust=.5, size=14), 
	line = element_blank())
# ------------------------------------------------------------


# ------------------------------------------------------------
# Evalutate the pseudo-expected goals if the player had 
# somehow taken a shot from each action's location
actions[, xg:=exp(predict(glmFit, actions))]
# ------------------------------------------------------------


# ------------------------------------------------------------
# Compute probability of shot/dribble/pass/dribble among all actions
# conditional on the expected goals in that action's position

# by player (players with >x shots only)
actions[, total_shots:=sum(action=='shot'), by='player']
x=2
# mnFitP = multinom(action ~ xg*player, actions[total_shots>x])

# by team
mnFitT = multinom(action ~ xg*team, actions)
# ------------------------------------------------------------


# ------------------------------------------------------------
# Aggregate by player/team

# by player
# playerData = expand.grid(seq(0, max(actions$xg), by=.05), unique(actions$player))

# by team
teamData = data.table(expand.grid(seq(0, max(actions$xg), by=.05), unique(actions$team)))
setnames(teamData, c('xg','team'))
teamData[, pred:=predict(mnFitT, teamData)]
probs = predict(mnFitT, teamData, type='probs')
teamData = cbind(teamData, probs)
teamDataLong = melt(teamData, id.vars=c('xg','team','pred'))

# who likes to dribble/pass/shoot the most?
# teamData[, max_dribbler:=max(dribble), by='team']
# unique(teamData[,c('team','max_dribbler'),with=F])[order(-max_dribbler)]
# teamData[, max_shooter:=max(shot), by='team']
# unique(teamData[,c('team','max_shooter'),with=F])[order(-max_shooter)]
# teamData[, max_passer:=max(pass), by='team']
# unique(teamData[,c('team','max_passer'),with=F])[order(-max_passer)]
teamData[, auc_dribbler:=sum(Dribble), by='team']
unique(teamData[,c('team','auc_dribbler'),with=F])[order(-auc_dribbler)]
teamData[, auc_shooter:=sum(Shot), by='team']
unique(teamData[,c('team','auc_shooter'),with=F])[order(-auc_shooter)]
teamData[, auc_passer:=sum(Pass), by='team']
unique(teamData[,c('team','auc_passer'),with=F])[order(-auc_passer)]
probs = unique(teamData[,c('team','auc_shooter', 'auc_passer', 'auc_dribbler'),with=F])
# ------------------------------------------------------------


# ------------------------------------------------------------
# Graph

# colors 
teamColors = c('Atlanta United'='#9D2235', 'Chicago'='#102141', 'Colorado'='#862633', 'Columbus'='#FFF200', 'DC United'='#000000', 'FC Dallas'='#BF0D3E', 'Houston'='#F68712', 'L.A. Galaxy'='#00245D', 'Montreal'='#00529B', 'New England'='#C63323', 'New York City FC'='#69ACE5', 'New York'='#ED1E36', 'Orlando City'='#612B9B', 'Philadelphia'='#B1872D', 'Portland'='#004812', 'Salt Lake'='#B30838', 'San Jose'='#0D4C92', 'Seattle'='#5D9732', 'Kansas City'='#93B1D7', 'Toronto'='#E31937', 'Vancouver'='#00245E','Minnesota United'='#9CCEE7')

# conferences
west = c('Colorado', 'FC Dallas', 'Kansas City', 'L.A. Galaxy', 'San Jose', 'Salt Lake', 'Houston', 'Seattle', 'Portland', 'Vancouver', 'Minnesota United')
teamDataLong[team %in% west, conference:='West']
teamDataLong[!team %in% west, conference:='East']

# each probability by team
if (emphasizeSeattle) exampleTeams = c('Seattle', 'Houston', 'Colorado')
if (!emphasizeSeattle) exampleTeams = c('Philadelphia', 'Houston', 'Chicago')
pfacet = ggplot(teamDataLong[team %in% exampleTeams & xg<.75], aes(y=value, x=xg, color=variable)) + 
	geom_line(size=1.5) + 
	scale_color_manual('', values=brewer.pal(3,'Paired')) + 
	facet_wrap(~team, ncol=3) + 
	labs(title='Probability of Offensive Action\nby Type, Field Location and Team', 
		y='Probability of Action', x='Field Location (Pseudo-Expected Goals)', 
		subtitle='Selected Teams') + 
	theme_bw() + 
	theme(plot.title=element_text(hjust=.5), axis.title.y=element_text(size=14), 
		axis.title.x=element_text(size=14), plot.subtitle=element_text(hjust=.5)) 
	
# each team by action
pwest = ggplot(teamDataLong[conference=='West'], aes(y=value, x=xg, color=team)) + 
	geom_line(alpha=.75, size=1) + 
	scale_color_manual('', values=teamColors) + 
	facet_wrap(~variable, scales='free') + 
	labs(title='Probability of Offensive Action\nby Type, Field Location and Team', 
		y='Probability of Action', x='Field Location (Pseudo-Expected Goals)', 
		subtitle='Western Conference') + 
	theme_bw() + 
	theme(plot.title=element_text(hjust=.5), axis.title.y=element_text(size=14), 
		axis.title.x=element_text(size=14), plot.subtitle=element_text(hjust=.5)) 
if(emphasizeSeattle) pwest = pwest + geom_line(data=teamDataLong[team=='Seattle'], size=2)
		
peast = ggplot(teamDataLong[conference=='East'], aes(y=value, x=xg, color=team)) + 
	geom_line(alpha=.75, size=1) + 
	scale_color_manual('', values=teamColors) + 
	facet_wrap(~variable, scales='free') + 
	labs(title='Probability of Offensive Action\nby Type, Field Location and Team', 
		y='Probability of Action', x='Field Location (Pseudo-Expected Goals)', 
		subtitle='Eastern Conference') + 
	theme_bw() + 
	theme(plot.title=element_text(hjust=.5), axis.title.y=element_text(size=14), 
		axis.title.x=element_text(size=14), plot.subtitle=element_text(hjust=.5)) 
if(emphasizeSeattle) peast = peast + geom_line(data=teamDataLong[team=='Seattle'], size=2)

# ------------------------------------------------------------


# -------------------------------
# Save
pdf(outFile, height=4, width=9)
pfacet
pwest
peast
dev.off()
# -------------------------------
