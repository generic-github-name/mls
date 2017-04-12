# Winningest teams
# idea: visualize each team's all-time mean points and winning percentage. show how those metrics have evolved (cumulatively) over time

# -------------------------
# set up R
rm(list=ls())
library(data.table)
library(ggplot2)
# -------------------------


# --------------------------------------------------------
# input/output files
inFile = './webscrape/all_matches_in_mls_website.csv'
outFile = './historical_trends/graphs.pdf'
source('./home_field_advantage/formalize_team_names.r')
# --------------------------------------------------------


# ----------------------------------------------------------------------------------
# load data
data = fread(inFile)

# drop future games
data = data[!is.na(winner)]

# drop internationals, friendlies and preseason games
comps = 'MLS Regular Season'
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
# ----------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------
# Reshape data (is there a better way?)
i=1
teams = unique(data[competition=='MLS Regular Season' & year(date)<2017]$team_home)
vars = c('winner', 'team_home', 'team_away', 'home_points', 'away_points', 'date')
for(t in teams) {
	tmp = data[team_away==t | team_home==t, vars, with=FALSE]
	tmp[team_home==t, points:=home_points]
	tmp[team_away==t, points:=away_points]
	tmp[, team:=t]
	tmp = tmp[, c('team','points','winner','date'),with=FALSE]
	if (i==1) analysisData = tmp
	if (i>1) analysisData = rbind(analysisData, tmp)
	i=i+1
}
# ----------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------
# Analyze

# identify conference
west = c('colorado', 'fc-dallas', 'sporting-kc', 'la-galaxy', 'san-jose', 'real-salt-lake', 'chivas-usa', 'houston', 'seattle', 'portland', 'vancouver')
analysisData[team %in% west, conference:='West']
analysisData[!team %in% west, conference:='East']

# prep analysis data
analysisData = formalizeTeamNames(analysisData)
analysisData[, win:=winner==team]
analysisData[, list(win_pct=mean(win)), by='team'][order(-win_pct)]
analysisData[, list(mean_points=mean(points)), by='team'][order(-mean_points)]
analysisData[, game_number:=seq_along(win), by='team']

# make cumulative means
cummean = function(x) cumsum(x) / seq_along(x)
analysisData[, cumulative_mean_pct:=cummean(win), by='team']
analysisData[, cumulative_mean_points:=cummean(points), by='team']

# look at draws
t = table(analysisData[year(date)>=2000,c('team','wld'),with=F]))
t = dcast.data.table(t, team~wld)
t[, total:=draw+loss+win]
t[, draw:=draw/total]
t[, loss:=loss/total]
t[, win:=win/total]
t[order(draw)]

# reference team
glmData = copy(analysisData)
glmData[team=='Seattle Sounders', team:='1. Seattle Sounders']

# # regression
# glmFitWins = glm(win ~ team, data=glmData, family='binomial')
# glmFitPoints = lm(points ~ team, data=glmData)

# # extract estimates
# winEstimates = exp(cbind(coef(glmFitWins), confint(glmFitWins)))
# pointsEstimates = cbind(coef(glmFitPoints), confint(glmFitPoints))
# winEstimates
# pointsEstimates
# ----------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------
# Time series plots
teamColors = c('Atlanta'='#9D2235', 'Chicago'='#102141', 'Colorado'='#862633', 'Columbus'='#FFF200', 'DC United'='#000000', 'FC Dallas'='#BF0D3E', 'Houston'='#F68712', 'LA Galaxy'='#00245D', 'Montreal'='#00529B', 'New England'='#C63323', 'NYCFC'='#69ACE5', 'NY Red Bulls'='#ED1E36', 'Orlando'='#612B9B', 'Philadelphia'='#B1872D', 'Portland'='#004812', 'Real Salt Lake'='#B30838', 'San Jose'='#0D4C92', 'Seattle Sounders'='#5D9732', 'Sporting KC'='#93B1D7', 'Toronto FC'='#E31937', 'Vancouver'='#00245E')
otherTeams = teams[!teams %in% names(teamColors)]
otherTeamColors = rep('black', length(otherTeams))
otherTeamColors = setNames(otherTeamColors, otherTeams)
teamColors = c(teamColors, otherTeamColors)

minGames = 25
teamLabels = analysisData[game_number==minGames+10]
teamLabels[team=='Columbus', cumulative_mean_points:=cumulative_mean_points-.2]
teamLabels[team=='DC United', cumulative_mean_points:=cumulative_mean_points+.2]
teamLabels[team=='NY Red Bulls', cumulative_mean_points:=cumulative_mean_points]
teamLabels[team=='New England', cumulative_mean_points:=cumulative_mean_points+.1]
teamLabels = teamLabels[team %in% names(teamColors)]

# graph west
p1 = ggplot(analysisData[game_number>minGames & conference=='West'], aes(y=cumulative_mean_points, x=date, color=team, group=team)) + 
	geom_line(size=1.2) + 
	geom_text(data=teamLabels[conference=='West'], aes(label=team, y=cumulative_mean_points, x=date), size=5, color='grey20') + 
	scale_color_manual('', values=teamColors) + 
	labs(title='Best Team by Points\nWestern Conference', y='Cumulative Mean Points per Game', x='') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16), legend.position='none') 

# graph east
p2 = ggplot(analysisData[game_number>minGames & (conference=='East' | team=='Seattle Sounders')], aes(y=cumulative_mean_points, x=date, color=team, group=team)) + 
	geom_line(size=1.2) + 
	geom_text(data=teamLabels[conference=='East' | team=='Seattle Sounders'], aes(label=team, y=cumulative_mean_points, x=date), size=5, color='grey20') + 
	scale_color_manual('', values=teamColors) + 
	labs(title='Best Team by Points\nEastern Conference (plus Seattle)', y='Cumulative Mean Points per Game', x='') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16), legend.position='none') 

# graph best 6
best = c('Chicago', 'DC United', 'Seattle Sounders', 'LA Galaxy', 'Houston')
p3 = ggplot(analysisData[game_number>minGames & team %in% best], aes(y=cumulative_mean_points, x=date, color=team, group=team)) + 
	geom_line(size=1.2) + 
	geom_text(data=teamLabels[team %in% best], aes(label=team, y=cumulative_mean_points, x=date), size=5, color='grey20') + 
	scale_color_manual('', values=teamColors) + 
	labs(title='Best Team by Points\nBest 5 Teams', y='Cumulative Mean Points per Game', x='') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16), legend.position='none') 
	
# graph over game number instead of date
sfcGames = max(analysisData[team=='Seattle Sounders']$game_number)

# graph west
p4 = ggplot(analysisData[game_number>minGames & game_number<=sfcGames & conference=='West'], aes(y=cumulative_mean_points, x=game_number, color=team, group=team)) + 
	geom_line(size=1.2) + 
	geom_text(data=teamLabels[conference=='West'], aes(label=team, y=cumulative_mean_points, x=game_number), size=5, color='grey20') + 
	scale_color_manual('', values=teamColors) + 
	labs(title=paste('Best Team by Points in', sfcGames, 'Games \nWestern Conference'), y='Cumulative Mean Points per Game', x='Game Number') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16), legend.position='none') 

# graph east
p5 = ggplot(analysisData[game_number>minGames & game_number<=sfcGames & (conference=='East' | team=='Seattle Sounders')], aes(y=cumulative_mean_points, x=game_number, color=team, group=team)) + 
	geom_line(size=1.2) + 
	geom_text(data=teamLabels[conference=='East' | team=='Seattle Sounders'], aes(label=team, y=cumulative_mean_points, x=game_number), size=5, color='grey20') + 
	scale_color_manual('', values=teamColors) + 
	labs(title=paste('Best Team by Points in', sfcGames, 'Games \nEastern Conference (plus Seattle)'), y='Cumulative Mean Points per Game', x='Game Number') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16), legend.position='none') 

pdf(outFile, height=6, width=10)
p1
p2
p3
p4
p5
dev.off()
# ----------------------------------------------------------------------------------
