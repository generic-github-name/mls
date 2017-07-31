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
inFileHistorical = './webscrape/all_matches_in_mls_website.csv'
inFile2017 = './webscrape/ASA/2017/Game Information.csv'
outFile = './historical_trends/graphs.pdf'
source('./_common/formalize_team_names.r')
# --------------------------------------------------------


# ----------------------------------------------------------------------------------
# load data
oldData = fread(inFileHistorical)
newData = fread(inFile2017)

# drop before 2017 from old data
oldData = oldData[year(date)<2017]

# format dates
oldData[, date:=as.Date(date)]
newData[, date:=as.Date(date, '%m/%d/%Y')]

# rename/subset new data to match old data
newNames = c('team_home','team_away','score_home','score_away')
setnames(newData, c('hteam','ateam','hfinal','afinal'), newNames)
newData = newData[, c('date', newNames), with=FALSE]
newData[, winner:=ifelse(score_home>score_away, team_home, team_away)]
newData[score_home==score_away, winner:='draw']
newData[, date:=as.Date(date, '%m/%d/%Y')]
newData[, competition:='MLS Regular Season']

# rbind
data = rbind(newData, oldData)

# drop internationals, friendlies and preseason games
comps = 'MLS Regular Season'
data = data[competition %in% comps]

# compute points earned by the home team
data[, home_points:=0]
data[winner==team_home, home_points:=3]
data[winner=='draw', home_points:=1]
data[, away_points:=0]
data[winner==team_away, away_points:=3]
data[winner=='draw', away_points:=1]

# formalize team names
data = formalizeTeamNames(data)
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
west = c('Colorado', 'FC Dallas', 'Sporting KC', 'LA Galaxy', 'San Jose', 'Real Salt Lake', 'Chivas USA', 'Houston', 'Seattle Sounders', 'Portland', 'Vancouver')
analysisData[team %in% west, conference:='West']
analysisData[!team %in% west, conference:='East']

# prep analysis data
analysisData[, win:=winner==team]
analysisData[, list(win_pct=mean(win)), by='team'][order(-win_pct)]
analysisData[, list(mean_points=mean(points)), by='team'][order(-mean_points)]
analysisData = analysisData[order(team, date)]
analysisData[, game_number:=seq_along(win), by='team']

# make cumulative means
cummean = function(x) cumsum(x) / seq_along(x)
analysisData[, cumulative_mean_pct:=cummean(win), by='team']
analysisData[, cumulative_mean_points:=cummean(points), by='team']

# # look at draws
# t = table(analysisData[year(date)>=2000,c('team','wld'),with=F])
# t = dcast.data.table(t, team~wld)
# t[, total:=draw+loss+win]
# t[, draw:=draw/total]
# t[, loss:=loss/total]
# t[, win:=win/total]
# t[order(draw)]

# # reference team
# glmData = copy(analysisData)
# glmData[team=='Seattle Sounders', team:='1. Seattle Sounders']

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

# identify team colors
teamColors = c('Atlanta'='#9D2235', 'Chicago'='#102141', 'Colorado'='#862633', 'Columbus'='#FFF200', 'DC United'='#000000', 'FC Dallas'='#BF0D3E', 'Houston'='#F68712', 'LA Galaxy'='#00245D', 'Montreal'='#00529B', 'New England'='#C63323', 'NYCFC'='#69ACE5', 'NY Red Bulls'='#ED1E36', 'Orlando'='#612B9B', 'Philadelphia'='#B1872D', 'Portland'='#004812', 'Real Salt Lake'='#B30838', 'San Jose'='#0D4C92', 'Seattle Sounders'='#5D9732', 'Sporting KC'='#93B1D7', 'Toronto FC'='#E31937', 'Vancouver'='#00245E')
otherTeams = teams[!teams %in% names(teamColors)]
otherTeamColors = rep('black', length(otherTeams))
otherTeamColors = setNames(otherTeamColors, otherTeams)
teamColors = c(teamColors, otherTeamColors)

# set a minimum games before a team shows up on the plots
minGames = 25

# manually place labels
teamLabels = analysisData[game_number==minGames+10]
teamLabels[team=='Chicago', date:=as.Date('12/12/2003','%m/%d/%Y')]
teamLabels[team=='Chicago', cumulative_mean_points:=1.775]
teamLabels[team=='Chivas USA', date:=as.Date('1/15/2006','%m/%d/%Y')]
teamLabels[team=='Chivas USA', cumulative_mean_points:=0.6]
teamLabels[team=='Colorado', date:=as.Date('6/12/1997','%m/%d/%Y')]
teamLabels[team=='Colorado', cumulative_mean_points:=.9]
teamLabels[team=='Columbus', date:=as.Date('1/13/1997','%m/%d/%Y')]
teamLabels[team=='Columbus', cumulative_mean_points:=1.15]
teamLabels[team=='DC United', date:=as.Date('3/12/1997','%m/%d/%Y')]
teamLabels[team=='DC United', cumulative_mean_points:=1.8285714]
teamLabels[team=='FC Dallas', date:=as.Date('5/13/2001','%m/%d/%Y')]
teamLabels[team=='FC Dallas', cumulative_mean_points:=1.6]
teamLabels[team=='Houston', date:=as.Date('2/21/2007','%m/%d/%Y')]
teamLabels[team=='Houston', cumulative_mean_points:=1.5]
teamLabels[team=='LA Galaxy', date:=as.Date('4/12/2002','%m/%d/%Y')]
teamLabels[team=='LA Galaxy', cumulative_mean_points:=1.85]
teamLabels[team=='Miami', date:=as.Date('4/3/2000','%m/%d/%Y')]
teamLabels[team=='Miami', cumulative_mean_points:=1.3714286]
teamLabels[team=='Montreal', date:=as.Date('3/2/2013','%m/%d/%Y')]
teamLabels[team=='Montreal', cumulative_mean_points:=1.2857143]
teamLabels[team=='NY Red Bulls', date:=as.Date('1/5/2000','%m/%d/%Y')]
teamLabels[team=='NY Red Bulls', cumulative_mean_points:=1.15]
teamLabels[team=='NYCFC', date:=as.Date('3/6/2016','%m/%d/%Y')]
teamLabels[team=='NYCFC', cumulative_mean_points:=1.1428571]
teamLabels[team=='New England', date:=as.Date('4/12/2004','%m/%d/%Y')]
teamLabels[team=='New England', cumulative_mean_points:=1.2]
teamLabels[team=='Orlando', date:=as.Date('3/6/2016','%m/%d/%Y')]
teamLabels[team=='Orlando', cumulative_mean_points:=1.2857143]
teamLabels[team=='Philadelphia', date:=as.Date('4/16/2011','%m/%d/%Y')]
teamLabels[team=='Philadelphia', cumulative_mean_points:=1.1714286]
teamLabels[team=='Portland', date:=as.Date('9/12/2012','%m/%d/%Y')]
teamLabels[team=='Portland', cumulative_mean_points:=1.25]
teamLabels[team=='Real Salt Lake', date:=as.Date('3/15/2008','%m/%d/%Y')]
teamLabels[team=='Real Salt Lake', cumulative_mean_points:=.85]
teamLabels[team=='San Jose', date:=as.Date('2/5/1997','%m/%d/%Y')]
teamLabels[team=='San Jose', cumulative_mean_points:=1.275]
teamLabels[team=='Seattle Sounders', date:=as.Date('2/1/2012','%m/%d/%Y')]
teamLabels[team=='Seattle Sounders', cumulative_mean_points:=1.8]
teamLabels[team=='Sporting KC', date:=as.Date('4/12/1997','%m/%d/%Y')]
teamLabels[team=='Sporting KC', cumulative_mean_points:=1.8]
teamLabels[team=='Tampa Bay Mutiny', date:=as.Date('4/12/2000','%m/%d/%Y')]
teamLabels[team=='Tampa Bay Mutiny', cumulative_mean_points:=1.6]
teamLabels[team=='Toronto FC', date:=as.Date('4/26/2008','%m/%d/%Y')]
teamLabels[team=='Toronto FC', cumulative_mean_points:=0.9714286]
teamLabels[team=='Vancouver', date:=as.Date('5/10/2012','%m/%d/%Y')]
teamLabels[team=='Vancouver', cumulative_mean_points:=0.8857143]
teamLabels = teamLabels[team %in% names(teamColors)]

# control order of legends
o = copy(analysisData)
o[, now:=max(date), by='team']
o = o[date==now]
o = o[order(-cumulative_mean_points)]
analysisData[, team:=factor(team, o$team)]

# graph west
p1 = ggplot(analysisData[game_number>minGames & conference=='West'], aes(y=cumulative_mean_points, x=date, color=team, group=team)) + 
	geom_line(size=1.2) + 
	geom_text(data=teamLabels[conference=='West'], aes(label=team, y=cumulative_mean_points, x=date), size=5, color='grey20') + 
	scale_color_manual('Ranking', values=teamColors) + 
	labs(title='Best Team by Points\nWestern Conference', y='Cumulative Points per Game', x='') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16)) 

# graph east
p2 = ggplot(analysisData[game_number>minGames & (conference=='East' | team=='Seattle Sounders')], aes(y=cumulative_mean_points, x=date, color=team, group=team)) + 
	geom_line(size=1.2) + 
	geom_text(data=teamLabels[conference=='East' | team=='Seattle Sounders'], aes(label=team, y=cumulative_mean_points, x=date), size=5, color='grey20') + 
	scale_color_manual('Ranking', values=teamColors) + 
	labs(title='Best Team by Points\nEastern Conference (plus Seattle)', y='Cumulative Points per Game', x='') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16)) 

# graph best 5
analysisData[, today:=max(date), by='team']
tmp = analysisData[date==today][order(-cumulative_mean_points)]
tmp[, rank:=seq(.N)]
best = tmp[rank<=5]$team
p3 = ggplot(analysisData[game_number>minGames & team %in% best], aes(y=cumulative_mean_points, x=date, color=team, group=team)) + 
	geom_line(size=1.2) + 
	geom_text(data=teamLabels[team %in% best], aes(label=team, y=cumulative_mean_points, x=date), size=5, color='grey20') + 
	scale_color_manual('Ranking', values=teamColors) + 
	labs(title='Best Team by Points\nBest 5 Teams', y='Cumulative Points per Game', x='') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16)) 
	
# graph over game number instead of date
sfcGames = max(analysisData[team=='Seattle Sounders']$game_number)

# reorder legends
o = copy(analysisData)
o[, now:=max(game_number), by='team']
o = o[(now<sfcGames & game_number==now) | game_number==sfcGames]
o = o[order(-cumulative_mean_points)]
analysisData[, team:=factor(team, o$team)]

# graph west
p4 = ggplot(analysisData[game_number>minGames & game_number<=sfcGames & conference=='West'], aes(y=cumulative_mean_points, x=game_number, color=team, group=team)) + 
	geom_line(size=1.2) + 
	scale_color_manual('Ranking', values=teamColors) + 
	labs(title=paste('Best Team by Points in First', sfcGames, 'Games \nWestern Conference'), y='Cumulative Points per Game', x='Game Number') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16)) 

# graph east
p5 = ggplot(analysisData[game_number>minGames & game_number<=sfcGames & (conference=='East' | team=='Seattle Sounders')], aes(y=cumulative_mean_points, x=game_number, color=team, group=team)) + 
	geom_line(size=1.2) + 
	scale_color_manual('Ranking', values=teamColors) + 
	labs(title=paste('Best Team by Points in First', sfcGames, 'Games \nEastern Conference (plus Seattle)'), y='Cumulative Points per Game', x='Game Number') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16)) 

pdf(outFile, height=6, width=10)
p1
p2
p3
p4
p5
dev.off()
# ----------------------------------------------------------------------------------
