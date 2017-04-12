# Analyze unbeaten streaks and who broke them
# output files saved in the current working directory

# set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(RColorBrewer)

# input/output files
inFile = './all_matches_in_mls_website.csv'
outFile = './streak_graphs.pdf'

# load data
data = fread(inFile)

# look at only MLS regular season that has already happened
data = data[competition=='MLS Regular Season' & !is.na(winner)]

# drop two matches with erroneous dates (found manually. there could be more!)
data = data[!which(team_away=='ny-red-bulls' & team_home=='atlanta' & date==as.Date('2017-03-11'))] # these appeared with two dates
data = data[!which(team_away=='colorado' & team_home=='ny-red-bulls' & date==as.Date('2017-03-19'))]

# identify teams
teams = unique(data$team_home)

# function to identify streaks
idStreaks = function(homeOnly=FALSE, t=NA) {
	# isolate streaks for the current team
	if (homeOnly) team_data = data[team_home==t]
	if (!homeOnly) team_data = data[team_away==t | team_home==t]
	team_data = team_data[order(date, team_home)]
	team_data[, unbeaten:=winner %in% c(t, 'draw')]
	tmp_streaks = rle(as.character(team_data$unbeaten))$lengths
	streak_idx = seq(length(tmp_streaks))
	streak_idx = rep(streak_idx, tmp_streaks)
	tmp_streaks = rep(tmp_streaks, tmp_streaks)
	team_data[, streak_length:=tmp_streaks]
	team_data[, streak_id:=streak_idx]
	team_streaks = team_data[unbeaten==TRUE | shift(unbeaten)==TRUE] # get streaks including the first loss after the streak
	team_streaks[, game_id:=seq_len(.N), by=streak_id]
	
	# collapse to just start/end/who broke it
	streak_start = team_streaks[game_id==1 & unbeaten==TRUE, c('date', 'streak_id', 'streak_length'), with=FALSE]
	setnames(streak_start, 'date', 'streak_start')
	streak_end = team_streaks[game_id==streak_length & unbeaten==TRUE, c('date', 'streak_id'), with=FALSE]
	setnames(streak_end, 'date', 'streak_end')
	streak_breaker = team_streaks[game_id==1 & unbeaten==FALSE, c('date', 'streak_id', 'winner'), with=FALSE]
	setnames(streak_breaker, c('date', 'winner'), c('streak_broken', 'streak_breaker'))
	streak_breaker[, streak_id:=streak_id-1]
	team_streaks = merge(streak_start, streak_end, 'streak_id')
	team_streaks = merge(team_streaks, streak_breaker, 'streak_id', all.x=TRUE)
	team_streaks$team = t
	
	# return
	return(team_streaks)
}

# identify unbeaten streaks
streaks = NULL
for(t in teams) {
	
	# run function
	team_streaks = idStreaks(homeOnly=FALSE, t=t)
	
	# add the current team's streaks to the list
	streaks = rbind(streaks, team_streaks)
}

# identify conference
west = c('colorado', 'fc-dallas', 'sporting-kc', 'la-galaxy', 'san-jose', 'real-salt-lake', 'chivas-usa', 'houston', 'seattle', 'portland', 'vancouver')
streaks[team %in% west, conference:='West']
streaks[!team %in% west, conference:='East']

# relabel streak ID's
streaks[, streak_id:=as.numeric(as.factor(paste0(team, streak_id)))]

# reduce to streaks of K or more
K = 9
streaks = streaks[streak_length>=K]

# identify home unbeaten streaks
homeStreaks = NULL
for(t in teams) {
	
	# run function
	team_streaks = idStreaks(homeOnly=TRUE, t=t)
	
	# add the current team's streaks to the list
	homeStreaks = rbind(homeStreaks, team_streaks)
}

# identify conference
homeStreaks[team %in% west, conference:='West']
homeStreaks[!team %in% west, conference:='East']

# relabel streak ID's
homeStreaks[, streak_id:=as.numeric(as.factor(paste0(team, streak_id)))]

# reduce to streaks of K or more
Khome = 9
homeStreaks = homeStreaks[streak_length>=Khome]

# who breaks streaks the most?
t1 = rev(sort(table(streaks$streak_breaker)))
t1

# who breaks streaks the most?
t1home = rev(sort(table(homeStreaks$streak_breaker)))
t1home

# who breaks streaks the most since 2007?
t1b = rev(sort(table(streaks[year(streak_broken)>=2007]$streak_breaker)))
t1b

# who breaks streaks the most since 2007?
t1homeb = rev(sort(table(homeStreaks[year(streak_broken)>=2007]$streak_breaker)))
t1homeb

# who has nemesis teams?
t2 = table(streaks[, c('team', 'streak_breaker'), with=FALSE])
t2 = data.table(melt(t2))
t2 = t2[order(-value, streak_breaker, team)]
t2

# streaks broken by seattle
seattle = rbind(streaks[streak_breaker=='seattle'], homeStreaks[streak_breaker=='seattle'])
seattle = merge(seattle, data[team_home=='seattle' | team_away=='seattle', c('score_away', 'score_home', 'date', 'team_home'), with=FALSE], by.x='streak_broken', by.y='date', all.x=TRUE)

# set up to graph
graphData = melt(streaks, id.vars=c('team', 'streak_breaker', 'streak_length', 'streak_broken', 'streak_id', 'conference'))
graphData[, value:=as.Date(value)]
labelData = graphData[, mean(as.Date(value)), by=c('team','streak_id', 'streak_breaker', 'streak_length', 'conference')]
labelData[, label1:=paste0('Length:', streak_length)]
labelData[, label2:=paste0('Broken by:\n', streak_breaker)]
lastTeamE = rev(unique(graphData[conference=='East']$team)[order(unique(graphData[conference=='East']$team))])[1]
lastTeamW = unique(graphData[conference=='West']$team)[order(unique(graphData[conference=='West']$team))][1]
legendDataE = graphData[team==lastTeamE][1]
legendDataE[, value:=max(graphData[conference=='East']$value)-320]
legendDataW = graphData[team==lastTeamW][1]
legendDataW[, value:=min(graphData[conference=='West']$value)+365]
legendDataEHome=copy(legendDataE)
legendDataWHome=copy(legendDataW)
legendDataEHome[, value:=max(graphData[conference=='East']$value)-330]
legendDataWHome[, value:=min(graphData[conference=='West']$value)+800]

# set up to graph home streaks
graphDataHome = melt(homeStreaks, id.vars=c('team', 'streak_breaker', 'streak_length', 'streak_broken', 'streak_id', 'conference'))
graphDataHome[, value:=as.Date(value)]
labelDataHome = graphDataHome[, mean(as.Date(value)), by=c('team','streak_id', 'streak_breaker', 'streak_length', 'conference')]
labelDataHome[, label1:=paste0('Length:', streak_length)]
labelDataHome[, label2:=paste0('Broken by:\n', streak_breaker)]

# graph parameters
textSize = 2
textColor = 'gray40'
teamColors = c('atlanta'='#9D2235', 'chicago'='#102141', 'colorado'='#862633', 'columbus'='#FFF200', 'd.c.-united'='#000000', 'fc-dallas'='#BF0D3E', 'houston'='#F68712', 'la-galaxy'='#00245D', 'montreal'='#00529B', 'new-england'='#C63323', 'nycfc'='#69ACE5', 'ny-red-bulls'='#ED1E36', 'orlando'='#612B9B', 'philadelphia'='#B1872D', 'portland'='#004812', 'real-salt-lake'='#B30838', 'san-jose'='#0D4C92', 'seattle'='#5D9732', 'sporting-kc'='#93B1D7', 'toronto-fc'='#E31937', 'vancouver'='#00245E')
otherTeams = teams[!teams %in% names(teamColors)]
otherTeamColors = rep('black', length(otherTeams))
otherTeamColors = setNames(otherTeamColors, otherTeams)
teamColors = c(teamColors, otherTeamColors)

# visualize overall streaks in the west
p1 = ggplot(graphData[conference=='West'], aes(x=value, y=team, group=streak_id, color=team)) + 
	geom_line(, size=2) + 
	annotate('text', x=labelData[conference=='West']$V1, y=labelData[conference=='West']$team, label=labelData[conference=='West']$streak_length, vjust=-1, size=textSize, color=textColor) + 
	annotate('text', x=labelData[conference=='West']$V1, y=labelData[conference=='West']$team, label=labelData[conference=='West']$streak_breaker, vjust=1.5, size=textSize+.5, color=textColor) + 
	geom_label(data=legendDataW, label='#: Streak Length\n team: Streak Breaker', alpha=.5, color=textColor, size=3) + 
	scale_color_manual('', values=teamColors) + 
	labs(title=paste('Streaks of', K, 'Games or More\n Western Conference'), y='', x='') + 
	theme_bw() + 
	theme(plot.title=element_text(hjust=.5), legend.position='none') 

# visualize overall streaks in the east
p2 = ggplot(graphData[conference=='East'], aes(x=value, y=team, group=streak_id, color=team)) + 
	geom_line(, size=2) + 
	annotate('text', x=labelData[conference=='East']$V1, y=labelData[conference=='East']$team, label=labelData[conference=='East']$streak_length, vjust=-1, size=textSize, color=textColor) + 
	annotate('text', x=labelData[conference=='East']$V1, y=labelData[conference=='East']$team, label=labelData[conference=='East']$streak_breaker, vjust=1.5, size=textSize+.5, color=textColor) + 
	geom_label(data=legendDataE, label='#: Streak Length\n team: Streak Breaker', alpha=.5, color=textColor, size=3) + 
	scale_color_manual('', values=teamColors) + 
	labs(title=paste('Streaks of', K, 'Games or More\nEastern Conference'), y='', x='') + 
	theme_bw() + 
	theme(plot.title=element_text(hjust=.5), legend.position='none') 

# visualize home streaks in the west
p3 = ggplot(graphDataHome[conference=='West'], aes(x=value, y=team, group=streak_id, color=team)) + 
	geom_line(, size=2) + 
	annotate('text', x=labelDataHome[conference=='West']$V1, y=labelDataHome[conference=='West']$team, label=labelDataHome[conference=='West']$streak_length, vjust=-1, size=textSize, color=textColor) + 
	annotate('text', x=labelDataHome[conference=='West']$V1, y=labelDataHome[conference=='West']$team, label=labelDataHome[conference=='West']$streak_breaker, vjust=1.5, size=textSize+.5, color=textColor) + 
	geom_label(data=legendDataWHome, label='#: Streak Length\n team: Streak Breaker', alpha=.5, color=textColor, size=3) + 
	scale_color_manual('', values=teamColors) + 
	labs(title=paste('Home Streaks of', Khome, 'Games or More\nWestern Conference'), y='', x='') + 
	theme_bw() + 
	theme(plot.title=element_text(hjust=.5), legend.position='none') 

# visualize home streaks in the east
p4 = ggplot(graphDataHome[conference=='East'], aes(x=value, y=team, group=streak_id, color=team)) + 
	geom_line(, size=2) + 
	annotate('text', x=labelDataHome[conference=='East']$V1, y=labelDataHome[conference=='East']$team, label=labelDataHome[conference=='East']$streak_length, vjust=-1, size=textSize, color=textColor) + 
	annotate('text', x=labelDataHome[conference=='East']$V1, y=labelDataHome[conference=='East']$team, label=labelDataHome[conference=='East']$streak_breaker, vjust=1.5, size=textSize+.5, color=textColor) + 
	geom_label(data=legendDataEHome, label='#: Streak Length\n team: Streak Breaker', alpha=.5, color=textColor, size=3) + 
	scale_color_manual('', values=teamColors) + 
	labs(title=paste('Home Streaks of', Khome, 'Games or More\nEastern Conference'), y='', x='') + 
	theme_bw() + 
	theme(plot.title=element_text(hjust=.5), legend.position='none') 

pdf(outFile, height=6, width=10)
print(p1)
print(p2)
print(p3)
print(p4)
dev.off()