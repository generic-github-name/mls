# Explore goal difference and other stats with each player on the field to analyze who provides the "spark"


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(stringr)
# --------------------


# -------------------------------------------------------------------------
# Files/directories/lists

# team to analyze
t = 'Seattle'

# input files
inFileDribbles = './webscrape/ASA/2017/dribbles.csv'
inFilePasses = './webscrape/ASA/2017/raw passes.csv'
inFileShots = './webscrape/ASA/2017/raw shots.csv'
inFileFoulsC = './webscrape/ASA/2017/raw fouls committed.csv'
inFileFoulsS = './webscrape/ASA/2017/raw fouls suffered.csv'
inFileDefense = './webscrape/ASA/2017/raw defensive actions.csv'
inFileLineups = './webscrape/ASA/2017/Starting Lineups.csv'
inFileMinutes = './webscrape/ASA/2017/minutes played by game.csv'
inFileGameinfo = './webscrape/ASA/2017/Game Information.csv'

# match-level file
matchFile = './webscrape/all_matches_in_mls_website.csv'

# output file
outFile1 = './player_contribution/player_contributions_current.pdf'
outFile2 = './player_contribution/player_contributions_all.pdf'

# formalization function
source('./_common/formalize_team_names.r')
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------------------
# Load/prep substitution data

# load
lineups = data.table(read.csv(inFileLineups)) # fread doesn't like this file
minutes = fread(inFileMinutes)
gameinfo = fread(inFileGameinfo)

# format various tables/variables
subs = melt(lineups, id.vars=c('gameID', 'team', 'home', 'formation'), variable.name='spot', value.name='player')
subs = subs[player!='']
subs[, gameID:=as.character(gameID)]
gameinfo[, gameID:=as.character(gameID)]
minutes[, minutes:=as.numeric(minutes)]

# figure out when each player subbed in
subs[!grepl('bench',spot), subbed_in:=0]
subs = merge(subs, gameinfo[, c('gameID','secondHalfTime','date'), with=FALSE], by='gameID')
subs[, c('minute','second') := tstrsplit(secondHalfTime, ':', fixed=TRUE)]
subs[, end:=as.numeric(minute)+(as.numeric(second)/60)]
subs = merge(subs, minutes, by=c('gameID','player'))
subs$minute = NULL
subs$second = NULL
subs[is.na(subbed_in), subbed_in:=end-minutes]

# figure out when each player subbed out 
# (complicated because I want to ensure there are no times when a player doesn't sub out after his replacement comes in,
# and I'm sure the subbed_in variable is correct)
subs[, sub_order:=rank(minutes), by=c('gameID','team')]
subs[, max:=max(minutes), by=c('gameID','team')]
subs[minutes==max, sub_order:=NA]
subs[sub_order==1 & subbed_in!=0, last_sub_time:=subbed_in]
subs[sub_order==2 & subbed_in!=0, middle_sub_time:=subbed_in] # this will be NA if there was only 1 sub
subs[sub_order==3 & subbed_in!=0, first_sub_time:=subbed_in] # this will be NA if there were only 2 subs
subs[, last_sub_time:=max(last_sub_time, na.rm=TRUE), by=c('gameID','team')]
subs[, middle_sub_time:=max(middle_sub_time, na.rm=TRUE), by=c('gameID','team')]
subs[, first_sub_time:=max(first_sub_time, na.rm=TRUE), by=c('gameID','team')]
subs[, max:=max(sub_order, na.rm=TRUE), by=c('gameID','team')]
subs[sub_order==max & subbed_in==0, subbed_out:=last_sub_time, by=c('gameID','team')]
subs[sub_order==max-1 & subbed_in==0, subbed_out:=middle_sub_time, by=c('gameID','team')]
subs[sub_order==max-2 & subbed_in==0, subbed_out:=first_sub_time, by=c('gameID','team')]
subs[is.na(subbed_out), subbed_out:=end]

# clean up a little
subs = subs[, c('gameID','date','player','team','home','formation','subbed_in','subbed_out','minutes','end'), with=FALSE]
# -------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------
# Load/prep action data

# load
dribbles = fread(inFileDribbles)
passes = fread(inFilePasses)
shots = fread(inFileShots)
foulsC = fread(inFileFoulsC)
foulsS = fread(inFileFoulsS)
defense = fread(inFileDefense)

# identify actions
dribbles[, action:='dribble']
passes[, action:='pass']
shots[, action:='shot']
foulsC[, action:='foul_committed']
foulsS[, action:='foul_suffered']
defense[, action:='defense']

# rename
setnames(shots, 'shooter', 'player')
setnames(passes, c('success','passer'), c('outcome','player'))
setnames(dribbles, 'success', 'outcome')

# append
data = rbind(dribbles, passes, shots, foulsC, foulsS, defense, fill=TRUE)

# subset variables
vars = c('date', 'time', 'player', 'team', 'team.1', 'x', 'y', 'action', 'outcome', 'result', 'keyPass')
data = data[, vars, with=FALSE]

# subset to just the selected team
data = data[team==t | team.1==t]

# format time variable
time = str_split(data$time, ':', 3)
time = data.table(do.call('rbind', time))
time[, V1:=as.numeric(gsub(' ', '', V1))]
time[, V2:=as.numeric(gsub(' ', '', V2))]
m = time$V1 + (time$V2/60)
data[, minute:=m]

# sort
data = data[order(minute)]

# create "pass into the box" variable
# later
# -------------------------------------------------------------------------------------


# --------------------------------------------------------------------------------
# Bring in sub times and aggregate various stats by player

# merge
data = merge(data, subs, c('date','team','player'))

# loop over players
i=1
for(p in unique(data[team==t]$player)) {
	data[player==p, sin:=subbed_in, by='gameID']
	data[, sin:=max(sin, na.rm=TRUE), by='gameID']
	data[player==p, sout:=subbed_out, by='gameID']
	data[, sout:=max(sout, na.rm=TRUE), by='gameID']
	data[, played:=p %in% player, by='gameID']
	tmpStats = data[played==TRUE & minute>=sin & minute<=sout, list(player=p, 
						goals_for=sum(result=='Goal' & team==t, na.rm=TRUE), 
						goals_against=sum(result=='Goal' & team!=t, na.rm=TRUE), 
						passes_completed=sum(action=='pass' & outcome==1 & team==t, na.rm=TRUE), 
						passes_missed=sum(action=='pass' & outcome==0 & team==t, na.rm=TRUE), 
						passes_against_completed=sum(action=='pass' & outcome==1 & team!=t, na.rm=TRUE), 
						passes_against_missed=sum(action=='pass' & outcome==0 & team!=t, na.rm=TRUE), 
						keypasses_for=sum(keyPass==1 & team==t, na.rm=TRUE), 
						keypasses_against=sum(keyPass==1 & team!=t, na.rm=TRUE), 
						fouls_committed=sum(action=='foul_committed' & team==t, na.rm=TRUE), 
						fouls_suffered=sum(action=='foul_suffered' & team!=t, na.rm=TRUE), 
						dribbles_completed=sum(action=='dribble' & outcome==1 & team==t, na.rm=TRUE), 
						dribbles_missed=sum(action=='dribble' & outcome==0 & team==t, na.rm=TRUE), 
						dribbles_against_completed=sum(action=='dribble' & outcome==1 & team!=t, na.rm=TRUE), 
						dribbles_against_missed=sum(action=='dribble' & outcome==0 & team!=t, na.rm=TRUE)
					)]
	if (i==1) stats = tmpStats
	if (i>1) stats = rbind(stats, tmpStats)
	i=i+1
}

# add minutes
m = unique(data[,c('gameID','player','minutes'),with=FALSE])
m = m[, list(minutes=sum(minutes,na.rm=TRUE)), by='player']
stats = merge(stats, m, by='player', all.x=TRUE)

# make summary variables
stats[, gd:=goals_for-goals_against]
stats[, gd_per90:=(goals_for/minutes*90)-(goals_against/minutes*90)]
stats[, keypassd:=keypasses_for-keypasses_against]
stats[, keypassd_per90:=(keypasses_for/minutes*90)-(keypasses_against/minutes*90)]
stats[, fould:=fouls_committed-fouls_suffered]
stats[, fould_per90:=(fouls_committed/minutes*90)-(fouls_suffered/minutes*90)]
stats[, pass_pct:=passes_completed/(passes_completed+passes_missed)]
stats[, pass_pct_against:=passes_against_completed/(passes_against_completed+passes_against_missed)]
stats[, dribble_pct:=dribbles_completed/(dribbles_completed+dribbles_missed)]
stats[, dribble_pct_against:=dribbles_against_completed/(dribbles_against_completed+dribbles_against_missed)]

# drop players with few minutes
stats = stats[minutes>750]
# --------------------------------------------------------------------------------


# --------------------------------------------------------------------------------
# Make graphs

colors = c('#AACD6E', '#F16B6F', '#C5C6B6', '#3C3530', '#ABD0CE')

gdPlot = ggplot(data=stats, aes(y=gd, x=reorder(player, -stats$gd))) + 
	geom_bar(stat='identity', fill=colors[1]) + 
	geom_text(data=stats[gd>=0], aes(label=minutes, x=player), vjust=-.3) + # labels above
	geom_text(data=stats[gd<0], aes(label=minutes, x=player), vjust=1) + # labels below
	labs(title='Goals', y='Goal Differential\nwith Player on Field', x='', caption='Bar Labels: Total Minutes Played') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16))

gd90Plot = ggplot(data=stats, aes(y=gd_per90, x=reorder(player, -stats$gd_per90))) + 
	geom_bar(stat='identity', fill=colors[1]) + 
	geom_text(data=stats[gd_per90>=0], aes(label=minutes, x=player), vjust=-.3) + # labels above
	geom_text(data=stats[gd_per90<0], aes(label=minutes, x=player), vjust=1) + # labels below
	labs(title='Goals', y='Goal Differential Rate\n(Goals per 90 - Goals Conceded per 90)\nwith Player on Field', x='', caption='Bar Labels: Total Minutes Played') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16))
	
keypassdPlot = ggplot(data=stats, aes(y=keypassd, x=reorder(player, -stats$keypassd))) + 
	geom_bar(stat='identity', fill=colors[2]) + 
	geom_text(data=stats[keypassd>=0], aes(label=minutes, x=player), vjust=-.3) + # labels above
	geom_text(data=stats[keypassd<0], aes(label=minutes, x=player), vjust=1) + # labels below
	labs(title='Key Passes', y='Key Pass Differential\nwith Player on Field', x='', caption='Bar Labels: Total Minutes Played') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16))
	
keypassd90Plot = ggplot(data=stats, aes(y=keypassd_per90, x=reorder(player, -stats$keypassd_per90))) + 
	geom_bar(stat='identity', fill=colors[2]) + 
	geom_text(data=stats[keypassd_per90>=0], aes(label=minutes, x=player), vjust=-.3) + # labels above
	geom_text(data=stats[keypassd_per90<0], aes(label=minutes, x=player), vjust=1) + # labels below
	labs(title='Key Passes', y='Key Pass Differential Rate\n(Key Pass per 90 - Key Pass Conceded per 90)\nwith Player on Field', x='', caption='Bar Labels: Total Minutes Played') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16))
	
fouldPlot = ggplot(data=stats, aes(y=fould, x=reorder(player, -stats$fould))) + 
	geom_bar(stat='identity', fill=colors[4]) + 
	geom_text(data=stats[fould>=0], aes(label=minutes, x=player), vjust=-.3) + # labels above
	geom_text(data=stats[fould<0], aes(label=minutes, x=player), vjust=1) + # labels below
	labs(title='Fouls', y='Foul Differential (Committed - Suffered)\nwith Player on Field', x='', caption='Bar Labels: Total Minutes Played') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16))
	
fould90Plot = ggplot(data=stats, aes(y=fould_per90, x=reorder(player, -stats$fould_per90))) + 
	geom_bar(stat='identity', fill=colors[4]) + 
	geom_text(data=stats[fould_per90>=0], aes(label=minutes, x=player), vjust=-.3) + # labels above
	geom_text(data=stats[fould_per90<0], aes(label=minutes, x=player), vjust=1) + # labels below
	labs(title='Fouls', y='Foul Differential Rate\n(Committed per 90 - Suffered per 90)\nwith Player on Field', x='', caption='Bar Labels: Total Minutes Played') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16))
	
passPlot = ggplot(data=stats, aes(y=pass_pct, x=reorder(player, -stats$pass_pct))) + 
	geom_bar(stat='identity', fill=colors[3]) + 
	geom_text(data=stats[pass_pct>=0], aes(label=minutes, x=player), vjust=-.3) + # labels above
	geom_text(data=stats[pass_pct<0], aes(label=minutes, x=player), vjust=1) + # labels below
	labs(title='Passes', y='Pass Completion Percentage\nwith Player on Field', x='', caption='Bar Labels: Total Minutes Played') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16))
	
dribblePlot = ggplot(data=stats, aes(y=dribble_pct, x=reorder(player, -stats$dribble_pct))) + 
	geom_bar(stat='identity', fill=colors[5]) + 
	geom_text(data=stats[dribble_pct>=0], aes(label=minutes, x=player), vjust=-.3) + # labels above
	geom_text(data=stats[dribble_pct<0], aes(label=minutes, x=player), vjust=1) + # labels below
	labs(title='Dribbles', y='Dribble Success Rate\nwith Player on Field', x='', caption='Bar Labels: Total Minutes Played') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16))
# --------------------------------------------------------------------------------


# --------------------------------------------------------------------------------
# Save separate files to have different aspect ratios
pdf(outFile1, height=6, width=9)
gdPlot
gd90Plot
keypassdPlot
keypassd90Plot
fouldPlot
fould90Plot
passPlot
dribblePlot
dev.off()
# --------------------------------------------------------------------------------
