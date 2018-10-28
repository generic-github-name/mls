# Idea: Make a graph of passing % vs defensive success rate over the season. Show where each player falls on a cartesian coordinate grid where the axes are the means so quadrant 1 is above average on both dimesions, quadrant 2 is below average on passing but above average on defense, quadrant 3 is below average on both, quadrant 4 is above average on defense but below average on passing.
# Maybe incorporate the ASA idea of passing chains xG as one of the dimensions? 


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(RColorBrewer)
library(ggplot2)
# --------------------


# ---------------------------------------------------------------
# Files and directories

# input files
inFilePasses2015 = './webscrape/ASA/2015/raw passes.csv'
inFileDefense2015 = './webscrape/ASA/2015/raw defensive actions.csv'
inFilePasses2016 = './webscrape/ASA/2016/raw passes.csv'
inFileDefense2016 = './webscrape/ASA/2016/raw defensive actions.csv'
inFilePasses2017 = './webscrape/ASA/2017/raw passes.csv'
inFileDefense2017 = './webscrape/ASA/2017/raw defensive actions.csv'
inFilePasses2018 = './webscrape/ASA/2018/raw passes.csv'
inFileDefense2018 = './webscrape/ASA/2018/raw defensive actions.csv'

# output files
graphFile = './player_performance/player_percentages.pdf'
# ---------------------------------------------------------------


# ----------------------------------------------
# Load/prep data

# load
passes2015 = fread(inFilePasses2015)
defense2015 = fread(inFileDefense2015)
passes2016 = fread(inFilePasses2016)
defense2016 = fread(inFileDefense2016)
passes2017 = fread(inFilePasses2017)
defense2017 = fread(inFileDefense2017)
passes2018 = fread(inFilePasses2018)
defense2018 = fread(inFileDefense2018)

# identify years
passes2015[, year:=2015]
defense2015[, year:=2015]
passes2016[, year:=2016]
defense2016[, year:=2016]
passes2017[, year:=2017]
defense2017[, year:=2017]
passes2018[, year:=2018]
defense2018[, year:=2018]

# identify actions
passes2015[, action:='pass']
defense2015[, action:='defense']
passes2016[, action:='pass']
defense2016[, action:='defense']
passes2017[, action:='pass']
defense2017[, action:='defense']
passes2018[, action:='pass']
defense2018[, action:='defense']

# rename
setnames(passes2015, c('success','passer'), c('outcome','player'))
setnames(passes2016, c('success','passer'), c('outcome','player'))
setnames(passes2017, c('success','passer'), c('outcome','player'))
setnames(passes2018, c('success','passer'), c('outcome','player'))

# append
data = rbind(passes2015, passes2016, passes2017, passes2018, 
			defense2015, defense2016, defense2017, defense2018, fill=TRUE)
# ----------------------------------------------


# ---------------------------------
# Collapse

# player-level 
playerAgg = data[, list(pct=mean(outcome), N=.N), by=c('player','action','year','team')]

# reshape
playerAgg = dcast(playerAgg, player+team+year~action, value.var=c('pct','N'))

# compute distance from 90-90
playerAgg[, dist1:=ifelse(pct_pass<.9, .9-pct_pass, 0)]
playerAgg[, dist2:=ifelse(pct_defense<.9, .9-pct_defense, 0)]
playerAgg[, distance:=sqrt((dist1^2) + (dist2^2))]

# drop infrequent players
playerAgg = playerAgg[N_pass>50 & N_defense>10]

# drop infrequent players with stricter criteria
playerAggStrict = playerAgg[N_pass>250 & N_defense>50]

# display rankings
playerAgg[order(distance)][1:10]
playerAggStrict[order(distance)][1:10]
playerAggStrict[year==2018, overall_rank_2018:=rank(distance)]
playerAggStrict[year==2018, passing_rank_2018:=rank(-pct_pass)]
playerAggStrict[year==2018, defense_rank_2018:=rank(-pct_defense)]
playerAggStrict[year==2018][order(distance)][1:10]
playerAggStrict[year==2018][order(-pct_pass)][1:10]
playerAggStrict[year==2018][order(-pct_defense)][1:10]

# look at rankings among "possession-based" teams
teams = c('Kansas City', 'Atlanta United', 'New York City FC', 'New York', 'Seattle')
playerAggStrict[, rank:=rank(distance)]
playerAggStrict[team %in% teams][year==2018][order(-pct_defense)][1:10]
# ---------------------------------


# ----------------------------------------------
# Graph

# colors
cols = brewer.pal(6, 'Paired')

# graph 2018 sounders
meanya = mean(playerAgg[team=='Seattle' & year==2018]$pct_pass)
meanxa = mean(playerAgg[team=='Seattle' & year==2018]$pct_defense)
texty1a=.865
texty2a=.715
textx1a=.85
textx2a=.65
ps2018 = ggplot(playerAgg[team=='Seattle' & year==2018], aes(y=pct_pass, x=pct_defense, size=N_pass)) + 
	annotate('text', y=meanya, x=-Inf, label='Mean Passing Percentage', vjust=1, hjust=0, size=2.5) + 
	annotate('text', y=-Inf, x=meanxa, label='Mean Defensive Percentage', vjust=1, hjust=0, size=2.5, angle=90) + 
	annotate('text', y=texty1a, x=textx1a, label='Above Average\nIn Both', size=4, alpha=.35) + 
	annotate('text', y=texty2a, x=textx2a, label='Below Average\nIn Both', size=4, alpha=.35) + 
	annotate('text', y=texty1a, x=textx2a, label='Above Average Passing\nBelow Average Defense', size=4, alpha=.35) + 
	annotate('text', y=texty2a, x=textx1a, label='Above Average Defense\nBelow Average Passing', size=4, alpha=.35) + 
	geom_point(alpha=.5, color='#4f953b') + 
	geom_hline(aes(yintercept=meanya)) + 
	geom_vline(aes(xintercept=meanxa)) + 
	geom_text(data=playerAgg[team=='Seattle' & year==2018 & (pct_pass>=meanya | pct_defense>=meanxa)], aes(label=player, size=NULL), size=4, vjust=1, hjust=1) + 
	geom_text(data=playerAgg[team=='Seattle' & year==2018 & pct_pass<meanya & pct_defense<meanxa], aes(label=player, size=NULL), size=4, vjust=0, hjust=0) + 
	labs(title='2018 Sounders', y='Passing Percentage', x='Defensive Action Success Percentage', size='Total Number\nof Passes') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.title.x=element_text(size=14), plot.title=element_text(hjust=.5, size=16))

# best 10 players in last 3 years
meanyb = mean(playerAggStrict[distance<.06]$pct_pass)
meanxb = mean(playerAggStrict[distance<.06]$pct_defense)
texty1b=.89
texty2b=.85
textx1b=.91
textx2b=.865
ptop10 = ggplot(playerAggStrict[distance<.06], aes(y=pct_pass, x=pct_defense, size=N_pass)) + 
	annotate('text', y=meanyb, x=-Inf, label='Mean Passing Percentage', vjust=1, hjust=0, size=2.5) + 
	annotate('text', y=-Inf, x=meanxb, label='Mean Defensive Percentage', vjust=1, hjust=0, size=2.5, angle=90) + 
	annotate('text', y=texty1b, x=textx1b, label='Above Average\nIn Both', size=4, alpha=.35) + 
	annotate('text', y=texty2b, x=textx2b, label='Below Average\nIn Both', size=4, alpha=.35) + 
	annotate('text', y=texty1b, x=textx2b, label='Above Average Passing\nBelow Average Defense', size=4, alpha=.35) + 
	geom_point(alpha=.5, color='#bd1550') + 
	geom_hline(aes(yintercept=meanyb)) + 
	geom_vline(aes(xintercept=meanxb)) + 
	geom_text(data=playerAggStrict[distance<.06 & pct_defense>=meanxb], aes(label=paste(player, year), size=NULL), size=4, vjust=1, hjust=1) + 
	geom_text(data=playerAggStrict[distance<.06 & pct_defense<meanxb], aes(label=paste(player, year), size=NULL), size=4, vjust=0, hjust=0) + 
	labs(title='Top Players in Last 3 Years', y='Passing Percentage', x='Defensive Action Success Percentage', size='Total Number\nof Passes', caption='Among players with >250 total passes and >50 total defensive actions') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.title.x=element_text(size=14), plot.title=element_text(hjust=.5, size=16), plot.caption=element_text(size=7))

# best 10 players in 2018
meanyc = mean(playerAgg[distance<.09 & year==2018]$pct_pass)
meanxc = mean(playerAgg[distance<.09 & year==2018]$pct_defense)
texty1c=.89
texty2c=.821
textx1c=.905
textx2c=.845
ptop10_2018 = ggplot(playerAgg[distance<.09 & year==2018], aes(y=pct_pass, x=pct_defense, size=N_pass)) + 
	annotate('text', y=meanyc, x=-Inf, label='Mean Passing Percentage', vjust=1, hjust=0, size=2.5) + 
	annotate('text', y=-Inf, x=meanxc, label='Mean Defensive Percentage', vjust=1, hjust=0, size=2.5, angle=90) + 
	annotate('text', y=texty1c, x=textx1c, label='Above Average\nIn Both', size=4, alpha=.35) + 
	annotate('text', y=texty2c, x=textx2c, label='Below Average\nIn Both', size=4, alpha=.35) + 
	annotate('text', y=texty1c, x=textx2c, label='Above Average Defense\nBelow Average Passing', size=4, alpha=.35) + 
	annotate('text', y=texty2c, x=textx1c, label='Above Average Defense\nBelow Average Passing', size=4, alpha=.35) + 
	geom_point(alpha=.5, color='#005f6b') + 
	geom_hline(aes(yintercept=meanyc)) + 
	geom_vline(aes(xintercept=meanxc)) + 
	geom_text(data=playerAgg[distance<.09 & pct_defense>=meanxc & year==2018], aes(label=player, size=NULL), size=4, vjust=1, hjust=1) + 
	geom_text(data=playerAgg[distance<.09 & pct_defense<meanxc & year==2018], aes(label=player, size=NULL), size=4, vjust=0, hjust=0) + 
	labs(title='Top Players in 2018', y='Passing Percentage', x='Defensive Action Success Percentage', size='Total Number\nof Passes', caption='Among players with >50 total passes and >10 total defensive actions') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.title.x=element_text(size=14), plot.title=element_text(hjust=.5, size=16), plot.caption=element_text(size=8))
# ----------------------------------------------


# --------------------------------
# Save graphs
pdf(graphFile, height=6, width=9)
ps2018
ptop10
ptop10_2018
dev.off()
# --------------------------------
