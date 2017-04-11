# Explore fouls, yellows, reds and pks conceded by team


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
# --------------------


# -----------------------------------------------------
# Files/directories/lists

# input file
inFile = './webscrape/boxscores_for_every_match.csv'
matchFile = './webscrape/all_matches_in_mls_website.csv'

# output file
outFile = './fouls/dirtiest_teams.pdf'

# load functions
source('./_common/formalize_team_names.r')
# -----------------------------------------------------


# -------------------------------------------------------------------------
# Load/prep data

# load
data = fread(inFile)

# bring in match data
matchData = fread(matchFile)

# keep MLS competitions and current teams
comps = c('MLS Regular Season', 'MLS Playoffs', 'MLS Cup')
data = merge(data, matchData, by=c('team_home','team_away','date'))
data = data[competition %in% comps]
teams = unique(data[year(date)==2016]$team_home)
data = data[team_home %in% teams & team_away %in% teams]

# separate pks to identify the teams who committed them
pks = data[, c('team_home','team_away','team','pk_1','pk_2','pk_3','pk_4'), with=FALSE]
pks[, team:=ifelse(team==team_home, team_away, team_home)]
vars = c('pk_1', 'pk_2', 'pk_3', 'pk_4')
for(v in vars) pks[, (v):=(!is.na(get(v)) & get(v)!='')]
pks[, pks:=rowSums(.SD, na.rm=TRUE), .SDcols=vars]
pks = pks[, list(pks=sum(pks)), by='team']

# collapse to total fouls and cards
freqs = data[, list(fouls=sum(fouls), fouled=sum(fouled), yellow=sum(!is.na(yellow) & yellow!=''), 
	secondyellow=sum(!is.na(secondyellow) & secondyellow!=''), straightred=sum(!is.na(straightred) & straightred!=''), 
	games=length(unique(date))), by='team']

# add pks
freqs = merge(freqs, pks, by='team')

# compute rates
for(v in c('fouls','fouled','yellow','secondyellow','straightred','pks')) {
	freqs[, (paste0(v, '_rate')):=get(v)/games]
}

# dirtiest player?
players = data[, list(fouls=sum(fouls), fouled=sum(fouled),games=length(unique(date)),minutes=sum(minutes)), by='player']

# compute rates
players[, fouls_rate:=fouls/minutes*90]
players[, fouled_rate:=fouled/minutes*90]
players[, fouls_rateg:=fouls/games]
players[, fouled_rateg:=fouled/games]
players[minutes>500][order(-fouls_rate)]
players[games>10][order(-fouls_rateg)]
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Set up to graph

graphData = copy(freqs)
graphData = formalizeTeamNames(graphData)
graphData[, straightred_rate:=straightred_rate*10]
graphData[, secondyellow_rate:=secondyellow_rate*10]
graphData[, pks_rate:=pks_rate*10]
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Graph

# loop over variables
vars = c('straightred_rate', 'secondyellow_rate', 'pks_rate', 'yellow_rate', 'fouls_rate', 'fouled_rate')
labels = c('Straight Reds Per 10 Games', 'Second Yellows Per 10 Games', 'PKs Conceded Per 10 Games', 'Yellows Per Game', 'Fouls Per Game', 'Fouls Suffered Per Game')
plots = list()
teamColors = c('Atlanta United'='#9D2235', 'Chicago'='#102141', 'Colorado'='#862633', 'Columbus'='#FFF200', 'DC United'='#000000', 'FC Dallas'='#BF0D3E', 'Houston'='#F68712', 'LA Galaxy'='#00245D', 'Montreal'='#00529B', 'New England'='#C63323', 'NYCFC'='#69ACE5', 'NY Red Bulls'='#ED1E36', 'Orlando'='#612B9B', 'Philadelphia'='#B1872D', 'Portland'='#004812', 'Real Salt Lake'='#B30838', 'San Jose'='#0D4C92', 'Seattle Sounders'='#5D9732', 'Sporting KC'='#93B1D7', 'Toronto FC'='#E31937', 'Vancouver'='#00245E')

# can't figure out how to loop, use aes_string and reorder 
p = ggplot(graphData, aes(y=straightred_rate, x=reorder(team, -straightred_rate))) + 
	geom_bar(aes(fill=team), stat='identity', alpha=.85) + 
	scale_fill_manual('', values=teamColors) + 
	labs(title=labels[1], y='', x='') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=12), 
		axis.text.x=element_text(angle=315, hjust=0, size=9), 
		plot.title=element_text(hjust=.5, size=14), legend.position='none')
plots[[1]] = p

p = ggplot(graphData, aes(y=secondyellow_rate, x=reorder(team, -secondyellow_rate))) + 
	geom_bar(aes(fill=team), stat='identity', alpha=.85) + 
	scale_fill_manual('', values=teamColors) + 
	labs(title=labels[2], y='', x='') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=12), 
		axis.text.x=element_text(angle=315, hjust=0, size=9), 
		plot.title=element_text(hjust=.5, size=14), legend.position='none')
plots[[2]] = p

p = ggplot(graphData, aes(y=pks_rate, x=reorder(team, -pks_rate))) + 
	geom_bar(aes(fill=team), stat='identity', alpha=.85) + 
	scale_fill_manual('', values=teamColors) + 
	labs(title=labels[3], y='', x='') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=12), 
		axis.text.x=element_text(angle=315, hjust=0, size=9), 
		plot.title=element_text(hjust=.5, size=14), legend.position='none')
plots[[3]] = p

p = ggplot(graphData, aes(y=yellow_rate, x=reorder(team, -yellow_rate))) + 
	geom_bar(aes(fill=team), stat='identity', alpha=.85) + 
	scale_fill_manual('', values=teamColors) + 
	labs(title=labels[4], y='', x='') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=12), 
		axis.text.x=element_text(angle=315, hjust=0, size=9), 
		plot.title=element_text(hjust=.5, size=14), legend.position='none')
plots[[4]] = p

p = ggplot(graphData, aes(y=fouls_rate, x=reorder(team, -fouls_rate))) + 
	geom_bar(aes(fill=team), stat='identity', alpha=.85) + 
	scale_fill_manual('', values=teamColors) + 
	labs(title=labels[5], y='', x='') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=12), 
		axis.text.x=element_text(angle=315, hjust=0, size=9), 
		plot.title=element_text(hjust=.5, size=14), legend.position='none')
plots[[5]] = p

p = ggplot(graphData, aes(y=fouled_rate, x=reorder(team, -fouled_rate))) + 
	geom_bar(aes(fill=team), stat='identity', alpha=.85) + 
	scale_fill_manual('', values=teamColors) + 
	labs(title=labels[6], y='', x='') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=12), 
		axis.text.x=element_text(angle=315, hjust=0, size=9), 
		plot.title=element_text(hjust=.5, size=14), legend.position='none')
plots[[6]] = p

# assemble
p = do.call('grid.arrange', c(plots, ncol=3))
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Ranks
freqs = formalizeTeamNames(freqs)
freqs[, straightred_rank:=rank(-straightred_rate)]
freqs[, secondyellow_rank:=rank(-secondyellow_rate)]
freqs[, pks_rank:=rank(-pks_rate)]
freqs[, yellow_rank:=rank(-yellow_rate)]
freqs[, fouls_rank:=rank(-fouls_rate)]
freqs[, overall_rank:=rowSums(.SD), .SDcols=names(freqs)[grepl('_rank',names(freqs))]]
freqs[, overall_rank:=rank(overall_rank)]
freqs[, c('team',names(freqs)[grepl('_rank',names(freqs))]),with=FALSE][order(overall_rank)]
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Save
pdf(outFile, height=6, width=10)
plot(p)
dev.off()
# -------------------------------------------------------------------------
