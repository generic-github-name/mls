# Scrape mlssoccer.com for absence/injury data in the "match preview"
# output files saved in the current working directory

# to do
# Fix the part the splits injury types from names to grab from inside parens rather than split in ")," because this fails to split the list of players on international duty https://stackoverflow.com/questions/8613237/extract-info-inside-all-parenthesis-in-r

# ------------------------------
# Set up R
rm(list=ls())
library(data.table)
library(stringr)
library(rvest)
library(httr) # for "use_proxy"
# ------------------------------


# -----------------------------------------------------------------------------
# Files and Settings

# input file: all matches in mls website
inFile = './webscrape/all_matches_in_mls_website.csv'
asaFile2017 = './webscrape/ASA/2017/Game Information.csv'
asaFile2018 = './webscrape/ASA/2018/Game Information.csv'

# files with players' names
lineupFile2015 = './webscrape/ASA/2018/Starting Lineups.csv'
lineupFile2016 = './webscrape/ASA/2018/Starting Lineups.csv'
lineupFile2017 = './webscrape/ASA/2018/Starting Lineups.csv'
lineupFile2018 = './webscrape/ASA/2018/Starting Lineups.csv'

# output file
outFile = './webscrape/absences_for_every_match.csv'

# whether to re-scrape everything or just update all clubs in the current year
reScrapeAll = FALSE

# store today's date
today = Sys.Date()

# name conversions
source('./_common/convert_to_url_names.r')
source('./_common/convert_from_url_names.r')
source('./_common/convert_from_asa_names.r')
source('./webscrape/preview_scraper_function.r')
# -----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# Load list of matches

# not all matches have "matchcenter" data available
# recaps begin in 2011
# matchcenter begins in 2013

# load match data to loop over
matchData = fread(inFile)

# use ASA data for 2017 and 2018
matchData = matchData[!str_sub(date,1,4) %in% c('2017','2018')]
asa2017 = fread(asaFile2017)
asa2018 = fread(asaFile2018)
asa = rbind(asa2017, asa2018)
asa[, date:=as.character(as.Date(date, '%m/%d/%Y'))]
asa = asa[, c('ateam','hteam','afinal','hfinal','date')]
setnames(asa, c('team_away','team_home','score_away','score_home','date'))
asa = convertTeamNamesASA(asa)
matchData = rbind(matchData, asa,fill=TRUE)

# id games
matchData[, id:=seq_len(.N)]
# ----------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
# Loop over matches, scrape the lineups and save individually

# loop over matches post-2013
ids = matchData[year(date)>=2013]$id
I = length(ids)
prev_pct = 0
for(cid in rev(ids)) {

	# skip pre season
	if (grepl('Preseason', matchData[id==cid]$competition)) next
	if (grepl('CONCACAF', matchData[id==cid]$competition)) next

	# extract match info
	th = matchData[id==cid]$team_home
	ta = matchData[id==cid]$team_away
	date = matchData[id==cid]$date

	# convert team names
	th = convertTeamNames(th)
	ta = convertTeamNames(ta)
	
	# skip non-MLS games based on whether the convertTeamNames function can find it
	if (is.na(th) | is.na(ta)) next
	
	# only scrape new match-lineups if specified (always scrape this year)
	prOutFile = paste0('./webscrape/match_previews/', th, '_', ta, '_', date, '.csv')
	if (!reScrapeAll & file.exists(prOutFile)) next
	
	# scrape
	currMatch = previewScraper(th, ta, date)
	
	# save current match
	write.csv(currMatch, prOutFile, row.names=FALSE)
	
	# report to user
	i = which(cid==ids)
	pct_done = floor(i/I*100)
	if (pct_done!=prev_pct) cat(paste0(pct_done, '% '))
	prev_pct = pct_done
}
# ---------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------
# Append all files

# loop over club-years and append
i=1
for(cid in ids) {
	# extract match info
	th = matchData[id==cid]$team_home
	ta = matchData[id==cid]$team_away
	date = matchData[id==cid]$date

	# convert team names
	th = convertTeamNames(th)
	ta = convertTeamNames(ta)
	inFile = paste0('./webscrape/match_previews/', th, '_', ta, '_', date, '.csv')
	if (!file.exists(inFile)) next
	matches = fread(inFile)
	if (i==1) all_matches = matches
	if (i>1) all_matches = rbind(all_matches, matches, fill=TRUE)
	i=i+1
}
# ---------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------
# Clean up

# drop duplicates
all_matches = unique(all_matches)
all_matches = all_matches[!is.na(hteam) & !is.na(ateam)]
all_matches$V1 = NULL

# order by date/home team
all_matches = all_matches[order(hteam, ateam)]

# convert names back
for(t in unique(all_matches$hteam)) {
	for(v in c('hteam','ateam')) {
		st = convertTeamNamesRev(t)
		if(is.na(t) | is.na(st)) next
		all_matches[get(v)==t, (v):=st]
	}
}

# screen out any errors based on length of player name
lineups2015 = fread(lineupFile2015, fill=TRUE)
lineups2016 = fread(lineupFile2016, fill=TRUE)
lineups2017 = fread(lineupFile2017, fill=TRUE)
lineups2018 = fread(lineupFile2018, fill=TRUE)
lineups = rbind(lineups2018, lineups2017)
lineups = rbind(lineups, lineups2016)
lineups = rbind(lineups, lineups2015)
lineups = melt(lineups, id.vars=c('gameID','team','home','formation'))
lineups[, length:=nchar(value)]
maxLength = max(lineups$length)+5
all_matches = all_matches[nchar(player)<=maxLength]
# ---------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------
# Save

# archive old data
archiveFile = gsub('.csv', paste0(today, '.csv'), outFile)
archiveFile = gsub('webscrape', 'webscrape/archive', archiveFile)
file.copy(from=outFile, to=archiveFile, overwrite=TRUE)

# save data
write.csv(all_matches, outFile, row.names=FALSE)
# ---------------------------------------------------------------------------------
