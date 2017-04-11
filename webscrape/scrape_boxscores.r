# Scrape mlssoccer.com for historical w/l/d data
# output files saved in the current working directory

# to do
# reformat pks to be pk_minutes and have 3 "slots" for every player
# reformat goal_minutes to have 4 "slots" for every player
# create "subbed_in" variable for minute every player entered the game (0 for starter)
# create "subbed_out" variable for minute every player exited the game


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

# output file
outFile = './webscrape/boxscores_for_every_match.csv'

# whether to re-scrape everything or just update all clubs in the current year
reScrapeAll = FALSE

# store today's date
today = Sys.Date()

# name conversions
source('./_common/convert_to_url_names.r')
source('./_common/convert_from_url_names.r')
# -----------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# Create a function to scrape
lineupScraper = function(th, ta, date) {

	# scrape
	url = paste0('http://matchcenter.mlssoccer.com/matchcenter/', date, '-', th, '-vs-', ta, '/boxscore')
	proxy.string = use_proxy("")
	htmlpage = html_session(url, proxy.string)
	
	# test for status 404
	tmp = melt(data.table(htmlpage))
	test = grepl('status_code = 404', tmp[6,1,with=F])
	if (test==TRUE) return(NULL)
	
	# scrape player list
	player_scr = htmlpage %>%  
		html_nodes('.ps-name') %>% 
		html_nodes(xpath = './a')

	# if the current year/club doesn't exist, return nothing
	if (length(player_scr)==0) return(NULL) 

	# scrape table by column
	positions_scr = htmlpage %>%  
		html_nodes('.ps-table td:nth-child(2)') 

	minutes_scr = htmlpage %>%  
		html_nodes('.ps-table td:nth-child(4)') 

	goals_scr = htmlpage %>%  
		html_nodes('.ps-table td:nth-child(5)') 

	assists_scr = htmlpage %>%  
		html_nodes('.ps-table td:nth-child(6)') 

	shots_scr = htmlpage %>%  
		html_nodes('.ps-table td:nth-child(7)') 

	sog_scr = htmlpage %>%  
		html_nodes('.ps-table td:nth-child(8)') 

	corners_scr = htmlpage %>%  
		html_nodes('.ps-table td:nth-child(9)') 

	offsides_scr = htmlpage %>%  
		html_nodes('.ps-table td:nth-child(10)') 

	fouls_scr = htmlpage %>%  
		html_nodes('.ps-table td:nth-child(11)') 

	fouled_scr = htmlpage %>%  
		html_nodes('.ps-table td:nth-child(12)') 

	goaldets_scr = htmlpage %>%  
		html_nodes('.bx-goals td') 

	subs_scr = htmlpage %>%  
		html_nodes('.bx-subs td')

	cards_scr = htmlpage %>%  
		html_nodes('.bx-bookings td') 
	
	stadium_scr = htmlpage %>%  
		html_nodes('.match-info div:nth-child(1)') 

	attendance_scr = htmlpage %>%  
		html_nodes('.match-info div:nth-child(2)') 

	weather_scr = htmlpage %>%  
		html_nodes('.match-info div:nth-child(3)') 

	# scrape refs/managers from different page
	url = paste0('http://matchcenter.mlssoccer.com/matchcenter/', date, '-', th, '-vs-', ta, '/lineup')
	proxy.string = use_proxy("")
	htmlpagelineup = html_session(url, proxy.string)

	refs_scr = htmlpagelineup %>%  
		html_nodes('tbody:nth-child(1)') %>% 
		html_nodes('.name:nth-child(4)')

	managers_scr = htmlpagelineup %>%  
		html_nodes('.manager-table .name')
	
	# format player
	player = as.character(player_scr)
	player = do.call('rbind', str_split(player,'>|<'))[,3]
	if (all(is.na(player))) return(NULL) 
	
	# format stat table columns
	cols = c('positions_scr', 'minutes_scr', 'goals_scr', 'assists_scr', 'shots_scr', 
			'sog_scr', 'corners_scr', 'offsides_scr', 'fouls_scr', 'fouled_scr')
	toSubs = c('Position', 'Minutes', 'Goals', 'Assists', 'Shots', 'Shots', 'Corners', 'Offsides', 'Fouls', 'Fouls')
	for(c in seq_along(cols)) {
		colname = gsub('_scr', '', cols[c])
		toSub = toSubs[c]
		tmpCol = as.character(get(cols[c]))
		tmpCol = tmpCol[!grepl(toSub, tmpCol)]
		if (colname=='sog') tmpCol = tmpCol[!grepl('Saves', tmpCol)]
		if (colname=='corners') tmpCol = tmpCol[!grepl('Punches', tmpCol)]
		if (colname=='offsides') tmpCol = tmpCol[!grepl('Crosses', tmpCol)]
		tmpCol = gsub('<td class=\"\" data-reactid=\"', '', tmpCol)
		tmpCol = gsub('<td class=\"ps-zero\" data-reactid=\"', '', tmpCol)
		if (colname!='positions') tmpCol = as.numeric(do.call('rbind', str_split(tmpCol,'>|<'))[,2])
		if (colname=='positions') tmpCol = do.call('rbind', str_split(tmpCol,'>|<'))[,3]
		assign(colname, tmpCol)
	}
	
	# formax goal details
	if(sum(goals)==0) goaldets = data.table(player=as.character(NA), goal_1=as.character(NA), pk_1=as.character(NA))
	if (sum(goals)>0) {
		goaldets = as.character(goaldets_scr)
		goaldets = data.table(do.call('rbind', str_split(goaldets, '>|<')))
		goaldets = goaldets[, c('V3','V5','V9'), with=FALSE]
		goaldets[, V3:=shift(V3,2)]
		goaldets = goaldets[!V5 %in% c('','\n')]
		setnames(goaldets, names(goaldets), c('goal','player','pk'))
		goaldets[, id:=seq_len(.N),by='player']
		goaldets = dcast.data.table(goaldets, player~id, value.var=c('goal','pk'))
		for(v in names(goaldets)[grepl('pk',names(goaldets))]) goaldets[, (v):=ifelse(get(v)==' (PK)', get(gsub('pk', 'goal', v)), NA)]
	}
	
	# format subs
	subs = as.character(subs_scr)
	subs = data.table(do.call('rbind', str_split(subs, '>|<')))
	subs = subs[, c('V3','V11','V27'), with=FALSE]
	subs[, V3:=shift(V3,2)]
	subs = subs[!V11 %in% c('','\n')]
	setnames(subs, names(subs), c('minute','subbed_in','subbed_out'))
	subs = dcast.data.table(melt(subs, id.vars='minute', value.name='player'), player~variable, value.var='minute')
	
	# format cards WHAT IF THERE ARE 0 CARDS?
	cards = as.character(cards_scr)
	if (length(cards)>0) { 
		cards = data.table(do.call('rbind', str_split(cards, '>|<|=')))
		cards = cards[, c('V5','V7','V9'), with=FALSE]
		cards[, V5:=shift(V5,3)]
		cards[, V7:=shift(V7)]
		cards = cards[!V9 %in% c('','td class')]
		cards[, V7:=gsub('"bx-booking-icon bx-','', V7)]
		cards[, V7:=gsub('" data-reactid','', V7)]
		setnames(cards, names(cards), c('minute','card','player'))
		cards = dcast.data.table(cards, player~card, value.var='minute')
	}
	if (length(cards)==0) cards = data.table(player=as.character(NA), yellow=as.character(NA))
	
	# format stadium
	stadium = as.character(stadium_scr)
	stadium = str_split(stadium,'-->|<!--')[[1]][11]
	
	# format attendance
	attendance = as.character(attendance_scr)
	tmpA = do.call('rbind', str_split(attendance,'-->|<!--'))
	if (is.null(tmpA)) tmpA = data.frame(NA)
	if (ncol(tmpA)<7) attendance = NA
	if (ncol(tmpA)>=7) attendance = as.numeric(tmpA[,7])
	
	# format weather
	weather = as.character(weather_scr)
	weather = do.call('rbind', str_split(weather,'-->|<!--'))[,7]
	if (is.null(weather)) weather = NA
	
	# format refs
	refs = as.character(refs_scr)
	refs = do.call('rbind', str_split(refs,'>|<'))[,3]
	while(length(refs)<4) refs = c(refs, NA)
	
	# format managers
	managers = as.character(managers_scr)
	managers = do.call('rbind', str_split(managers,'>|<'))[,3]
	
	# test that lengths match up
	if (length(unique(c(length(minutes), length(goals), length(assists), length(shots), length(sog), length(corners), length(offsides), length(fouls), length(fouled))))>1) stop('Something went wrong formatting columns')
	
	# format current match data
	currMatch = data.table(team_home=th, team_away=ta, 'date'=date, 
		player=player, positions=positions, minutes=minutes, goals=goals, assists=assists, shots=shots, 
		sog=sog, corners=corners, offsides=offsides, fouls=fouls, fouled=fouled, 
		attendance=attendance, weather=weather, ref1=refs[1], ref2=refs[2], ref3=refs[3], ref4=refs[4], 
		manager_home=managers[1], manager_away=managers[2], stadium=stadium)
	
	# identify player' teams. it goes 16 home, 16 away, 2 home (gk) 2 away WHAT IF THERES NO GK ON THE BENCH???
	team = c(rep(th, 16), rep(ta, 16), rep(th, 2), rep(ta, 2))
	currMatch[, team:=team]
	
	# add goal details, cards and subs
	currMatch = merge(currMatch, goaldets, by='player', all.x=TRUE)
	currMatch = merge(currMatch, cards, by='player', all.x=TRUE)
	currMatch = merge(currMatch, subs, by='player', all.x=TRUE)
	
	# return matches
	return(currMatch)
}
# ------------------------------------------------------------------------------------------



# ---------------------------------------------------------------------------------------
# Loop over matches, scrape the lineups and save individually

# not all matches have "matchcenter" data available
# recaps begin in 2011
# matchcenter begins in 2013

# load match data to loop over
matchData = fread(inFile)
matchData[, id:=seq_len(.N)]

# loop over matches post-2013
ids = matchData[year(date)>=2013]$id
I = length(ids)
prev_pct = 0
for(cid in ids) {

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
	mlOutFile = paste0('./webscrape/match_boxscores/', th, '_', ta, '_', date, '.csv')
	if (!reScrapeAll & file.exists(mlOutFile)) next
	
	# scrape
	currMatch = lineupScraper(th, ta, date)
	
	# save current match
	write.csv(currMatch, mlOutFile, row.names=FALSE)
	
	# report to user
	i = which(cid==ids)
	pct_done = floor(i/I*100)
	if (pct_done!=prev_pct) cat(paste0(pct_done, '% '))
	prev_pct = pct_done
}
# ---------------------------------------------------------------------------------------


# ---------------------------------------------------------------------
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
	inFile = paste0('./webscrape/match_boxscores/', th, '_', ta, '_', date, '.csv')
	if (!file.exists(inFile)) next
	matches = fread(inFile)
	if (i==1) all_matches = matches
	if (i>1) all_matches = rbind(all_matches, matches, fill=TRUE)
	i=i+1
}
# ---------------------------------------------------------------------


# ---------------------------------------------------------------------------------
# Save

# drop duplicates
all_matches = unique(all_matches)

# order by date/home team
all_matches = all_matches[order(date, team_home)]

# convert names back
for(t in unique(all_matches$team_home)) {
	for(v in c('team_home','team_away','team')) {
		st = convertTeamNamesRev(t)
		if(is.na(t) | is.na(st)) next
		all_matches[get(v)==t, (v):=st]
	}
}

# archive old data
archiveFile = gsub('.csv', paste0(today, '.csv'), outFile)
archiveFile = gsub('webscrape', 'webscrape/archive', archiveFile)
file.copy(from=outFile, to=archiveFile, overwrite=TRUE)

# save data
write.csv(all_matches, outFile, row.names=FALSE)
# ---------------------------------------------------------------------------------
