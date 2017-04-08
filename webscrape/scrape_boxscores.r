# Scrape mlssoccer.com for historical w/l/d data
# output files saved in the current working directory

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
	players_scr = htmlpage %>%  
		html_nodes('.ps-name') %>% 
		html_nodes(xpath = './a')

	# if the current year/club doesn't exist, return nothing
	if (length(players_scr)==0) return(NULL) 

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

	cards_scr = htmlpage %>%  
		html_nodes('.bx-bookings .pi-target') 

	card_colors_scr = htmlpage %>%  
		html_nodes('.bx-icon') 

	attendance_scr = htmlpage %>%  
		html_nodes('.match-info div:nth-child(2)') 

	weather_scr = htmlpage %>%  
		html_nodes('.match-info div:nth-child(3)') 

	# scrape refs/managers from different page
	url = paste0('http://matchcenter.mlssoccer.com/matchcenter/', date, '-', th, '-vs-', ta, '/lineup')
	proxy.string = use_proxy("")
	htmlpage = html_session(url, proxy.string)

	refs_scr = htmlpage %>%  
		html_nodes('tbody:nth-child(1)') %>% 
		html_nodes('.name:nth-child(4)')

	managers_scr = htmlpage %>%  
		html_nodes('.manager-table .name')
	
	# format players
	players = as.character(players_scr)
	players = do.call('rbind', str_split(players,'>|<'))[,3]
	# players = gsub('<a href=\"//www.mlssoccer.com/player/', '', players)
	# players = gsub('\" target=\"_blank\" data-reactid=\"', '', players)
	# players = gsub('\">', '', players)
	# players = gsub('</a>', '', players)
	# for(i in 0:9) players = gsub(i, '', players)
	if (all(is.na(players))) return(NULL) 
	
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
	
	# format goal details
	goaldets = as.character(goaldets_scr)
	goaldets = do.call('rbind', str_split(goaldets,'>|<'))
	goaldets = data.table(goaldets[,c(3,5,9)])
	if (nrow(goaldets)>0) { 
		goaldets[, minute:=shift(V1,2)]
		goaldets = goaldets[V2!='' & V2!='\n']
		goaldets$V1 = NULL
		setnames(goaldets, c('V2','V3'), c('players','pk'))
		goaldets[, pk:=ifelse(pk==' (PK)', '1', '0')]
		goaldets[, minute:=gsub('\'','',minute)]
		goaldets[, id:=seq_len(.N),by='players']
		goaldets = dcast.data.table(goaldets,players~id, value.var=c('pk','minute'))
		minuteVars = names(goaldets)[grepl('minute', names(goaldets))]
		# reformat minutes and pks as comma separated lists
		goaldets[, goal_minutes:=get(minuteVars[1])]
		if(length(minuteVars)>1) {
			for(m in minuteVars[2:length(minuteVars)]) {
				goaldets[, goal_minutes:=paste(goal_minutes, get(m), sep=', '), by='players']
			}
		}
		pkVars = names(goaldets)[grepl('pk', names(goaldets))]
		goaldets[, pks:=get(pkVars[1])]
		if(length(pkVars)>1) {
			for(p in pkVars[2:length(pkVars)]) {
				goaldets[, pks:=paste(pks, get(p), sep=', '), by='players']
			}
		}
		goaldets = goaldets[, c('players','goal_minutes','pks'), with=FALSE]
	}
	if (nrow(goaldets)==0) goaldets = data.table(players=as.character(NA), goal_minutes=as.character(NA), pks=as.character(NA))
	
	# format cards
	cards = as.character(cards_scr)
	cards = gsub('<span class=\"nodelay pi-target\" data-reactid=\"', '', cards)
	cards = do.call('rbind', str_split(cards,'>|<'))[,2]
	
	# format card colors
	card_colors = as.character(card_colors_scr)
	card_colors = do.call('rbind', str_split(card_colors,'>|<'))[,4]
	card_colors = do.call('rbind', str_split(card_colors,'\"'))[,2]
	card_colors = gsub('bx-booking-icon bx-', '', card_colors)
	
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
		players=players, positions=positions, minutes=minutes, goals=goals, assists=assists, shots=shots, 
		sog=sog, corners=corners, offsides=offsides, fouls=fouls, fouled=fouled, 
		attendance=attendance, weather=weather, ref1=refs[1], ref2=refs[2], ref3=refs[3], ref4=refs[4], 
		manager_home=managers[1], manager_away=managers[2])
	
	# identify players' teams. it goes 16 home, 16 away, 2 home (gk) 2 away WHAT IF THERES NO GK ON THE BENCH???
	team = c(rep(th, 16), rep(ta, 16), rep(th, 2), rep(ta, 2))
	currMatch[, team:=team]
	
	# add cards (after adding teams so it doesn't mess up the order)
	if (!is.null(cards) & !is.null(card_colors)) {
		cards = data.table(players=cards, card=card_colors)
	} else {
		cards = data.table(players=as.character(NA), card=as.character(NA))
	}
	currMatch = merge(currMatch, cards, by='players', all.x=TRUE)
	
	# add goal details
	currMatch = merge(currMatch, goaldets, by='players', all.x=TRUE)
	
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
