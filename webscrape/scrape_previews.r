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
# -----------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# Create a function to scrape
previewScraper = function(th, ta, date) {

	# scrape
	url = paste0('http://matchcenter.mlssoccer.com/matchcenter/', date, '-', th, '-vs-', ta, '/preview')
	proxy.string = use_proxy("")
	htmlpage = html_session(url, proxy.string)
	
	# test for status 404
	tmp = melt(data.table(htmlpage))
	test1 = grepl('status_code = 404', tmp[6,1,with=F])
	
	# if the page can't be found, try the day before 
	# (because dates are in GMT in ASA but local time in MLS)
	if (test1==TRUE) {
		date = as.character(as.Date(date)-1)
		
		# scrape again
		url = paste0('http://matchcenter.mlssoccer.com/matchcenter/', date, '-', th, '-vs-', ta, '/preview')
		proxy.string = use_proxy("")
		htmlpage = html_session(url, proxy.string)
		
		# test for status 404
		tmp = melt(data.table(htmlpage))
		test2 = grepl('status_code = 404', tmp[6,1,with=F])
		if (test2==TRUE) return(NULL)
	}	
	
	# scrape absences section
	absences_scr = htmlpage %>%  
		html_nodes('li , p') %>% 
		html_text()
	
	# skip if no info
	if (length(absences_scr)>0) {
		
		# clean up text
		
		# subset to just suspensions, international duty and injuries
		absences = data.table(absences_scr)
		absences[, absences_scr:=tolower(absences_scr)]
		injury_explanations = c('injury report:', 'disabled list:', 'disabled:', 'injuries/suspensions:')
		international_explanations = c('international duty:', 'int’l duty:',  'int\'l duty:', 
			'int\'l absence:', 'international absences:')
		suspension_explanations = c('suspended:')
		explanations = c(injury_explanations, international_explanations, suspension_explanations)
		absences = absences[grepl(paste(explanations, collapse='|'), absences_scr)]
		
		# other categories that could be listed but we don't really want
		extras = c('suspended next yellow card:', 'suspended after two yellow cards:', 
				'suspended after next caution:', 'suspended next yellow:', 'warnings:')
		
		# deal with lack of return characters
		fullline = absences[nrow(absences)]$absences_scr
		for(e in c(explanations,extras)) { 
			everythingbefore = data.table(absences_scr=sub(paste0(e,'.*'),'\\1', fullline))
			everythingafter = data.table(absences_scr=sub(paste0('.*(',e,')'),'\\1', fullline))
			absences = rbind(absences, everythingbefore)
			absences = rbind(absences, everythingafter)
		}
		absences = unique(absences)
		absences = absences[absences_scr!='']
		
		# skip if there is nothing
		if (nrow(absences)<1) { 
			print(paste('Returning NULL from', url, ':'))
			print(absences)
			return(NULL)
		}

		# remove any line of `absences` that still has at least two of the elements in `c(explanations,extras)`
		if (nrow(absences)>1) {  
			keep = apply(sapply(c(explanations,extras), function(i) grepl(i,absences$absences_scr)),1,sum)==1
			absences = absences[keep]
		}
		
		# exclude "suspected next caution"s
		for(e in extras) absences = absences[!grepl(e,absences_scr)]
		
		# label things nicely according to the lists of explanations above
		absences[grepl(paste(suspension_explanations, collapse='|'), absences_scr), explanation:='Suspended']
		absences[grepl(paste(international_explanations, collapse='|'), absences_scr), explanation:='International duty']
		absences[grepl(paste(injury_explanations, collapse='|'), absences_scr), explanation:='Injured']
		
		# other ad-hoc fixes that mess with parsing
		absences[, absences_scr:=gsub('; out for season)', ')', absences_scr)]
		absences[, absences_scr:=gsub('head-to-head.*$','', absences_scr)]

		# skip invalid extractions
		if (nrow(absences)<1 | any(is.na(absences$explanation))) { 
			print(paste('Returning NULL from', url, ':'))
			print(absences)
			return(NULL)
		}
		
		# parse different injury classes
		split = tstrsplit(absences$absences_scr,';', fixed=TRUE)
		for(i in seq(length(split))) { 
			if (any(grepl('out',split[[i]]))) absences[, out:=split[[i]]]
			if (any(grepl('doubtful',split[[i]]))) absences[, doubtful:=split[[i]]]
			if (any(grepl('questionable',split[[i]]))) absences[, questionable:=split[[i]]]
		}
		if (!any(grepl('out',split[[i]])) & 
			!any(grepl('doubtful',split[[i]])) & 
			!any(grepl('questionable',split[[i]]))) absences[, out:=split[[1]]]
		
		# identify teams
		absences[, hteam:=th]
		absences[, ateam:=ta]
		
		# sub out explanations
		for (var in names(absences)) { 
			for(term in gsub(':', '', explanations)) { 
				for(punctuation in c(':', '-', ' -', '–', ' –', '—', ' —')) { 
					absences[, (var):=gsub(paste0(term, punctuation), '', get(var))]
				}
			}
		}
		
		# skip games with no absences
		absences[str_sub(absences_scr,1,1)==' ', absences_scr:=str_sub(absences_scr,2)]
		absences[str_sub(absences_scr,2, )=='none', absences_scr:='none']
		absences[, absences_scr:=str_trim(absences_scr)]
		if (all(absences$absences_scr=='none') | all(is.na(absences$absences_scr))) { 
			print(paste('Returning NULL from', url, ':'))
			print(absences)
			print(absences$absences_scr)
			return(NULL)
		}
		
		# reshape out/questionable long
		absences$absences_scr = NULL
		absences = melt(absences, id.vars=c('hteam','ateam','explanation'), 
			variable.name='status', value.name='players')
		
		# split players into separate columns
		n_players = max(str_count(absences$player, ','),na.rm=TRUE)+1
		absences[, players:=gsub(') ', '), ', players)] # sometimes they missed a comma
		absences[, paste0('player',seq(n_players)):=tstrsplit(players,'),',fixed=TRUE)]
		for(v in paste0('player',seq(n_players))) absences[explanation=='Injured', (v):=paste0(get(v), ')')]
		for(i in seq(n_players)) { 
		
			# clean white spaces
			v = paste0('player',i)
			t = paste0('reason',i)
			absences[, (v):=gsub('  ', ' ', get(v))]
			absences[, (v):=gsub(' ', ' ', get(v))]
			absences[, (v):=str_trim(get(v))]
			
			# extract injury type from parentheses
			re = "\\(([^()]+)\\)"
			absences[, (t):=as.character(NA)]
			for(j in seq(nrow(absences))) {
				injuryDescription = gsub(re, "\\1", str_extract_all(absences[j][[v]], re)[[1]])
				if (length(injuryDescription)>0) absences[j, (t):=injuryDescription]
				absences[j, (v):=gsub(get(t),'',get(v))]
			}
			absences[,(v):=gsub('\\(', '', get(v))]
			absences[,(t):=gsub('\\(', '', get(t))]
			absences[,(v):=gsub(')', '', get(v))]
			absences[,(t):=gsub(')', '', get(t))]
			
			# remove position listings with hypthens or without
			absences[, (v):=str_trim(get(v))]
			positions = c('g- ', 'gk- ', 'd- ', 'd/m- ', 'm- ', 'f- ', 'mf- ', ' m/', 'df/mf',
			'fw- ', 'g - ', 'gk - ', 'd - ', 'd/m - ', 'm - ', 'f - ', 'mf - ', 'fw - ', 'm/',
			'fw – ', 'g – ', 'gk – ', 'd – ', 'd/m – ', 'm – ', 'f – ', 'mf – ', 'fw – ', 'm/')
			for(p in positions) absences[, (v):=gsub(p, '', get(v))]
			positions_nodashes = c('g ','gk ','d ','d/m ','m ','f ','mf ', 'fw ', 
								' g ',' gk ',' d ',' d/m ',' m ',' f ',' mf ', ' fw ')
			for(p in positions_nodashes) { 
				absences[str_sub(get(v),1,nchar(p))==p, (v):=str_sub(get(v),nchar(p))]
			}
			absences[, (t):=gsub(')', '', get(t))]
			absences[, (v):=str_trim(get(v))]
		}
		absences$players = NULL
		
		# melt players long
		absences = melt(absences, id.vars=c('hteam','ateam','explanation','status'))
		absences = absences[!is.na(value)]
		
		# separate reshape injury type and player name into two columns
		absences[, index:=gsub('player','',variable)]
		absences[, index:=as.numeric(gsub('reason','',index))]
		for(i in seq(n_players)) absences[, variable:=gsub(i, '', variable)]
		players = absences[variable=='player']
		reasons = absences[variable=='reason', 'value']
		setnames(players, 'value', 'player')
		setnames(reasons, 'value', 'reason')
		players$variable = NULL
		players$index = NULL
		absences = cbind(players, reasons)
		
		# sub out team names and statuses from player names
		absences[, reason:=str_trim(reason)]
		absences[, player:=str_trim(player)]
		teams = c('sea:', 'ny: ', 'clb:', 'mtl:', 'chv:', 'chi:', 'col:', 'dc: ', 'van:', 'kc: ', 'ne: ', 'dal:', 'hou:', 'sj: ', 'por:', 'phi:', 'rsl:', 'tor:', 'la: ', 'orl:', 'nyc:', 'nyr:', 'ny:')
		statuses = c('out','questionable','doubtful','warnings')
		punctuations = c(':',' –',' --',',')
		statuses = paste0(statuses, punctuations)
		for(t in c(teams,statuses)) {
			absences[, player:=gsub(t, '', player)]
			absences[, player:=gsub(paste0(' ', t), '', player)]
		}		
		
		# remove miscellaneous other invalid characters
		misc = c('…','...','=--','none\\"\\"','c\\"\\"',' , NA',',NA','cNA,','\\"\\"',' *')
		for(m in misc) {
			absences[,player:=gsub(m,'',player)]
			absences[,reason:=gsub(m,'',reason)]
			absences[, reason:=str_trim(reason)]
			absences[, player:=str_trim(player)]
		}
		
		# drop empty fields and duplicates
		absences = absences[player!='none']
		absences$index = NULL
		absences = unique(absences)
		keepVars = c('hteam', 'ateam', 'explanation', 'status', 'player', 'reason')
		absences = absences[, keepVars, with=FALSE]
		
		# identify date
		absences$date = date
		
		# return matches
		return(absences)
	}
}
# ------------------------------------------------------------------------------------------



# ---------------------------------------------------------------------------------------
# Loop over matches, scrape the lineups and save individually

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
	inFile = paste0('./webscrape/match_previews/', th, '_', ta, '_', date, '.csv')
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

# archive old data
archiveFile = gsub('.csv', paste0(today, '.csv'), outFile)
archiveFile = gsub('webscrape', 'webscrape/archive', archiveFile)
file.copy(from=outFile, to=archiveFile, overwrite=TRUE)

# save data
write.csv(all_matches, outFile, row.names=FALSE)
# ---------------------------------------------------------------------------------
