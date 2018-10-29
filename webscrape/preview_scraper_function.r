# -----------------------------------------------
# 10/28/2018
# Function to scrape the match preview absences data for a single games
# This is intended to be run repeatedly by scrape_previews.r
#
# Inputs
# - th: object of class character containing the name of home team 
# 	(in "mlssoccer.com URL format", see convert_to_url_names.r)
# - ta: object of class character containing the name of away team 
# 	(in URL format)
# - date: object of class character or date (YYYY-MM-DD) corresponding to date of match
#
# Outputs
# - absences: object of class data.table with rows for each player absent and 7 columns:
# 		hteam: (character) home team name (same as input)
# 		ateam: (character) away team name (same as input)
#		explanation: (character) reason for absence (Injury, Suspension, International Duty)
#		status: (character) status of absence (out, questionable, doutbful)
# 		player: (character) name of player missing
# 		reason: (character) reason for absence (could be type of injury, name of national team or other) 
# 			(whatever they list in the parentheses after the player name on mlssoccer.com)
# 		date: (character) date of match (same as input)
# -----------------------------------------------


# ----------------------------------------
# Create function to scrape
# ----------------------------------------
previewScraper = function(th, ta, date) {

	# ------------------------------------------------------------
	# 1. Scrape the raw text off the website

	# identify URL
	url = paste0('http://matchcenter.mlssoccer.com/matchcenter/', 
				date, '-', th, '-vs-', ta, '/preview')
	proxy.string = use_proxy("")
	htmlpage = html_session(url, proxy.string)
	
	# test for status 404
	tmp = melt(data.table(htmlpage))
	test1 = grepl('status_code = 404', tmp[6,1,with=F])
	
	# if the page can't be found, try the day before 
	# (because dates are in GMT in ASA but local time in MLS)
	if (test1==TRUE) {
	
		# subtract one day
		date = as.character(as.Date(date)-1)
		
		# scrape again
		url = paste0('http://matchcenter.mlssoccer.com/matchcenter/', 
					date, '-', th, '-vs-', ta, '/preview')
		proxy.string = use_proxy("")
		htmlpage = html_session(url, proxy.string)
		
		# test for status 404
		tmp = melt(data.table(htmlpage))
		test2 = grepl('status_code = 404', tmp[6,1,with=F])
		if (test2==TRUE) return(NULL)
	}	
	
	# scrape the whole text
	absences_scr = htmlpage %>%  
		html_nodes('li , p') %>% 
		html_text()
	
	# skip if there is nothing
	if (length(absences_scr)>0) { 
		print(paste('Returning NULL from', url, ':'))
		print(absences_scr)
		return(NULL)
	}
	# ------------------------------------------------------------


	# ------------------------------------------------------------
	# 2. Rangle raw text into orderly columns

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
	
	# deal with lack of return characters by keying in on parentheses
	fullline = absences[nrow(absences)]$absences_scr
	for(e in c(explanations,extras)) { 
		everythingbefore = data.table(absences_scr=sub(paste0(e,'.*'),'\\1', fullline))
		everythingafter = data.table(absences_scr=sub(paste0('.*(',e,')'),'\\1', fullline))
		absences = rbind(absences, everythingbefore)
		absences = rbind(absences, everythingafter)
	}
	absences = unique(absences)
	absences = absences[absences_scr!='']
	
	# skip if there is nothing left
	if (nrow(absences)<1) { 
		print(paste('Returning NULL from', url, ':'))
		print(absences)
		return(NULL)
	}

	# if the parsing above failed, we'll just have to lose the data
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
	
	# identify the current match
	absences[, hteam:=th]
	absences[, ateam:=ta]
	absences$date = date
	
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
	# ------------------------------------------------------------
	

	# ------------------------------------------------------------
	# 3. Reshape lists of players and statuses into different rows
	
	# reshape out/questionable long
	absences$absences_scr = NULL
	absences = melt(absences, id.vars=c('hteam','ateam','date','explanation'), 
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
	}
	absences$players = NULL
	
	# melt players long
	absences = melt(absences, id.vars=c('hteam','ateam','date','explanation','status'))
	absences = absences[!is.na(value)]
	# ------------------------------------------------------------
	

	# ------------------------------------------------------------
	# 4. Find and replace lingering text we no longer need

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
	absences[, reason:=gsub(')', '', reason)]
	
	# sub out team names and statuses from player names
	absences[, reason:=str_trim(reason)]
	absences[, player:=str_trim(player)]
	teams = c('sea:', 'ny: ', 'clb:', 'mtl:', 'chv:', 'chi:', 'col:', 'dc: ', 'van:', 'kc: ', 'ne: ', 'dal:', 'hou:', 'sj: ', 'por:', 'phi:', 'rsl:', 'tor:', 'la: ', 'orl:', 'nyc:', 'nyr:', 'ny:')
	statuses_nodashes = c('out','questionable','doubtful','warnings')
	punctuations = c(':', ' --', ',', '- ', ' - ', ' – ', '/', '\u2010', ' \u2010',
		'\u2011', ' \u2011', '\u2012', ' \u2012', '\u2013', ' \u2013', '\u2014', ' \u2014', 
		'\u002D', ' \u002D', '\u1806', ' \u1806', '\ufe58', ' \ufe58', '\u2015', ' \u2015')
	statuses = apply(expand.grid(statuses_nodashes, punctuations), 1, paste, collapse='')
	for(t in c(teams,statuses)) {
		absences[, player:=gsub(t, '', player)]
		absences[, player:=gsub(paste0(' ', t), '', player)]
	}		
	
	# remove position listings with hypthens or without
	absences[, player:=str_trim(player)]
	positions_nodashes = c('g', 'gk', 'k', 'd', 'd/m', 'm', 'f', 'mf', 'df/mf', 'fw', 'w')
	positions = apply(expand.grid(positions_nodashes, punctuations), 1, paste, collapse='')
	for(p in positions) absences[, player:=gsub(p, '', player)]
	for(p in positions_nodashes) { 
		absences[str_sub(player,1,nchar(p)+1)==paste0(p,' '), player:=str_sub(player,nchar(p)+1)]
	}
	absences[, player:=str_trim(player)]
		
	# remove miscellaneous other invalid characters
	misc = c('…','\\.','=--','none\\"\\"','c\\"\\"',' , NA',',NA','cNA,','\\"\\"',' \\*')
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
	keepVars = c('hteam', 'ateam', 'date', 'explanation', 'status', 'player', 'reason')
	absences = absences[, keepVars, with=FALSE]
	# ------------------------------------------------------------
	
	# ----------------
	# Return the data
	return(absences)
	# ----------------

# ----------------------------------------
# End function
}
# ----------------------------------------
