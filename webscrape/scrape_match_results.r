# Scrape mlssoccer.com for historical w/l/d data
# output files saved in the current working directory

# set up R
rm(list=ls())
library(data.table)
library(stringr)
library(rvest)
library(httr) # for "use_proxy"

# create a function to scrape
mlsScraper = function(year, club) {

	# scrape
	url = paste0('https://www.mlssoccer.com/schedule?month=all&year=', year, '&club=', club, '&club_options=Filters&op=Update&form_build_id=form-f_TJdS2RcMLGXTOUGPWX3DTEaEJQmN3VtjECvzAae20&form_id=mp7_schedule_hub_search_filters_form')
	proxy.string = use_proxy("")
	htmlpage = html_session(url, proxy.string)
	matches_scr = htmlpage %>%  
		html_nodes('.club_name') %>% 
		html_nodes(xpath = './a') %>% 
		html_attr('href')

	scores_scr = htmlpage %>%  
		html_nodes('.match_score')

	competitions_scr = htmlpage %>%  
		html_nodes('.match_location_competition')

	dates_scr = htmlpage %>%  
		html_nodes('.match_date')

	# if the current year/club doesn't exist, return nothing
	if (length(matches_scr)==0) return(NULL) 
	
	# format scores
	scores = as.character(scores_scr)
	scores = gsub('<span class=\"match_score\">', '', scores)
	scores = gsub('</span>', '', scores)
	re = '\\(([^()]+)\\)'
	pks = as.numeric(gsub(re, '\\1', str_extract_all(scores, re))) # pentaly kicks in parens
	scores = as.numeric(scores)
	scores[is.na(scores)] = pks[is.na(scores)] # this considers pentaly kicks to be the only goals in a game
	if (all(is.na(scores))) return(NULL) 
	
	# format competitions
	competitions = as.character(competitions_scr)
	competitions = gsub('<div class=\"match_location_competition\">', '', competitions)
	competitions = gsub(' \\/.*', '', competitions)

	# format dates
	dates = as.character(dates_scr)
	dates = gsub('<div class=\"match_date\">', '', dates)
	dates = gsub('</div>', '', dates)
	dates = as.Date(dates, "%A, %B %d, %Y")
	
	# format matches
	matches = as.character(matches_scr)
	matches = gsub('/meta/club/', '', matches)
	n = length(matches)/2
	matches = data.table(match=rep(seq(n), each=2), team=matches, venue=rep(c('home', 'away'), n), score=scores)
	matches = dcast.data.table(matches, match~venue, value.var=c('team', 'score'))
	matches[, competition:=competitions]
	matches[, date:=dates]
	matches$match=NULL
						
	# identify winners						
	matches[score_home>score_away, winner:=team_home]
	matches[score_home<score_away, winner:=team_away]
	matches[score_home==score_away, winner:='draw']
	
	# return matches
	return(matches)
}

# loop over years/clubs (even ones that don't exist) and save individually b/c it takes forever
year_range = seq(2010, 2017)
club_range = seq(342) # team number 342 (Curacao) seems to be the most recently-added team to MLS's internal database this presumably means that there's at least one year in which each team from 1 to 342 has data
print(paste('Scraping data from years:', paste(year_range, collapse=' ')))
print(paste('Scraping data from clubs:', paste(club_range, collapse=' ')))
cat('Percent complete: ')
i=0
I = length(year_range)*length(club_range)
prev_pct = 0
for(y in year_range) {
	for(c in club_range) {
		i = i+1
		matches = suppressWarnings(mlsScraper(y, c))
		if (is.null(matches)) next
		outFile = paste0('./club_years/', c, '_', y, '.csv')
		write.csv(matches, outFile, row.names=FALSE)
		pct_done = floor(i/I*100)
		if (pct_done!=prev_pct) cat(paste0(pct_done, '% '))
		prev_pct = pct_done
	}
}

# append all files
all_matches = fread('./all_matches_in_mls_website_1996-2009.csv') # append to existing file because I only started saving club-year files after I had already scraped 1996-2009
for(y in year_range) {
	for(c in club_range) {
		inFile = paste0('./club_years/', c, '_', y, '.csv')
		if (!file.exists(inFile)) next
		matches = fread(inFile)
		all_matches = rbind(all_matches, matches, fill=TRUE)
	}
}

# drop duplicates
all_matches = unique(all_matches)

# order by date/home team
all_matches = all_matches[order(date, team_home)]

# save data
write.csv(all_matches, './all_matches_in_mls_website.csv', row.names=FALSE)
