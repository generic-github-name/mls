# Scrape match-level data
# 6/29/2017
# goal.com scrapable but basic, can't find archives
# https://www.socceramerica.com/publications/soccer-america-daily seems good
# foxsports.com blocks scrapers
# espn.com infinite scroll
# mlssoccer.com inifite scroll
# flashscore.com inifite scroll
# sports.yahoo.com can't find the right element

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

# base url
baseURL = 'https://www.whoscored.com/Matches/1163784/LiveStatistics/USA-Major-League-Soccer'

# output file
outFile = './webscrape/whoscored_match_data.csv'

# whether to re-scrape everything or just update all clubs in the current year
reScrapeAll = FALSE

# store today's date
today = Sys.Date()
# -----------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# Create a function to scrape
mlsScraper = function(year, club) {

		# scrape
		url = 'http://www.goal.com/en-us/results/mls/33'
		proxy.string = use_proxy("")
		htmlpage = html_session(url, proxy.string)
		matches_scr = htmlpage %>%  
			html_nodes('.away span, .vs div , .home span') 

			# scrape
			url = 'https://sports.yahoo.com/soccer/scoreboard/?schedState=2&dateRange=17'
			proxy.string = use_proxy("")
			htmlpage = html_session(url, proxy.string)
			matches_scr = htmlpage %>%  
				html_nodes('#scoreboard-group-2') 
			as.character(matches_scr)
				
		# scrape
		url = 'https://www.whoscored.com/Matches/1163770/MatchReport/USA-Major-League-Soccer-2017-New-York-City-FC-New-England-Rev-'
		proxy.string = use_proxy("")
		htmlpage = html_session(url, proxy.string)
		matches_scr = htmlpage %>%  
			html_nodes('.result') 
		
		%>% 
		html_nodes(xpath = './a') %>% 
		html_attr('href')

		.time , .home, th, .right, .away
		
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
# ------------------------------------------------------------------------------------------

