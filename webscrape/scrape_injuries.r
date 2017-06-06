# 4/11/2017
# web scrape MLS injury data using the wayback machine and www.mlssoccer.com/injuries


# ------------------------------
# Set up R
rm(list=ls())
library(data.table)
library(stringr)
library(rvest)
library(httr) # for "use_proxy"
# ------------------------------


# --------------------------------------------------
# Files, directories and settings

# output location
outDir = './webscrape/injuries/'

# whether to re-scrape everything or just new files
reScrape = FALSE
# --------------------------------------------------


# # ----------------------------------------------------------------------------------------------------
# # loop over years and scrape every backup known to the wayback machine

# # SOMEHOW THIS IS BROKEN SINCE I FIRST WROTE IT!!!

# i=1
# for(y in 2013:2017) {
	# # store html info
	# url = paste0('https://web.archive.org/web/', y, '0515000000*/https://www.mlssoccer.com/injuries')
	# proxy.string = use_proxy("")
	# htmlpage = html_session(url, proxy.string)
	
	# # scrape data on every archive they have crawled
	# if (i==1) archives = as.character(htmlpage %>% html_nodes('#calOver a'))
	# if (i>1) archives = c(archives, as.character(htmlpage %>% html_nodes('#calOver a')))
	# i=i+1
	
	# # sleep because the wayback machine seems to know what I'm up to
	# Sys.sleep(5)
# }
# # ----------------------------------------------------------------------------------------------------


# -----------------------------------------------------------------------------------------
# Loop over backups and get the data from them

# NOTE: prior to March 6th, 2015, MLS used a different table format and seemed ot update less consistently
# for now, I'll just focus on 2015+
archives = fread('./webscrape/injuries/list_of_archives.csv')
archives = gsub('.rds', '', archives[[1]])

i = 1
for(a in archives) {

	# # store URL ID from wayback and skip if already scraped
	# id = str_split(a, '/')[[1]][3]
	id = a
	outFile = paste0(outDir, id, '.rds')
	if (!reScrape & file.exists(outFile)) next 

	# store html info
	url = paste0('https://web.archive.org/web/', id, '/http://www.mlssoccer.com/injuries')
	proxy.string = use_proxy("")
	htmlpage = html_session(url, proxy.string)
	
	# scrape injury data
	injury_scr = htmlpage %>% html_nodes('p')
	injury_scr = as.character(injury_scr)
	
	# store current injury list in its raw state
	saveRDS(injury_scr, outFile)
	
	# sleep because the wayback machine seems to know what I'm up to
	Sys.sleep(5)
	if (i %% 10 == 0) Sys.sleep(10)
	if (i %% 20 == 0) Sys.sleep(10)
	i=i+1
}
# -----------------------------------------------------------------------------------------


# -----------------------------------------------------------------------------------------
# Append injury data

# so broken. the mls injury website seems to switch html format every few months. this makes it impossible to munge the data

i=1
for(f in archives) {
	tmp = data.table(readRDS(paste0(outDir, '/', f, '.rds')))
	tmp[, V1:=gsub('<p style="margin: 0px 0px 8px;">', '', V1)]
	tmp[, V1:=gsub('<strong>', '', V1)]
	tmp[, V1:=gsub('</strong>', '', V1)]
	tmp[, V1:=sub('<a.*</a>', '', V1)] # this removes urls
	tmp[, V1:=sub('</p>', '', V1)]
	tmp[, V1:=sub(' -', '', V1)]
	tmp[, V1:=sub(' -', '', V1)]
	tmp[, V1:=sub('<b>', '', V1)]
	tmp[, V1:=sub('</b>', '', V1)]
	tmp[, V1:=sub('<p style="margin: 0px 0px 8px;">', '', V1)]
	tmp[, V1:=sub('<p>', '', V1)]
	tmp[, V1:=sub('\n', '', V1)]
	tmp[, V1:=sub('<p style="font-size:13px;">', '', V1)]
	
	tmp = tmp[!grepl('copyrights', V1)]
	tmp = tmp[!grepl('Last Updated', V1)]
	tmp = tmp[!grepl('NOTE:', V1)]
	tmp = tmp[V1!='']
	tmp = tmp[V1!='None']
	
	tmp[grepl('OUT',V1), status:='Out']
	tmp[grepl('QUESTIONABLE',V1), status:='Questionable']
	tmp[is.na(status), status:='Unspecified']

	tmp[, date:=as.Date(str_sub(f, 1, 8),format='%Y%m%d')]
	
	if (i==1) data = tmp
	if (i>1) data = cbind(data, tmp)
	i=i+1
}
# -----------------------------------------------------------------------------------------
