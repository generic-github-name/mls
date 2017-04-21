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


# ----------------------------------------------------------------------------------------------------
# loop over years and scrape every backup known to the wayback machine
i=1
for(y in 2013:2017) {
	# store html info
	url = paste0('https://web.archive.org/web/', y, '0515000000*/https://www.mlssoccer.com/injuries')
	proxy.string = use_proxy("")
	htmlpage = html_session(url, proxy.string)
		
	# scrape data on every archive they have crawled
	if (i==1) archives = as.character(htmlpage %>% html_nodes('#calOver a'))
	if (i>1) archives = c(archives, as.character(htmlpage %>% html_nodes('#calOver a')))
	i=i+1
	
	# sleep because the wayback machine seems to know what I'm up to
	Sys.sleep(5)
}
# ----------------------------------------------------------------------------------------------------


# -----------------------------------------------------------------------------------------
# Loop over backups and get the data from them
i = 1
for(a in archives) {

	# store URL ID from wayback and skip if already scraped
	id = str_split(a, '/')[[1]][3]
	outFile = paste0(outDir, id, '.rds')
	if (!reScrape & file.exists(outFile)) next 

	# store html info
	url = paste0('https://web.archive.org/web/', id, '/http://www.mlssoccer.com/injuries')
	proxy.string = use_proxy("")
	htmlpage = html_session(url, proxy.string)
	
	# scrape injury data
	injury_scr = htmlpage %>% html_nodes('#node-74493 td , td p')
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
files = list.files(outDir)
for(f in files) {
	tmp = data.table(readRDS(paste0(outDir, '/', f)))
	tmp[, V1:=gsub('\t', '', V1)]
	tmp[, V1:=gsub('\n', '', V1)]
	tmp[, V1:=gsub('</li>','',V1)]
	tmp[, V1:=gsub('<li>',',',V1)]
	tmp[, V1:=gsub('</ul>','',V1)]
	tmp[, V1:=gsub('<ul>','',V1)]
	tmp[, V1:=gsub('<td>','',V1)]
	tmp[, V1:=gsub('</td>','',V1)]
	tmp[, V1:=gsub('<p>','',V1)]
	tmp[, V1:=gsub('</p>','',V1)]
	tmp[, V1:=gsub('</i>','',V1)]
	tmp[, V1:=gsub('<td style="vertical-align: top;">,', '', V1)]
	tmp[, V1:=gsub('<i style="text-align: right;">', ',', V1)]
	tmp[, V1:=gsub('<td style="vertical-align: top;">', ',', V1)]
	
	tmp[grepl('/files/', V1), team:=V1
}
# -----------------------------------------------------------------------------------------
