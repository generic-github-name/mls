# ----------------------------------------------
# David Phillips
#
# 11/4/2017
# Does possession cause wins? Or does winning cause possession?
# ----------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(RColorBrewer)
library(ggplot2)
# --------------------


# ------------------------------------------------------
# Files and directories

# input files
inFilePasses2015 = './webscrape/ASA/2015/raw passes.csv'
inFileShots2015 = './webscrape/ASA/2015/raw shots 2015.csv'
inFileDribbles2016 = './webscrape/ASA/2016/dribbles.csv'
inFilePasses2016 = './webscrape/ASA/2016/raw passes.csv'
inFileShots2016 = './webscrape/ASA/2016/raw shots.csv'
inFileDribbles2017 = './webscrape/ASA/2017/dribbles.csv'
inFilePasses2017 = './webscrape/ASA/2017/raw passes.csv'
inFileShots2017 = './webscrape/ASA/2017/raw shots.csv'
inFileDribbles2018 = './webscrape/ASA/2018/dribbles.csv'
inFilePasses2018 = './webscrape/ASA/2018/raw passes.csv'
inFileShots2018 = './webscrape/ASA/2018/raw shots.csv'
# ------------------------------------------------------


# ------------------------------------------------------------------------------------------
# Load/prep data

# load
data = NULL
for(a in c('Dribbles','Passes','Shots')) {
	for(year in seq(2015,2018)) {
		if (a=='Dribbles' & year==2015) next 

		# load
		tmpData = fread(get(paste0('inFile', a, year)))
			
		# identify
		tmpData[, action:=a]

		# rename
		if (a=='Shot') setnames(tmpData, 'shooter', 'player')
		if (a=='Passes') setnames(tmpData, c('success','passer'), c('outcome','player'))
		if (a=='Dribbles') setnames(tmpData, 'success', 'outcome')

		# append
		data = rbind(data, tmpData, fill=TRUE)
	}
}
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# Identify who has possession at all times

# ------------------------------------------------------------------------------------------

