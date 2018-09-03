# 7/10/2017
# Attendance analyses


# ---------------------
# Set up R
rm(list=ls())
library(data.table)
library(stringr)
library(ggplot2)
library(RColorBrewer)
# ---------------------


# ----------------------------------------------------
# Files and directories

# input files
inFile2015 = './webscrape/ASA/2015/Game Information.csv' # ASA files have incorrect dates!
inFile2016 = './webscrape/ASA/2016/Game Information.csv'
inFile2017 = './webscrape/ASA/2017/Game Information.csv'

# output file
outfile = '/attendance/attendance.pdf'
# ----------------------------------------------------


# --------------------------------------------------------------
# load/prep data

# load files
data2015 = fread(inFile2015)
data2016 = fread(inFile2016)
data2017 = fread(inFile2017)

# append years
data = rbind(data2015, data2016, data2017, fill=TRUE)

# subset columns
vars = c('date','startTime','hteam','ateam','attendance','hfinal','afinal')
data = data[, vars, with=FALSE]

# addtitional variables
data[, date:=as.Date(date, '%m/%d/%Y')]
data[, year:=year(date)]
data[, day:=weekdays(date)]
data[, day:=factor(day, levels=c('Tuesday', 'Wednesday','Thursday','Friday','Saturday','Sunday','Monday'))]
data[, date2:=date+(365*(2017-year))]

# format variables
hms = data.table(do.call('rbind', str_split(data$startTime, ':')))
hms[, V1:=as.numeric(V1)]
hms[V1<10, V1:=V1+12]
hms[is.na(V1) & grepl('AM', V3), V1:=11]
data[, startTime:=]

# determine each team's season total points


# determine a team's league standing at every point

# --------------------------------------------------------------


# ----------------------------------------------------
# analyze

# ----------------------------------------------------


# ----------------------------------------------------
# set up to graph

# ----------------------------------------------------


# ----------------------------------------------------
# graph

# day of the week 
# NOTE: ASA DATA ARE INCORRECT
# ggplot(data, aes(x=attendance)) + 
	# geom_density(fill='green', alpha=.5) + 
	# facet_wrap(~day, scales='free_y',ncol=1)+ 
	# labs(y='Density of Games', x='Attendance', 
		# title='Attendance by Day of the Week', 
		# caption='') + 
	# theme_bw() + 
	# theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())

# over the course of a season
ggplot(data, aes(y=attendance, x=date2, color=factor(year), group=factor(year))) + 
	geom_line(size=1.5) + 
	facet_wrap(~hteam) + 
	theme_bw()

# start time
ggplot(data, aes(y=attendance, x=startTime)) + 
	geom_point() + 
	geom_smooth() + 
	theme_bw()

# ----------------------------------------------------
