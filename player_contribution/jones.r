# table of wins/losses/xg with and without joevin jones

# set up R
rm(list=ls())
library(data.table)
library(ggplot2)

# files
inFileLineups = './webscrape/ASA/2017/Starting Lineups.csv'
inFileShots = './webscrape/ASA/2017/shots with xG.csv'
inFileResults = './webscrape/ASA/2017/Game Information.csv'
inFileJones = './player_contribution/jones.csv'
outFile = './player_contribution/jones.pdf'

# load data
lineups = data.table(read.csv(inFileLineups))
shots = fread(inFileShots)
results = fread(inFileResults)

# aggregate to game level
xg = shots[, list(xg=sum(xGvalueT)), by=c('gameID','team')]

# add xg to game results
results = merge(results, xg, by.x=c('gameID','hteam'), by.y=c('gameID','team'), all.x=TRUE)
results = merge(results, xg, by.x=c('gameID','ateam'), by.y=c('gameID','team'), all.x=TRUE)
setnames(results, c('xg.x', 'xg.y'), c('xg_home','xg_away'))

# subset to only Sounders
data = results[hteam=='Seattle' | ateam=='Seattle']

# format date
data[, date:=as.Date(date, '%m/%d/%Y')]

# identify when/where Joevin Jones started
jones = fread(inFileJones)
jones[, date:=as.Date(date, '%d-%B-%y')]
data = merge(data, jones[,c('date','position'),with=F], by='date', all.x=TRUE)
data[is.na(position), date:=date-1]
data$position = NULL
data = merge(data, jones[,c('date','minutes','position','sub'),with=F], by='date', all.x=TRUE)
data[position=='', position:=NA]
data[grepl('In',sub), position:=NA]
data[is.na(position), position:='DNS']

# make stats
data[, winner:=ifelse(hfinal>afinal,hteam,ateam)]
data[hfinal==afinal, winner:='draw']
data[, wld:=ifelse(winner=='Seattle',3,0)]
data[winner=='draw', wld:=1]
data[hteam=='Seattle', xgf:=xg_home]
data[ateam=='Seattle', xgf:=xg_away]
data[hteam=='Seattle', xga:=xg_away]
data[ateam=='Seattle', xga:=xg_home]
data[hteam=='Seattle', gf:=hfinal]
data[ateam=='Seattle', gf:=afinal]
data[hteam=='Seattle', ga:=afinal]
data[ateam=='Seattle', ga:=hfinal]
data[, gd:=gf-ga]
data[, xgd:=xgf-xga]

# collapse
agg = data[, list(games=.N,
		wins=sum(wld==3),
		loss=sum(wld==0),
		draw=sum(wld==1),
		ppg=mean(wld),
		gf=sum(gf),
		ga=sum(ga), 
		gf_pg=mean(gf),
		ga_pg=mean(ga), 
		gd_mean=mean(gd), 
		xgf=sum(xgf),
		xga=sum(xga), 
		xgf_pg=mean(xgf),
		xga_pg=mean(xga),
		xgd_mean=mean(xgd)), 
		by='position']

# add more stats
agg[, gd_total:=gf-ga]
agg[, xgd_total:=xgf-xga]
		
# set up to graph
graphData = melt(agg, id.vars='position')		
graphData[variable=='ppg', variable:='PPG']
graphData[variable=='games', variable:='Games']
graphData[variable=='wins', variable:='Wins']
graphData[variable=='loss', variable:='Losses']
graphData[variable=='draw', variable:='Draws']
graphData[variable=='gf', variable:='GF']
graphData[variable=='ga', variable:='GA']
graphData[variable=='gf_pg', variable:='GF per Game']
graphData[variable=='ga_pg', variable:='GA per Game']
graphData[variable=='gd_mean', variable:='Mean GD']
graphData[variable=='gd_total', variable:='Total GD']
graphData[variable=='xgf', variable:='xGF']
graphData[variable=='xga', variable:='xGA']
graphData[variable=='xgf_pg', variable:='xGF per Game']
graphData[variable=='xga_pg', variable:='xGA per Game']
graphData[variable=='xgd_mean', variable:='Mean xGD']
graphData[variable=='xgd_total', variable:='Total xGD']
graphData[, position:=factor(position, levels=c('LB','LM','DNS'), order=TRUE)]

# colors
colors = c('#77AAAD', '#AACD6E', '#F16B6F')

# visualize totals
p1 = ggplot(graphData[variable %in% c('Games','Wins','Losses','Draws','GF','GA','xGF','xGA','Total xGD','Total GD')], aes(y=value, x=variable, fill=position)) + 
	geom_bar(stat='identity', position='dodge') + 
	scale_fill_manual('Joevin Jones\'\nStarting Position', values=colors) + 
	labs(title='Sounders Success', y='', x='') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16))
	
# visualize per game
p2 = ggplot(graphData[variable %in% c('PPG','GF per Game','GA per Game','Mean GD','xGF per Game','xGA per Game','Mean xGD')], aes(y=value, x=variable, fill=position)) + 
	geom_bar(stat='identity', position='dodge') + 
	scale_fill_manual('Joevin Jones\'\nStarting Position', values=colors) + 
	labs(title='Sounders Success', y='', x='') + 
	theme_bw() + 
	theme(axis.title.y=element_text(size=14), axis.text.x=element_text(angle=315, hjust=0, size=14), plot.title=element_text(hjust=.5, size=16))
	
pdf(outFile, height=6, width=9)
p1
p2
dev.off()
