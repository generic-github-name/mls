# ----------------------------------------------
# David Phillips
#
# 11/4/2017
# Template for a self-contained analysis
# ----------------------------------------------

# to do
# figure out how to rotate a coord_polar without clipping the first bar
# adding limits=c(0,360) to scale_x_continuous clips
# removing it orients the whole graph to the top-right
# adding xlim=c(0,360) to coord_polar does nothing
# adding xlim=c(0,360) to coord_cartesian (before coord_polar) does nothing

# --------------------
# Start function
makeGraph = function(data) { 
	# --------------------


	# --------------------
	# Set up R
	library(data.table)
	library(RColorBrewer)
	library(ggthemes)
	library(ggplot2)
	# --------------------


	# ----------------------------------------------
	# Load/prep data

	# make test data
	set.seed(1)
	data = data.table(direction=sample(seq(0, 360, 45), 200, replace=TRUE), 
						success=rbinom(200, 1, .75))
	
	# collapse
	tmp = copy(data)
	tmp[direction==360, direction:=0]
	agg = tmp[, .N, by=direction]
	agg[, direction:=direction+0]
	# ----------------------------------------------


	# ----------------------------------------------
	# Graph

	# colors
	cols <- brewer.pal(6, 'Paired')

	# store graph
	p <- ggplot(agg, aes(y=N, x=direction)) + 
		geom_bar(size=3, stat='identity') + 
		coord_polar() + 
		scale_x_continuous(breaks=seq(0, 360, by=45),
			labels=c('Forward', '', 'Right', '', 'Backward', '', 'Left', '', '')) +
		scale_size_area() +
		theme_bw()
	
	# ----------------------------------------------
pdf('C:/local/test.pdf') 
p
dev.off()

	# --------------------------------
	# return graphs
	return(p)
}
# --------------------------------
