# ----------------------------------------------
# 11/11/2018
# Descriptive analysis of injuries and absences
# ----------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(RColorBrewer)
library(ggplot2)
# --------------------


# ----------------------------------------------
# Files and directories

# input file
inFile = './webscrape/absences_for_every_match.csv'

# output files
graphFile = './injuries/graphs.pdf'
# ----------------------------------------------


# ----------------------------------------------
# Load/prep data

# load
data = fread(inFile)

# subset variables
data = data[, c('var1','x','y','z'), with=FALSE]

# subset observations
data = data[!is.na(var1)]

# format variables


# reshape data


# make new variables

# ----------------------------------------------


# ---------------------------------
# Run analysis

# model formula
form = as.formula('y ~ x + z')

# run model
glmOut = glm(form, 'gaussian', data)

# predict fitted values
data[, pred:=predict(glmOut)]
# ---------------------------------


# ----------------------------------------------
# Graph

# colors
cols = brewer.pal(6, 'Paired')

# store graph
p = ggplot(data, aes(y=y, x=x, color=z)) + 
	geom_point() + 
	geom_line(aes(y=pred)) + 
	scale_color_gradientn('Z', colors=cols) + 
	theme_bw()
# ----------------------------------------------


# --------------------------------
# Save graphs
pdf(graphFile, height=6, width=9)
p
dev.off()
# --------------------------------
