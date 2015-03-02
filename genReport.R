args <- commandArgs(TRUE)
if (length(args) < 5) stop("Bad args, usage refdir cmpdir")

wDir <- args[2]
print(wDir)
setwd(wDir)
gPath <- args[3]
sGraph <- parse(text=paste0("'", args[4], "'"))[[1]]
degreePath <- args[5]
netName <- args[6]
direct <- args[7]

library('knitr', warn.conflicts = FALSE, quietly=TRUE)
library('igraph', warn.conflicts = FALSE, quietly=TRUE)
library('xtable', warn.conflicts = FALSE, quietly=TRUE)
library('VGAM', warn.conflicts = FALSE, quietly=TRUE)
library('stats4', warn.conflicts = FALSE, quietly=TRUE)

# print("all ok")
# 
print(gPath)
print(sGraph) 
print(degreePath) 
print(netName) 
print(getwd())
knit2pdf('degreeReport.Rnw')
