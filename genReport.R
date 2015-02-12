library(knitr)
args <- commandArgs(TRUE)
if (length(args) < 5) stop("Bad args, usage refdir cmpdir")

wDir <- args[2]
print(wDir)
setwd(wDir)
gPath <- args[3]
sGraph <- parse(text=paste0("'", args[4], "'"))[[1]]
degreePath <- args[5]
netName <- args[6]
# print("all ok")
# 
print(gPath)
print(sGraph) 
print(degreePath) 
print(netName) 
print(getwd())
knit2pdf('degreeReport.Rnw')
