import sys, os
import math, string, re
from datetime import datetime, date, time
import time as _time

knowsFile = open(sys.argv[1],'r')

edgesPerPerson={}
SEPARATOR="\t"
index=0
numEdges=0;
for line in knowsFile.readlines():
    if not line.startswith('#'):
        if index > 0:
            edge =  re.split(r'\t', line)
            if int(edge[0]) in edgesPerPerson:
                edgesPerPerson[int(edge[0])]+=1
            else:
                edgesPerPerson[int(edge[0])]=1
            numEdges+=1
        index+=1   
knowsFile.close()

outputFile = open(sys.argv[2],'w')
histogram = {}
for person in edgesPerPerson:
    degree = edgesPerPerson[person]
    outputFile.write(str(degree)+"\n")
outputFile.close()