import sys, os
import math, string, re
from datetime import datetime, date, time
import time as _time

knowsFile = open(sys.argv[1],'r')

edgesPerPerson={}
inDegree = None

if sys.args[3]:
	inDegree = {}


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

            if inDegree !=None:            	
	            if int(edge[1]) in inDegree:
	                inDegree[int(edge[1])]+=1
	            else:
	                inDegree[int(edge[1])]=1

            numEdges+=1
        index+=1   
knowsFile.close()

outDegreeFile = open(sys.argv[2],'w')
histogram = {}
for person in edgesPerPerson:
    degree = edgesPerPerson[person]
    outDegreeFile.write(str(degree)+"\n")
outDegreeFile.close()

if inDegree != None:
	inDegreeFile = open(sys.argv[4],'w')
	histogram = {}
	for node in inDegree:
	    degree = inDegree[node]
	    inDegreeFile.write(str(degree)+"\n")
	inDegreeFile.close()



