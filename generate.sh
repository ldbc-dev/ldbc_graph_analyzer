#!/bin/bash

GRAPH_FILES=("./datasets/Amazon0302.txt" "./datasets/wiki-Talk.txt" "./datasets/cit-Patents.txt" "./datasets/person_knows_person_0_0.csv" "./datasets/web-NotreDame.txt" "./datasets/com-lj.ungraph.txt" "./datasets/com-youtube.ungraph.txt" "./datasets/com-dblp.ungraph.txt" "./datasets/roadNet-CA.txt" "./datasets/roadNet-TX.txt")

DIRECTED=("T" "T" "T" "F" "T" "F" "F" "F" "F" "F")

NET_NAMES=("Amazon" "Wikipedia" "Patents" "Person" "NotreDame" "Live_Journal" "Youtube" "DBLP_co_authorship" "CA_Road_Net" "TX_Road_Net")

BASE_DIR=`pwd`

for ((i=0; i < ${#GRAPH_FILES[@]}; i++))
do

	if [ "${DIRECTED[$i]}" == "T" ]; then
		python generateDegreeFile.py ${GRAPH_FILES[$i]} "./datasets/out${NET_NAMES[$i]}Degrees.txt" "./datasets/in${NET_NAMES[$i]}Degrees.txt"
		#echo "Direct"
	else
		python generateDegreeFile.py ${GRAPH_FILES[$i]} "./datasets/out${NET_NAMES[$i]}Degrees.txt" 
		#echo "undirected"
	fi
	echo "---------------Generate degree sequence file(s) for ${NET_NAMES[$i]}-------------"
	echo
done
