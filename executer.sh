#!/bin/bash

#GRAPH_FILES=("./datasets/Amazon0302.txt" "./datasets/wiki-Talk.txt" "./datasets/cit-Patents.txt" "./datasets/person_knows_person_0_0.csv" "./datasets/web-NotreDame.txt" "./datasets/com-lj.ungraph.txt" "./datasets/com-youtube.ungraph.txt" "./datasets/com-dblp.ungraph.txt" "./datasets/roadNet-CA.txt" "./datasets/roadNet-TX.txt")

#DEGREE_FILES=("./datasets/amazonDegree.txt" "./datasets/wikiDegree.txt" "./datasets/citDegree.txt" "./datasets/personDegrees.csv" "./datasets/notreDameDegree.txt" "./datasets/livejournalDegree.txt" "./datasets/youtubeDegree.txt" "./datasets/dblpDegree.txt" "./datasets/roadnetCADegree.txt" "./datasets/roadnetTXDegree.txt")

#SEP_CHARACTER=("\t" "\t" "\t" "|" "\t" "\t" "\t" "\t" "\t" "\t")

#DIRECTED=("T" "T" "T" "F" "T" "F" "F" "F" "F" "F")

#NET_NAMES=("Amazon" "Wikipedia" "Patents" "Person" "NotreDame" "Live_Journal" "Youtube" "DBLP_co_authorship" "CA_Road_Net" "TX_Road_Net")

GRAPH_FILES=("./datasets/Amazon0302.txt" "./datasets/Amazon0302.txt" "./datasets/wiki-Talk.txt" "./datasets/wiki-Talk.txt" "./datasets/cit-Patents.txt" "./datasets/cit-Patents.txt" "./datasets/person_knows_person_0_0.csv" "./datasets/web-NotreDame.txt" "./datasets/web-NotreDame.txt" "./datasets/com-lj.ungraph.txt" "./datasets/com-youtube.ungraph.txt" "./datasets/com-dblp.ungraph.txt" "./datasets/roadNet-CA.txt" "./datasets/roadNet-TX.txt")

DIRECTED=("T" "T" "T" "T" "T" "T" "F" "T" "T" "F" "F" "F" "F" "F")

DEGREE_FILES=("./datasets/inAmazonDegrees.txt" "./datasets/outAmazonDegrees.txt" "./datasets/inWikipediaDegrees.txt" "./datasets/outWikipediaDegrees.txt" "./datasets/inPatentsDegrees.txt" "./datasets/outPatentsDegrees.txt" "./datasets/outPersonDegrees.csv" "./datasets/inNotreDameDegrees.txt" "./datasets/outNotreDameDegrees.txt" "./datasets/outLive_JournalDegrees.txt" "./datasets/outYoutubeDegrees.txt" "./datasets/outDBLP_co_authorshipDegrees.txt" "./datasets/outCA_Road_NetDegrees.txt" "./datasets/outTX_Road_NetDegrees.txt")

SEP_CHARACTER=("\t" "\t" "\t" "\t" "\t" "\t" "|" "\t" "\t" "\t" "\t" "\t" "\t" "\t")

NET_NAMES=("In_Amazon" "Out_Amazon" "In_Wikipedia" "Out_Wikipedia" "In_Patents" "Out_Patents" "Person" "In_NotreDame" "Out_NotreDame" "Live_Journal" "Youtube" "DBLP_co_authorship" "CA_Road_Net" "TX_Road_Net")

BASE_DIR=`pwd`

for ((i=0; i < ${#GRAPH_FILES[@]}; i++))
do
	echo "----- Processing network ${NET_NAMES[$i]} -----"
	OUT_DIR="${BASE_DIR}/nets/${NET_NAMES[$i]}"
	echo 
	if [ ! -d $OUT_DIR ]; then
		mkdir -p ./nets/${NET_NAMES[$i]}
	fi

	Rscript genReport.R --args $BASE_DIR ${GRAPH_FILES[$i]} ${SEP_CHARACTER[$i]} ${DEGREE_FILES[$i]} ${NET_NAMES[$i]} ${DIRECTED[$i]} #1> log${NET_NAMES[$i]}.txt

	OUTPUT=${OUT_DIR}/${NET_NAMES[$i]}Report
	mv degreeReport.tex ${OUTPUT}.tex

	if [ -d ${OUT_DIR}/figure ]; then
		rm -rfv ${OUT_DIR}/figure
	fi

	mv -f figure/ $OUT_DIR
	mv data_${NET_NAMES[$i]}.RData $OUT_DIR
	rm *.log *.aux *.pdf
	cd $OUT_DIR
	pdflatex ${NET_NAMES[$i]}Report.tex
	mutt -s "${NET_NAMES[$i]} Graph Analysis" -a "${OUTPUT}.pdf" -- aduarte@ac.upc.edu < /dev/null
	cd ${BASE_DIR}
	echo "---------------Graph processed--------------"
	echo
done
