#!/bin/bash

SCRIPT=`basename ${BASH_SOURCE[0]}`
OPT_DIR=$HOME
OPT_GRAPH=G
OPT_GRAPH_SEP=S
OPT_DEGREE=D
OPT_NNAME=N

NUMARGS=$#
if [ $NUMARGS -ne 10 ]; then
	echo -e \\n"Help documentation."\\n
	echo -e "Basic usage:"\\n
	echo "The following switches are recognized."
	echo "	-w --Sets the working directory used. Default is HOME folder."
	echo "	-g --Sets the path to the graph file."
	echo "	-s --Sets the value for the separator used in the graph load process."
	echo "	-d --Sets the path to the degree data file."
	echo "	-n --Sets the name of the network."
	echo -e "-h --Displays this help message. No further functions are performed."\\n
    echo -e "Example: analyzer.sh -w working_directory -g graph_file_path -s separator -d degree_file_path -n network_name"\\n
	exit 1
fi

while getopts :w:g:s:d:n:h FLAG; do
	case $FLAG in
		w) OPT_DIR=$OPTARG
		   ;;
	    g) OPT_GRAPH=$OPTARG
		   ;;
		s) OPT_GRAPH_SEP=$OPTARG
		   ;;
		d) OPT_DEGREE=$OPTARG
		   ;;
		n) OPT_NNAME=$OPTARG
		   ;;
		*) echo -e "Use $SCRIPT -h to see the help documentation."\\n
		   exit 2
		   ;;
	esac
done

shift $((OPTIND-1))

BASE_DIR=`pwd`
OUT_DIR="${BASE_DIR}/nets/$OPT_NNAME"

if [ ! -d $OUT_DIR ]; then
	mkdir -p ./nets/$OPT_NNAME
fi

Rscript genReport.R --args $OPT_DIR $OPT_GRAPH $OPT_GRAPH_SEP $OPT_DEGREE $OPT_NNAME 

OUTPUT=${OUT_DIR}/${OPT_NNAME}Report


mv degreeReport.tex ${OUTPUT}.tex

if [ -d ${OUT_DIR}/figure ]; then
	rm -rfv ${OUT_DIR}/figure
fi

mv -f figure/ $OUT_DIR
mv data_${OPT_NNAME}.RData $OUT_DIR

rm *.log *.aux *.pdf
cd $OUT_DIR
pdflatex ${OPT_NNAME}Report.tex
mutt -s "${OPT_NNAME} Graph Analysis" -a "${OUTPUT}.pdf" -- aduarte@ac.upc.edu < /dev/null
