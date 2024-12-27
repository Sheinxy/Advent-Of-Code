#!/bin/sh

DRY_RUN=0
YEAR=`date +%Y`
DAY=`date +%d`
NEW_DIR=true

until [ $# -eq 0 ]; do
    case $1 in
        -d|--day)         DAY="$2"; shift;;
        --dry-run)        DRY_RUN=1;;
        -nn|--no-new-dir) NEW_DIR=false; shift;;
        -y|--year)        YEAR="$2"; shift;;
        *)                DAY="$1";;
    esac
    shift
done

YEAR=`echo $YEAR | bc`
DAY=`echo $DAY | bc`

if [ $DRY_RUN -eq 1 ]; then
    echo "Year: $YEAR"
    echo "Day: $DAY"
    exit
fi

if $NEW_DIR; then
    if [ ! -d "$YEAR/inputs" ]; then
        mkdir -p $YEAR/inputs
    fi
    cd $YEAR/inputs
else
    mkdir -p inputs
    cd inputs
fi

FILE=`printf %02d $DAY`
curl --cookie "session=$AOC_SESSION" "https://adventofcode.com/$YEAR/day/$DAY/input" > $FILE
