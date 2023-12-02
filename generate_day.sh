#!/bin/sh

INPUT="String"
OUTPUT="String"
MODULES=""
DRY_RUN=0
YEAR=`date +%Y`
DAY=`date +%d`

until [ $# -eq 0 ]; do
    case $1 in
        -i|--input)   INPUT="$2"; shift;;
        -o|--output)  OUTPUT="$2"; shift;;
        -m|--modules) MODULES="$MODULES $2"; shift;;
        -y|--year)    YEAR="$2"; shift;;
        -d|--day)     DAY="$2"; shift;;
        --dry-run)    DRY_RUN=1;;
        *)            DAY="$1";;
    esac
    shift
done

YEAR=`echo $YEAR | bc`
DAY=`echo $DAY | bc`

CONTENT=`cat <<EOF

type Input = $INPUT
type Output = $OUTPUT

parseInput :: String -> Input
parseInput = error "Not Implemented"

partOne :: Input -> Output
partOne = error "Not Implemented"

partTwo :: Input -> Output
partTwo = error "Not Implemented"

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> (readFile $ last args)
    mapM (compute input)  $ init args 
EOF
`

CONTENT="`echo import System.Environment`
$CONTENT"

for module in $MODULES; do
    CONTENT="`echo import $module`
$CONTENT"
done

CONTENT="`echo module Main where`

$CONTENT"

if [ $DRY_RUN -eq 1 ]; then
    echo "Year: $YEAR"
    echo "Day: $DAY"
    echo "Template: "
    echo "$CONTENT"
    exit
fi

if [ ! -d "$YEAR" ]; then
    mkdir $YEAR
fi

cd $YEAR
mkdir Day_`printf %02d $DAY`
cd Day_`printf %02d $DAY`

echo "$CONTENT" > Day_`printf %02d $DAY`.hs
curl --cookie "session=$AOC_SESSION" "https://adventofcode.com/$YEAR/day/$DAY/input" > input

echo "## Day `printf %02d $DAY` > README.md
