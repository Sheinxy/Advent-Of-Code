#!/bin/sh

cd $1

for i in $(seq 1 12); do
    num=`printf %02d $i`
    day=Day$num
    mkdir -p src/$day
    cd src/$day
    cat > Main.hs <<EOF
module $day.Main (day$num) where

import AOC (submit)

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = undefined

partOne :: Input -> Output
partOne = undefined

partTwo :: Input -> Output
partTwo = undefined

day$num :: String -> String -> IO ()
day$num "parse" = print . parseInput
day$num "one"   = print . partOne . parseInput
day$num "two"   = print . partTwo . parseInput
day$num "sone"  = submit $1 $i 1 . show . partOne . parseInput
day$num "stwo"  = submit $1 $i 2 . show . partTwo . parseInput
day$num _ = error "Undefined part"
EOF
    stylish-haskell -i Main.hs
    cd - >/dev/null
done

cd ..
