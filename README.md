# Introduction  

Hello there!  

This repository contains my solutions for the [Advent of Code](https://adventofcode.com/).  

I've been participating since 2020 and writing up solutions since 2022.  

# Language  

My language of choice for these puzzles is [Haskell](https://en.wikipedia.org/wiki/Haskell).  

However, keep in mind that I have no formal education in Haskell—I'm still learning the language.
As a result, my code may not always be the smartest, cleanest, or most optimised.
I'm just a guy having fun with Haskell!  

# Running My Solutions  

From 2020 to 2022, my solutions were standalone.
To run them, simply compile each day's solution using `ghc` and execute it with
an `input.txt` file in the same directory as the executable.  

For 2023 and 2024, solutions remained standalone but now expect arguments when
running the executable. The last argument should be the input file,
while the others specify which part to run (typically `"one"` and `"two"`, but the code is flexible).  

More recently, I’ve been working on structuring each year as a single executable,
with the entire repository set up as a Cabal project, alongside a library.  

### Running Examples  

To run **Day 3, Parts One and Two** of 2015 with `file.txt` as input:  
```sh
cabal run AOC2015 03 one two file.txt
```  

To run multiple days, separate them with `/`:  
```sh
cabal run AOC2015 03 one two file.txt / 04 one two file.txt
```  

To use the default input location (e.g., `./2015/13.txt`), use `d` as the filename:  
```sh
# If your input file is actually named "d" (or something like "def" or "default"),
# you’ll need to specify it as "./d"
cabal run AOC2015 13 one two d
```  

To submit an answer using my AOC library, use `sone` or `stwo`:  
```sh
# You'll need to export the environment variable AOC_SESSION, beware when doing that
cabal run AOC2015 23 sone d
```

# Shell scripts

I have a few helper shell scripts, however only ywo are really of interest here:
- ```./shell-utils/init_year.sh [-y|--year YEAR] [YEAR]: setups everything for a given year (defaults to the current year)```
- ```./shell-utils/retrieve_input.sh [-d|--day DAY] [--dry-run] [-nn|--no-new-dir] [-y|--year YEAD] [DAY]: retrieves the input for a give day and year (defaults to the current one). Dry runs will only print the year and day. If -nn is activated, the input will be places under the ./inputs folder, otherwise under the ./$YEAR/inputs one.```

# Write-Ups  

I've been writing up solutions since 2022. (Though I only covered the first few days in 2022, 2023 onward is complete.)  

- [2022](https://github.com/Sheinxy/Advent2022)  
- [2023](./2023)  
- [2024](./2024)  
- [2025](./2025)  
