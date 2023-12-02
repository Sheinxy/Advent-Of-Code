## Day 02

Awful parsing, great execution!

```hs
import Text.Regex.TDFA
import System.Environment

data Cube = Red Int | Blue Int | Green Int deriving (Show, Read)

type Input = [[Cube]]
type Output = Int

parseInput :: String -> Input
parseInput = map (map makeCube . matchCube) . lines
    where matchCube s = s =~ "([0-9]+) (red|green|blue)" :: [[String]]
          makeCube [_, n, "red"  ] = Red   (read n)
          makeCube [_, n, "blue" ] = Blue  (read n)
          makeCube [_, n, "green"] = Green (read n)

partOne :: Input -> Output
partOne = sum . map fst . filter (all id . map validate . snd) . zip [1 .. ]
    where validate (Blue  n) = n <= 14
          validate (Green n) = n <= 13
          validate (Red   n) = n <= 12

partTwo :: Input -> Output
partTwo = sum . map (\x -> minimumBlue x * minimumGreen x * minimumRed x)
    where isBlue  (Blue  _) = True
          isBlue  _         = False
          isRed   (Red   _) = True
          isRed   _         = False
          isGreen (Green _) = True
          isGreen _         = False
          minimumBlue  = maximum . map (\(Blue  n) -> n) . filter isBlue
          minimumGreen = maximum . map (\(Green n) -> n) . filter isGreen
          minimumRed   = maximum . map (\(Red   n) -> n) . filter isRed
```

Notice first that I am using [Text.Regex.TDFA](https://hackage.haskell.org/package/regex-tdfa-1.3.2.2/docs/Text-Regex-TDFA.html),
because it made the parsing SO MUCH EASIER. I just lost time on learning how it works and managing to import it while running the code lol.

Look at how easy it simplifies the parsing:
```hs
parseInput :: String -> Input
parseInput = map (map makeCube . matchCube) . lines
    where matchCube s = s =~ "([0-9]+) (red|green|blue)" :: [[String]]
          makeCube [_, n, "red"  ] = Red   (read n)
          makeCube [_, n, "blue" ] = Blue  (read n)
          makeCube [_, n, "green"] = Green (read n)
```
I simply get each line, and for each line I run a regex that gets everything of the form "number colour", then I put all the matches inside a data structure!

YIPPEE 😻

Next is the first puzzle:
```hs
partOne :: Input -> Output
partOne = sum . map fst . filter (all id . map validate . snd) . zip [1 .. ]
    where validate (Blue  n) = n <= 14
          validate (Green n) = n <= 13
          validate (Red   n) = n <= 12
```
I zip to get the index of each game, then I only keep the one that respect the number of boxes of each colour and I sum the IDs of the one I kept.

The second puzzle is kind of ugly but it works:
```hs
partTwo :: Input -> Output
partTwo = sum . map (\x -> minimumBlue x * minimumGreen x * minimumRed x)
    where isBlue  (Blue  _) = True
          isBlue  _         = False
          isRed   (Red   _) = True
          isRed   _         = False
          isGreen (Green _) = True
          isGreen _         = False
          minimumBlue  = maximum . map (\(Blue  n) -> n) . filter isBlue
          minimumGreen = maximum . map (\(Green n) -> n) . filter isGreen
          minimumRed   = maximum . map (\(Red   n) -> n) . filter isRed
```
For each game I find the minimum number necessary for a colour by first keeping only the data entry for that colour,
and then finding the maximum number of time that that colour has been picked (which is the minimum number of boxes I need, maximum is minimum lol).

Then I multiply the results of each colour for a game, and I sum all the results.

![Cat with sunglasses because it's rad](https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fwallpaperaccess.com%2Ffull%2F621550.jpg&f=1&nofb=1&ipt=be36eabe17b7991d3ca1ae566b8ed97125008975dcc55654dc2831bd6b15ec26&ipo=images)
