module Main where

import           Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import           System.Environment
import           System.Process
import           System.IO

type Input = M.Map String [String]
type Output = Int

parseInput :: String -> Input
parseInput = foldr (makeGraph . words) M.empty . lines
    where addOtherEdge succ node        = M.insertWith (++) node [succ]
          makeGraph (node : succ) graph = foldr (addOtherEdge nodeName) graph' succ
            where nodeName = init node
                  graph'   = M.insertWith (++) nodeName succ graph

getDot :: Input -> String
getDot input = "graph {\n\t" ++ nodes ++ "\n}"
    where nodes = intercalate "\n\t" [key ++ " -- {" ++ intercalate ", " (filter (> key) succ) ++ "};" | (key, succ) <- M.assocs input]

removeEdge :: Input -> (String, String) -> Input
removeEdge graph (a, b) = M.adjust (filter (/= b)) a $ M.adjust (filter (/= a)) b graph

getComponent :: Input -> S.Set String
getComponent graph = bfs (S.singleton start) [start]
    where start = (head . M.keys) graph
          bfs seen []        = seen
          bfs seen (x:queue) = bfs seen' queue'
            where neighbours = filter (`S.notMember` seen) $ graph M.! x
                  seen'      = S.union seen $ S.fromList neighbours
                  queue'     = queue ++ neighbours

partOne :: Input -> IO Output
partOne input = do
    let extension = "pdf"
    let filename  = "graph." ++ extension

    file <- openFile filename WriteMode

    (Just hin, _, _, _) <- createProcess (proc "neato" ["-T" ++ extension]){ std_out = UseHandle file, std_in = CreatePipe }
    hPutStrLn hin $ getDot input
    hClose hin
    hClose file

    _ <- createProcess (proc "xdg-open" [filename])

    putStrLn "Select edges to remove (e.g: (\"bvb\", \"cmg\"), (\"nvd\", \"jqt\"), (\"hfx\", \"pzl\")):"
    edgesS     <- getLine
    let edges  = read $ "[" ++ edgesS ++ "]" :: [(String, String)]
    let graph  = foldl' removeEdge input edges
    let s1     = S.size $ getComponent graph
    let s2     = M.size input - s1

    return (s1 * s2)

partTwo :: Input -> Output
partTwo = const $ length "merry xmas!"

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "dot"   = putStrLn . getDot  $ input
compute input "one"   = print =<<  partOne   input
compute input "two"   = print    . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
