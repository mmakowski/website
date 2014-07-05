-- requires directory-tree
module Graph where

import Control.Exception.Base
import Data.Monoid
import Data.String.Utils
import qualified Data.ByteString.Char8 as B
import System.Directory.Tree
import System.Environment
import System.FilePath.Posix
import Text.Regex.PCRE

type Node = String
type Edge = (Node, Node)
data Graph = Graph { graphNodes :: [Node]
                   , graphEdges :: [Edge]
                   }

instance Monoid Graph where
  mempty = Graph [] []
  mappend (Graph n1 e1) (Graph n2 e2) = Graph (n1 ++ n2) (e1 ++ e2)

main :: IO ()
main = do
  args <- getArgs 
  let dir = parseCmdLineArgs args
  graph <- buildGraph dir
  putStrLn $ render graph

parseCmdLineArgs :: [String] -> FilePath
parseCmdLineArgs [dir] = dir
parseCmdLineArgs _     = error usageMessage

buildGraph :: FilePath -> IO Graph
buildGraph wikidataDir = do
  (_ :/ tree) <- readDirectory wikidataDir
  case tree of
    (Failed _ e)  -> failedToReadError wikidataDir e
    (File _ _)    -> error $ wikidataDir ++ " is a file"
    (Dir _ elems) -> buildGraph' "" elems mempty

buildGraph' :: FilePath -> [DirTree String] -> Graph -> IO Graph
buildGraph' _ [] g = return g
buildGraph' prefix (f:fs) g = do
  remaining <- buildGraph' prefix fs g 
  case f of
    (Failed nm e)  -> failedToReadError nm e
    (File nm c)    -> return $ (if isWikiPage nm then updateGraph mempty (prefix </> nm) c else mempty) `mappend` remaining
    (Dir nm elems) -> if nm `elem` dirsToIgnore then return remaining
                      else do 
                        nested <- buildGraph' (prefix </> nm) elems mempty
                        return $ nested `mappend` remaining

updateGraph :: Graph -> FilePath -> String -> Graph
updateGraph (Graph ns es) fp content = Graph (nodeName fp : ns) (es ++ edgesInContent fp content)

edgesInContent :: FilePath -> String -> [Edge]
edgesInContent fp = map (\l -> (nodeName fp, l)) . linksInContent

linksInContent :: String -> [String]
linksInContent content = extractMatches $ B.pack content =~ linkRegex

extractMatches :: [[B.ByteString]] -> [String]
extractMatches matches = filter (not . null) $ groups 1 matches ++ groups 2 matches 

groups :: Int -> [[B.ByteString]] -> [String]
groups n = map (B.unpack . (!! n))

linkRegex :: String
linkRegex = "\\[((?!http)[^]\\n]*)\\]\\(\\)|\\[[^]\\n]*\\]\\(((?!http)[^)\\n]+)\\)"

isWikiPage :: FileName -> Bool
isWikiPage = (== ".page") . takeExtension

render :: Graph -> String
render (Graph nodes edges) = "digraph wiki {\n" ++ renderNodes nodes ++"\n" ++ renderEdges edges ++ "}"

nodeName :: FilePath -> String
nodeName = dropExtension 

renderNodes :: [Node] -> String
renderNodes = concatMap renderNode

renderEdges :: [Edge] -> String
renderEdges = concatMap renderEdge

renderNode :: Node -> String
renderNode n = "\"" ++ escape n ++ "\";\n"

renderEdge :: Edge -> String
renderEdge (n1, n2) = "\"" ++ escape n1 ++ "\" -> \"" ++ escape n2 ++ "\";\n"

escape :: String -> String
escape = replace "\"" "\\\""

usageMessage :: String
usageMessage = "usage: graph <wikidata dir>"

failedToReadError :: FilePath -> IOException -> a
failedToReadError f e = error $ "failed to read " ++ f ++ ": " ++ show e

dirsToIgnore :: [FileName]
dirsToIgnore = [".git"]
