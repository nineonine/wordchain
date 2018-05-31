{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

{-
The idea is simple - First we create a dictionary
where we store all the words form the lexicon.
Then we build a Search tree.
Then we traverse tree in BFS fashion to find the target word.
This will be the shortest path from start word.
-}

module Library (
    getDict
  , answer
  , validateWord
  ) where

-- lets import all the goodies
import Data.Char (toUpper)
import Control.Monad (guard)
import qualified Data.Trie as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

-- | A dictionary which contains all the words.
--   A trie is a suitable data structure for our case.
--   Also, few synonyms to improve readability of type signatures.
type Dict      = T.Trie BS.ByteString
type StartWord = BS.ByteString
type EndWord   = BS.ByteString
type Path      = [BS.ByteString]

-- | N-ary search tree for our chains
data NTree = Node {
    label    :: BS.ByteString
  , path     :: Path
  , children :: [NTree]
} deriving (Eq, Show)

-- | Some constant values we will use later
--   pack [65..90] -> "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
letters = [65..90]

-- | Since bytestring library does not export its data constructor
--   there is no way to pattern match on it
--   so with a help of some pragmas we create few pattern synonyms
--   which unlocks pattern matching on bytestrings for us
infixr 5 :#
pattern a :# as <- (BS.uncons -> Just (a, as))
pattern Empty   <- (BS.uncons -> Nothing)

-- | Since we are using bytestring in our trie as keys
--   we will use it as our main String type throughout the program
--   we also deliberately trade off safety for performance here
input :: IO BS.ByteString
input = BS.readFile "four_letter_words.txt"

-- remove carriage returns
parse :: BS.ByteString  -- ^ input from file
      -> [BS.ByteString]
parse = C.words

-- create a dictionary (lexicon)
mkDict :: [BS.ByteString] -- ^ list of all possible words
       -> Dict
mkDict = T.fromList . map tuplize where
  tuplize = \a -> (a,a)

-- get file contents, parse and create a dictionary
getDict :: IO Dict
getDict = (mkDict . parse) <$> input

-- | The function that does all the heavy lifting in generating all word possibilites.
--   Some parts of it are quite hacky and not elegant
--   e.g. if the word length had to change we would need to rewrite this part.
--   Another gotcha is Int we are passing to set up char position we want to change
--   but as long as we keep this function private we are ok since only we control what to pass in.
--   List monad works nicely here!
step' :: Dict -- ^ Lexicon
      -> BS.ByteString -- ^ a word to change
      -> Int -- ^ character position we want to change
      -> [BS.ByteString]
step' dict word i = do
  n <- letters                   -- all the capital letters -> [A..Z]
  let (a:#b:#c:#d:#Empty) = word -- using our ad-hoc pattern matching here
      newWord = BS.pack $ case i of 1 -> [n,b,c,d]; 2 -> [a,n,c,d]; 3 -> [a,b,n,d]; 4 -> [a,b,c,n] -- THIS IS BAD!!!
  guard $ T.member newWord dict  -- check if the word is legit ( in lexicon )
  guard $ newWord /= word        -- do not include the word we are trying to change (avoid duplication)
  return newWord

-- | given a dictionary and a word
--   generate all the legit words with one char changed
--   BAD: word length is hardcoded here
step :: Dict -- ^ Lexicon
     -> BS.ByteString -- ^ a word to change
     -> [BS.ByteString]
step dict word = concatMap (step' dict word) [1..4]

-- | traverse our tree of chains in a BFS fashion
--   transforming it into list of tuples of words and their paths
--   breadth first is important in our algo
--   it would take us forever to solve it with DFS approach!
bfTransform :: [NTree] -- ^ our tree
            -> [(BS.ByteString, Path)]
bfTransform ts = map (\v -> (label v, path v) ) ts ++ bfTransform (concatMap children ts)

-- | create a tree the way it should be done in Haskell - LAZILY
mkTree :: Dict -- ^ Lexicon
       -> Path -- ^ path to this word
       -> BS.ByteString -- ^ node label
       -> NTree
mkTree dict path word = Node word path' children' where
  path'     = (word:path)
  children' = map (mkTree dict path') (step dict word)

-- | scan our transformed tree until we find endWord
--   we detect our endword with a predicate function f
--   we are witnessing the beauty of lazy evaluation here
--   we are evaluating the tree only to the point where we encounter our targetWord!
search :: ((BS.ByteString, Path) -> Bool) -- ^ predicate function to detect desired word
       -> NTree -- ^ our tree
       -> (BS.ByteString, [BS.ByteString])
search f t = head $ filter f (bfTransform [t])

-- | main function for getting the solution to the problem
answer :: Dict
       -> StartWord
       -> EndWord
       -> Path
answer dict start target = prepare $ search predicate tree where
  predicate = (==) target . fst
  tree      = mkTree dict [] start
  prepare   = tail . init . reverse . snd

-- | a handy function for discarding bad inputs.
--   We check if the word is of length 4 and is in our lexicon
--   again - hardcoded length is bad :-\
validateWord :: Dict -> String -> Bool
validateWord dict = and . satisfy [ofLength 4, isLegitWord] where
  satisfy preds w = map (flip ($) w) preds
  ofLength n      = (==) n . length
  isLegitWord     = flip T.member dict . C.pack . map toUpper

{-
Afterthought:
In general, BFS traversal is optimal here, but the running time is proportional
to a size of our search tree. The longer the chain, the longer we wait.

TODO: Potential optimizations worth looking into:

 - create a memoized version of our dictionary 'member' function

 - paralellize 'step' function where we generate all possible next words

 - algo improvement - we could sort the list of next possible words returned by 'step' function
   in a way that the words which contain characters from target word would go first.
   This way our 'children' field in our Tree data structure would become a priority queue of some sort

 - algo improvement - we could try to implement something that would start scanning
   a tree from both ends simultaneously - start and target words.
   We would need to end our search when the same node was found by both searches

 - algo / data structures improvement. look for inspiration in:
     A* search algorithm with Hamming distance as Heuristics
     Edit distance
     Disjoint set data structure
-}
