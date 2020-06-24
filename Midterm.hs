module Midterm where

import ProbSLG
import Helpers

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
import Data.List (nub)

-------------------------------------------------------------------------------
-- Problem 1:
-------------------------------------------------------------------------------

listOfLists = [["I"], ["L", "i", "k", "e"], ["t","o"], ["t","i","c","k","l","e"]]
list2 = ["I like", "to tickle", "men in", "the arms"]

follows :: Ord sy => ProbSLG sy -> sy -> [(sy, Double)]
follows (ProbSLG (starting, ending, transitions)) sy = map (\(x, y, prob) -> (y, prob)) (filter p transitions)
                                                    where
                                                        p (fst, snd, prob) = fst == sy

precedes :: Ord sy => ProbSLG sy -> sy -> [(sy, Double)]
precedes (ProbSLG (starting, ending, transitions)) sy = map (\(x, y, prob) -> (x, prob)) (filter p transitions)
                                                    where
                                                        p (fst, snd, prob) = snd == sy

--input checking??
valP :: Ord sy => ProbSLG sy -> [sy] -> Double
valP (ProbSLG (starting, ending, transitions)) string = probStart * probTransitions * probEnd
                                                    where
                                                        start = head string
                                                        end = last string
                                                        filteredStarting = (filter (\(sy, prob) -> sy == start) starting)  --remove all pairs in starting set that dont contain sy as the symbol                                                
                                                        probStart = case start `elem` (map (\(sy, dbl) -> sy) filteredStarting) of False -> 0    -- if start is not in the starting set return zero
                                                                                                                                   True -> sum (map (\(sy, prob) -> prob) filteredStarting)  --otherwise, find and sum the associated probablilities and return dat                                         
                                                        filteredEnding = (filter (\(sy, prob) -> sy == end) ending)
                                                        probEnd = case end `elem` (map (\(sy, dbl) -> sy) filteredEnding) of False -> 0
                                                                                                                             True -> sum (map (\(sy, prob) -> prob) filteredEnding)
                                                        probTransitions = valP' (ProbSLG (starting, ending, transitions)) string--use valP' for this test this

valP' :: Ord sy => ProbSLG sy -> [sy] -> Double
valP' (ProbSLG (starting, ending, transitions)) string = probTransitions
                                                    where
                                                        bgrams = bigrams string
                                                        filteredTransitions = concat $ map (\(word1, word2) -> filter (\(wrd1, wrd2, prob) -> wrd1 == word1 && wrd2 == word2) transitions) bgrams
                                                        probabilities = map (\(x,y,dbl) -> dbl) filteredTransitions
                                                        probTransitions = product probabilities

-------------------------------------------------------------------------------
-- Problem 2:
-------------------------------------------------------------------------------

buildProbSLG :: Ord a => Corpus a -> ProbSLG a
buildProbSLG corpus = ProbSLG (starting, final, transitions)
                    where
                        starting = map (\(str, numOccurrences) -> (str, divide numOccurrences (length initStrings))) (frequencies initStrings)
                        final = map (\(str, numOccurrences) -> (str, divide numOccurrences (length finalStrings))) (frequencies finalStrings)
                        transitions = map (\((a,b), num) -> (a,b, divide num (numBigramsStartingWith a)) ) numberedBigrams


                        numBigramsStartingWith x = sum $ map (\(word, num) -> num) (filter (\((a,b), num) -> a == x) numberedBigrams)
                        numberedBigrams = frequencies $ concat $ map (\string -> bigrams string) corpus
                        initStrings = map (\arr -> head arr) corpus
                        finalStrings = map (\arr -> last arr) corpus
-- A potentially helpful starting point:
-- buildProbSLG corpus = ProbSLG ([], [], [])




-------------------------------------------------------------------------------
-- Problem 3:
-------------------------------------------------------------------------------

-- Add your sanitization functions to this list. Note that each function must
-- operate over sentences of tagged words.


sanitize :: [Sentence TaggedWord -> Sentence TaggedWord]
sanitize = []

posProbSLG :: Corpus TaggedWord -> ProbSLG String
posProbSLG taggedCorpus = buildProbSLG corpus
                        where
                            corpus = map (\string -> map (\(TaggedWord (word, pos)) -> pos) string ) taggedCorpus

tag :: Corpus TaggedWord -> String -> [(Sentence TaggedWord, Double)]
tag corpus str = output
            where
                output = map (\string -> (string, probabilityOfString string)) sentences --final output
                string = words str  --array of words
                taggedString = [concat $ map (\word -> [head $ findPOS word]) string] --find all pats of speech for each word in stgring

                sentences = nub $ concat $ map (\(TaggedWord (word, pos)) -> taggedString ++ [map (\(TaggedWord (wrd, ps)) -> if (wrd == word) then (changeTag (TaggedWord (wrd, ps)) pos) else ((TaggedWord (wrd, ps)))) (concat taggedString)]) partsOfSpeech --ouch

                partsOfSpeech = concat $ map (\word -> findPOS word) (words str)
                findPOS param = nub $ concat $ map (\string -> filter (\taggedWord -> getWord taggedWord == param) string) corpus
                posString str = map (\(TaggedWord (word, pos)) -> pos) str
                probabilityOfString string = valP probSLG (posString string)
                probSLG = posProbSLG corpus

tagBest :: Corpus TaggedWord -> String -> Sentence TaggedWord
tagBest corpus str = theMostProbableSentence
                where
                    theMostProbableSentence = fst $ head $ filter (\(sentence, prob) -> prob == mostProb) output
                    mostProb = maximum $ map (\(sentence, prob) -> prob) output
                    output = tag corpus str
