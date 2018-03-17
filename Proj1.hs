module Proj1 (initialGuess, nextGuess, GameState) where
import Debug.Trace
import Data.List
import Data.Ord
import System.Environment
import System.Exit

--GameState helps to store the list of string like A1,B1,C1 with its score when compared to 
--target.
type GameState = [([String], Int ,Int ,Int)]



--i always give my first guess “B1”,C2”,”G3” with score 0,0,0.
initialGuess :: ([String],GameState)
initialGuess = (["A1","D2","F3"],[ ([],0,0,0)]) 

--it gets the output of filtered list which gets score match each time from previous guess kept in game state.
--finally it again filters the list which match the score with last move which is not kept in game state and gives the head
--of filtered list as previous string cant come in it as it has to completely match with it and score never goes 3 in a.
nextGuess :: ([String],GameState) ->(Int,Int,Int) ->([String],GameState)
nextGuess (xs ,state)(a,b,c)= ( mylist!!getElement(storeIntValue mylist 0),newGameState:state)
                              where newGameState=(xs,a,b,c)
                                    filteroutput=removeList state
                                    mylist=[zs |zs<-filteroutput,(guessAnswer xs zs)==(a,b,c)]


--it gets the minimum element index from string from array of int.
getElement::[Int]->Int
getElement xs =  getIndex(minimumBy (comparing fst) (zip xs [0..]))


--it gets second element of tuple.
getIndex::(Int,Int)->Int
getIndex (x,y)=  y



--give length of list of list of strings.
giveLength::[[String]]->Int
giveLength xs=length xs    



--it helps to filter the list from previous game state by removing the elements with same pitch of strings compared to each and every
--combination starting from right most as it got first into game state. and reusing the filtered list to cut down elements.
removeList:: GameState->[[String]]
removeList gameState=foldr mineGuess [[]] gameState


--need to return  a head of tail of list of list of string generated from mineguess.
--returns list of list of string with making xs a new target and getting all values same which has same pitch to reduce our guess 
mineGuess::([String],Int,Int,Int)->[[String]]->[[String]]
mineGuess  ([] ,0,0,0) [[] ] =[ ys |ys<-getCombinations]
mineGuess  (xs ,a,b,c) previousOutput =
                        [ys |ys<-previousOutput,(guessAnswer xs ys)==(a,b,c)]




----it gets the list of of all guess scores by comibining with each possible combination in 
--filterd list. variable n helps to keep track of index of list which signifies the possible
--guess.
storeIntValue ::[[String]]->Int->[Int]
storeIntValue  zs n =
                            if n < (length zs) then               
                                generateScoreTable([(guessAnswer (zs!!n) y)|y<-zs]):
                                storeIntValue zs (n+1)    
                            else []





--gets the frequency of each element in list
--with help of getScoreInt, it  genrates the score by making first elemnt as target.
generateScoreTable::[(Int,Int,Int)]->Int
generateScoreTable xs = getScoreInt([ (c,  (length $ filter (== c) xs))  
                         | c <- nub xs])



--it gets the score of list of scores  with frequency of it.
--e.g:  if (0,1,2),4  score is occuring 4 times. so it gives 16+ rest of scores to determine
--next best guess.
getScoreInt::[((Int,Int,Int),Int)]->Int
getScoreInt [((a,b,c),d)]= d*d
getScoreInt (((a,b,c),d):ys)= d*d +  getScoreInt ys
         



--generated all possible combinations in  sorted order .
getCombinations::[[String]]
getCombinations=totalGuess generatePair



--generated all 7 different pair of string and number
generatePair::[String]
generatePair =[i++j| i<-["A","B","C","D","E","F","G"] , j<-["1","2","3"]]


--need to generate all different 3 pairs from given 21 combinations in
--selecting 21combination3 ways ==1330 
totalGuess::[String]->[[String]]
totalGuess generatePair =[[x,y,z]|x<-generatePair,
            y<-tail generatePair,
            z<-tail(tail generatePair),x<y,y<z]




--guessanswer helps to determine the score of target and guess by removing duplicated from string and calculating intersection
--foloowed by length of unique elements left in guess and target that match.
--nub helps to remove the duplicated and giving first match in order.
guessAnswer::[String]->[String]->(Int,Int,Int)
guessAnswer target guess=(pitch,rightNote,rightOctave)
  where guess'      = nub guess
        pitch       = length ( intersect guess' target)
        num         = length guess'
        rightNote   = num - (length $ deleteFirstsBy (mineeqNth 0) guess' target) 
                    - pitch
        rightOctave = num - (length $ deleteFirstsBy (mineeqNth 1) guess' target) 
                    - pitch

        

-- | eqNth n l1 l2 returns True iff element n of l1 is equal to 
--   element n of l2.
mineeqNth :: Eq a => Int -> [a] -> [a] -> Bool
mineeqNth n l1 l2 = (l1 !! n) == (l2 !! n)




