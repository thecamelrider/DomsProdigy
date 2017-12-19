module Doms2 where

 {- play a 5's & 3's round between 2 given players
    2017-18 assignment 2
 -}
 
 import Doms1
 import System.Random
 import Data.List
 -- import Debug.Trace
 -- import Mergesort
 
 {- Variables
  P,p dom player
  h hand
  b board
  d domino
 -}
 ------------------------------------------------------------------------------
 -- DATA STRUCTURES
 
 -- player names - P1 drops first
 data Player = P1|P2 -- player 1 or player 2
                  deriving (Eq,Show)
 -- the game scores for P1 and P2
 type Scores = (Int,Int)
 
  -- state of a game
 type GameState =(Hand,Hand,Player,Board,Scores)
               
                  
 {- DomsPlayer
    given a Hand, and a Board, returns a Dom and an End
    only called when player is not knocking
    make this a type, so different players can be created
 -}
 
 type DomsPlayer = Hand->Board->(Dom,End)
 


 -------------------------------------------------------------------------------
 -- DOMSPLAYERS
 
 -- naivePlayer plays the first legal dom it can, prefering to play left
 
 naivePlayer :: DomsPlayer
 
 naivePlayer h b  
  |not(null ldrops) = ((head ldrops),L)
  |otherwise = ((head rdrops),R)
  where
   ldrops = leftdrops h b
   rdrops = rightdrops h b
 
 -- hsdplayer plays highest scoring dom
 
 {-
 -- let version .. have to use if then else
 
 hsdPlayer h b =
    let 
     (ldrops, rdrops) = possPlays h b 
     lscores = map scoreBoard (map (\ d -> playLeft d b) ldrops) -- scores for each ldrop
     (ld,ls) = foldr (\ (d,s) (md,ms)-> (if (s>ms) then (d,s) else (md,ms))) -- fold for max score
                     ((7,7),-1) -- dummy pair as fold base 
                     (zip ldrops lscores) -- fold over zip of doms & their scores
     -- similar for right drops                
     rscores = map scoreBoard (map (\ d -> playRight d b) rdrops)
     (rd,rs) = foldr (\ (d,s) (md,ms)-> (if (s>ms) then (d,s) else (md,ms)))
                  ((7,7),-1)
                  (zip rdrops rscores)
    in
     if (ls>rs) then (ld,L) else (rd,R) -- return max score
                
 -- where version .. no if then else .. preferable
 
 hsdPlayer h b
  |ls>rs = (ld,L)
  |otherwise = (rd,R)
  where
   (ldrops, rdrops) = possPlays h b 
   lscores = map scoreBoard (map (\ d -> playLeft d b) ldrops) -- scores for each ldrop
   (ld,ls) = foldr (\ (d,s) (md,ms)-> (if (s>ms) then (d,s) else (md,ms))) -- fold for max score
                     ((7,7),-1) -- dummy pair as fold base 
                     (zip ldrops lscores) -- fold over zip of doms & their scores
   -- similar for right drops                
   rscores = map scoreBoard (map (\ d -> playRight d b) rdrops)
   (rd,rs) = foldr (\ (d,s) (md,ms)-> (if (s>ms) then (d,s) else (md,ms)))
                  ((7,7),-1)
                  (zip rdrops rscores)
 -}
 
 -- hsdPlayer using comprehensions & comparing
                   
 hsdPlayer h b = (d,e)
  where
  (lPlays,rPlays) = possPlays h b
  allPlays = [( scoreBoard ( playLeft d b), (d,L))|d<-lPlays]++   -- a play is (score, (dom,end))      
              [( scoreBoard ( playRight d b) ,(d,R))|d<-rPlays]
  (_,(d,e)) = maximumBy (comparing fst) allPlays
  
 -- need comparing
 -- useful fn specifying what we want to compare by
 comparing :: Ord b=>(a->b)->a->a->Ordering
 comparing f l r = compare (f l) (f r)
 
 
 
----------------------------------------------------------------------------------------- 
 -- playDomsRound
 -- given 2 players and a seed for random no generation
 
 playDomsRound :: DomsPlayer->DomsPlayer->Int->Scores
 
 playDomsRound p1 p2 seed = 
  let 
   stdgen=mkStdGen seed
   spack = shuffleDoms stdgen -- shuffle the pack
   p1_hand = take 9 spack -- deal the hands
   p2_hand = take 9 (drop 9 spack)
   init_gamestate = (p1_hand,p2_hand,P1,[],(0,0)) -- initial state
  in
   playDoms p1 p2 init_gamestate -- auxilliary given which player drops, inital scores, initial StdGen
 
------------------------------------------------------------------------------------------   
 -- shuffle the doms
 
 shuffleDoms :: StdGen -> [Dom]

 shuffleDoms gen =  
  let
    weights = take 28 (randoms gen :: [Int])
    dset = (map fst (sortBy  
               (\ (_,w1)(_,w2)  -> (compare w1 w2)) 
               (zip domSet weights)))
  in
   dset
 ------------------------------------------------------------------------------------------
 -- playDoms runs the hand
 -- recurses for each drop
 -- returns scores at the end

 playDoms :: DomsPlayer->DomsPlayer->GameState->(Int,Int)
 
 playDoms p1 p2 gs@(h1,h2,nextplayer,b,scores)
  |(kp1 &&  kp2) = scores -- both players knocking, end of the hand
  |((nextplayer==P1) && (not kp1)) =  playDoms p1 p2 (p1play p1 gs) -- p1 plays, returning new gameState. p2 to go next
  |(nextplayer==P1) = playDoms p1 p2 (p2play p2 gs) -- p1 knocking so p2 plays
  |(not kp2) = playDoms p1 p2 (p2play p2 gs) -- p2 plays
  |otherwise = playDoms p1 p2 (p1play p1 gs) -- p2 knocking so p1 plays
  where
   kp1 = (knockingP h1 b) -- true if p1 knocking
   -- kp1 = trace (show gs) (knockingP h1 b) -- true if p1 knocking
   kp2 = knockingP h2 b -- true if p2 knocking
  --trace (h1++" "++h2++" "++nextplayer++" "++b++" "++ (fst scores)++" "++(snd scores)) 
 ------------------------------------------------------------------------------------------
 
 -- player p1 to drop
 -- return resulting game state
 
 p1play :: DomsPlayer->GameState->GameState
 
 p1play p1 (h1,h2,_,b, (s1,s2)) = 
  ((deleteDom dom h1), h2, P2,nb,(ns1, s2))
   where
    (dom,end) = p1 h1 b -- call the player, returning dom dropped and end it's dropped at.
    (Just nb)=playDom dom end b -- play the dom - it will go
    ns1 = s1+ (scoreBoard nb) -- what it scored
    
    
 
 -- p2 to drop
   
 p2play :: DomsPlayer->GameState->GameState
 
 p2play p2 (h1,h2,_,b,(s1,s2)) = 
  (h1, (deleteDom dom h2),P1, nb, (s1, ns2))
  where
   (dom,end) = p2 h2 b -- call the player, returning dom dropped and end it's dropped at.
   (Just nb)=playDom dom end b -- play the dom - it will go
   ns2 = s2+ (scoreBoard nb)
 
-------------------------------------------------------------------------------------------
 -- deleteDom
 -- delete a Dom from a hand
 
 deleteDom :: Dom->Hand->Hand
 
 deleteDom d h = filter (\dd -> not (sameDomP dd d)) h
 
