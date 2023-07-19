module Search.Search where

import Data.List
import Data.Maybe
import qualified Data.Map as Map (empty,insert,Map,member,mapAccumWithKey)
import Data.Tree
import System.Random
import Control.Monad

import Algebra.FField
import Algebra.PolyRings
import Algebra.NTT
import Algebra.Fourier
import Util.Logger
import Util.Util

---

data Path = Path Ring [Morphism] deriving (Show,Eq,Ord)
path_get_start :: Path -> Ring
path_get_start (Path start morphs) = start

path_get_steps :: Path -> Maybe [Kernel] 
path_get_steps (Path r (m:morphs)) = let new_r = apply m r in
  let m_new_steps = (new_r >>= (\m_r -> path_get_steps (Path m_r morphs))) in
    let m_ker_list = traverse id ([morphism_to_kernel m r]) in
      m_ker_list >>= (\ker_list -> pure (ker_list ++) <*> m_new_steps)
path_get_steps (Path start []) = Just []

path_get_morphs :: Path -> [Morphism]
path_get_morphs (Path r morphs) = morphs

path_get_end :: Path -> Maybe Ring
path_get_end (Path start (m:morphs)) = let new_start = apply m start in
  new_start >>= (\r -> (path_get_end (Path r morphs)))
path_get_end (Path cur []) = Just cur

path_get_states :: Path -> Maybe [Ring]
path_get_states (Path start morphs) = traverse id (scanl (\prev_state m -> prev_state >>= apply m) (Just start) morphs)

path_map :: (Morphism -> Ring -> Maybe b) -> Path -> Maybe [b]
path_map func (Path start morphs) = rec_map func start morphs
  where
    rec_map :: (Morphism -> Ring -> Maybe b) -> Ring -> [Morphism] -> Maybe [b]
    rec_map func cur (m:morphs) = do
          result <- func m cur
          next <- apply m cur
          rest <- rec_map func next morphs
          return ([result]++rest)
    rec_map func cur [] = Just []
    
path_define :: Path -> Maybe [LinearOp FF]
path_define path = path_map define_morphism path
  
buildPath :: Ring -> (Ring -> IO (Maybe Morphism)) -> IO Path
buildPath start f = buildPath_help start f start []
buildPath_help :: Ring -> (Ring -> IO (Maybe Morphism)) -> Ring -> [Morphism] -> IO Path
buildPath_help start f cur build = join (do {
  morph <- f cur; -- Maybe Morph
  recursive <- return (if morph == Nothing then Just (return (Path start build) :: IO Path) else (do { -- Maybe
                          m <- morph; -- Morph
                          new_cur <- apply m cur; -- Ring
                          return (buildPath_help start f new_cur (build++[m])) }));
  return (fromMaybe ((logObj "ERROR: SEARCH" "buildPath fail")>>(return (Path start build))) recursive); })

buildPath_random :: Ring -> StdGen -> (Ring -> StdGen -> IO (Maybe (Morphism,StdGen))) -> IO (Path,StdGen)
buildPath_random start rand f = buildPath_random_help start rand f start []
buildPath_random_help :: Ring -> StdGen -> (Ring -> StdGen -> IO (Maybe (Morphism, StdGen))) -> Ring -> [Morphism] -> IO (Path,StdGen)
buildPath_random_help start rand f cur build =
  join (do { -- IO
        morph_rand <- f cur rand; -- Maybe (Morph,StdGen)
        recursive <- return (if morph_rand == Nothing then Just (return (Path start build,rand) :: IO (Path,StdGen)) else (do { -- Maybe
                                                                                                  (morph,nrand) <- morph_rand; -- (Morph,StdGen)
                                                                                                    new_cur <- apply morph cur; -- Ring
                                                                                                    return (buildPath_random_help start nrand f new_cur (build++[morph]));  })); -- Maybe (IO (Path,StdGen))
        return (fromMaybe ((logObj "ERROR: SEARCH" "buildPath fail")>>(return (Path start build,rand))) (recursive)); }) -- if fails, returns path so far

   
appendPath :: Path -> Path -> Maybe Path
appendPath lhs rhs = do {
  lend <- path_get_end lhs;
  if lend == path_get_start rhs then Just (Path (path_get_start lhs) ((path_get_morphs lhs)++(path_get_morphs rhs))) else Nothing }

takePath :: Int -> Path -> Path
takePath i (Path s morphs) = let j=mod i (length morphs) in if j==0 then Path s morphs else Path s (take j morphs)

uniquePathChoices :: Path -> Path -> Int
uniquePathChoices p1 p2 = let common = commonStates p1 p2 in let uniq = do {
  choices <- foldl choiceFold (Just [(p1,p2)]) common; -- [(Path,Path)]
  uniqueC <- return (foldl (\prev (x,y) -> prev + (if x==y then 0 else 1)) 0 choices);
  return uniqueC } in
    fromMaybe (-1) uniq
  
combinePaths :: StdGen -> Path -> Path -> Maybe (Path,StdGen)
combinePaths rand p1 p2 = let common = commonStates p1 p2 in -- [Ring]
  do { -- Maybe
    choices <- foldl choiceFold (Just [(p1,p2)]) common; -- [(Path,Path)]
    r_choices <- return ( reverse choices ) ; -- [(Path,Path)]
    nPath <- return (foldl npFold (Just (Path (path_get_start p1) [],rand)) r_choices);
    nPath }
  where
    npFold :: Maybe (Path,StdGen) -> (Path,Path) -> Maybe (Path,StdGen)
    npFold (Just (prev,old_rand)) ctup = let (choice,new_rand) = randomChoice [fst ctup,snd ctup] old_rand in
      appendPath prev choice >>= (\x -> Just (x,new_rand))
    npFold Nothing _ = Nothing
    
choiceFold :: Maybe [(Path,Path)] ->  Ring -> Maybe [(Path,Path)]
choiceFold prev cur = do
   m_prev <- prev
   prev_head <- return . head $ m_prev
   q1 <- return . fst $ prev_head
   q2 <- return . snd $ prev_head
   (before_q1,after_q1) <- splitPathOnState q1 cur
   (before_q2,after_q2) <- splitPathOnState q2 cur
   return ([(after_q1,after_q2),(before_q1,before_q2)] ++ (tail m_prev))
                              

randomPath :: Ring -> StdGen -> IO (Path,StdGen)
randomPath start rand = buildPath_random start rand builder
  where
    builder :: Ring -> StdGen -> IO (Maybe (Morphism,StdGen))
    builder cur rand = do { --IO
      morphs <- morphismMatch >>= (\x -> match x cur); -- [Morph]
      return (if morphs == [] then Nothing else Just (randomChoice morphs rand)); }

combinePaths2 :: StdGen -> Path -> Path -> IO (Path,StdGen)
combinePaths2 rand p1 p2 = let (r1,r2) = split rand in
   let combinedMorphs  = (path_get_morphs p1) ++ (path_get_morphs p2) in
     --let shuffledMorphs = shuffle' combinedMorphs (length combinedMorphs) r1 in
     buildPath_random (path_get_start p1) rand (pathBuilder combinedMorphs)
     where
       pathBuilder :: [Morphism] -> Ring -> StdGen -> IO (Maybe (Morphism,StdGen))
       pathBuilder oldMorphs ring rand = do { -- IO
         morphs <- morphismMatch >>= (\x -> match x ring); -- [Morph] 
         overlapping <- return (intersectBy (\m1 m2 -> is_par_morph (morph_get_inner m1) m2) morphs oldMorphs);
         choices <- return (if overlapping == [] then morphs else overlapping);
         return (if choices == [] then Nothing else Just (randomChoice choices rand)); }


commonStates :: Path -> Path -> [Ring]
commonStates p1 p2 = let p1_states = path_get_states p1 in
  let p2_states = path_get_states p2 in
    fromMaybe [] (traverse id [p1_states,p2_states] >>= (\x -> Just (intersect (head x) (last x))))

splitPathOnState :: Path -> Ring -> Maybe (Path,Path)
splitPathOnState (Path start morphs) split = helper start [] start morphs split
  where
    helper :: Ring -> [Morphism] -> Ring -> [Morphism] -> Ring -> Maybe (Path,Path)
    helper start before current [] split = if current == split then Just (Path start before, Path split []) else Nothing
    helper start before current after split = if current == split then Just (Path start before,Path split after) else
      let m = head after in
        (apply m current) >>= (\next -> helper start (before++[m]) next (tail after) split)
                                               

---

buildPathForest :: Ring -> IO [Tree (Kernel)]
buildPathForest start = do { -- IO
  mm <- morphismMatch; -- Match
  morphs <- match mm start; -- [Morphs]
  initial <- return [(start,mi) | mi <- morphs]; -- [(Ring,Morphism)]
  unfoldForestM buildPathForest_branch initial; } -- IO [Tree (Kernel)]
  
-- 
buildPathForest_branch :: (Ring,Morphism) -> IO (Kernel, [(Ring, Morphism)])
buildPathForest_branch (r,m) = do { -- IO
  mm <- morphismMatch;
  morphs <- match mm r;
  r2 <- maybeToIO ("buildPathForest calling apply"++show (m,r)) (apply m r);
  k <- maybeToIO "buildPathForest calling morphism_to_kernel" (morphism_to_kernel m r);
  return (k,[(r2,mi) | mi <- morphs ]) }

--


buildRingTree :: Ring -> IO (Tree Ring)
buildRingTree start = unfoldTreeM buildRingTree_branch start

buildRingTree_branch :: Ring -> IO (Ring,[Ring])
buildRingTree_branch r = do { --IO
  mm <- morphismMatch; --Match
  morphs <- match mm r; -- [Morphs]
  rings <- maybeToIO "buildRingTree calling apply" (sequence (fmap (\m -> apply m r) morphs));
  return (r,rings); }

--buildMorphTree :: Ring -> IO [(Tree Morph)]
--buildMorphTree start = do
--  mm <- morphismMatch :: IO Match
--  morphs <- match mm start :: IO [Morphism]
--  initial <- return [(start,m) | m<- morphs] :: IO [(Ring,Morphism)]
--  unfoldForestM buildMorphTree 
                                                       
--(r,foldl (++) [] (fmap maybeToList (fmap (\m -> apply m r) (match matchMorphism r))))

--
----
--
----randomWalk :: RandomGen b => [Tree (Maybe Kernel)] -> b -> (Maybe [Kernel],b)
--randomWalk :: (RandomGen b, Eq a) => [Tree (Maybe a)] -> b -> (Maybe [a],b)
--randomWalk forest g = let (index,new_g) = randomR (0,(length forest)-1) g in
--  let tree = forest !! index in
--    let (walk,new_new_g) = randomWalk_help ([],new_g) tree in
--      (traverse id walk,new_new_g)
----    (traverse id (fst (randomWalk_help ([],new_g) tree))
--                   
--randomWalk_help :: (RandomGen b, Eq a) => ([a],b) -> Tree a -> ([a],b)
--randomWalk_help (walk,g) (Node c rest) = if rest == [] then (walk++[c],g) else
--  let (index,new_g) = randomR (0,(length rest)-1) g in
--    let new_rest = rest !! index  in -- this may be a bad idea
--        randomWalk_help (walk++[c],new_g) new_rest
--


           
strTree :: Show a => Tree a -> String
strTree (Node x rest) = show x ++ (foldr (++) "" [ (strTree r) | r <- rest] )

--

-- rewrite turtles to work with paths
turtles :: Ring -> Morphism -> IO Path
turtles start turtle = let findTurtle ring = do { -- IO
                             mm <- morphismMatch; -- Match
                             uf_morphs <- match mm ring; -- [Morph]
                             morphs <- return (filter (\m -> is_par_morph turtle m) uf_morphs); -- [Morph]
                             return (if morphs == [] then Nothing else Just (head morphs)); } in -- Ring -> IO (Maybe (Morph))
                         buildPath start findTurtle

turtlesExtend :: Path -> Morphism -> IO Path
turtlesExtend p1 turtle = do {
  p1_end <- maybeToIO "turtlesExtend calling path_get_end" (path_get_end p1);
  p2 <- turtles p1_end turtle;
  maybeToIO "turtlesExtend calling appendPath" (appendPath p1 p2); }

randomSample :: Ring -> StdGen -> Int -> IO [Path]
randomSample ring rand size = 
  do
    plist <- sequence (scanl scanf (randomPath ring rand) [0..(size-2)])
    return (fmap fst plist)
  where
    scanf :: IO (Path,StdGen) -> Int -> IO (Path,StdGen)
    scanf x_rand y = do {
      (x,rand) <- x_rand;
      randomPath (path_get_start x) rand }
 
--turtlesAWD :: Ring -> Morphism -> Maybe [Kernel] -> Maybe [Kernel]
--turtlesAWD cur turtle path = let morphs = (filter (\m -> is_par_morph turtle m) (match matchMorphism cur)) in
--  if morphs == [] then path else
--    let my_morph = head morphs in do {
--      new_ring <- apply my_morph cur;
--      uw_path <- path;
--      uw_ker <- traverse id [(morphism_to_kernel my_morph cur)];
--      turtlesAWD new_ring turtle (Just (uw_path ++ uw_ker))
--      }
--      

-- UTILITY

--allPaths :: Ring -> [Kernel]
--allPaths start = let pf = buildPathForest start in
--  foldr 

--buildTree_help :: Ring -> [Kernel]
--buildTree_help r = 


--search_paths :: Ring -> Map.Map Ring [Morphism] -> [[Kernel]]
--search_paths cur hmap = let mmorphs = Map.lookup cur hmap in
--  let m_ker_neighbors = fmap (\m -> (morphism_to_kernel cur m , apply m cur) 
--  let neighbors = foldl (++) [] (fmap maybToList (fmap (\m -> apply m current) (Map.lookup cur hmap))) in
--  let paths = fmap search_paths 
--1

--rec_expand :: [Ring] -> [Ring]
--rec_expand lmr = nub (foldr (++) [] [[x] >>= expand | x <- lmr])
--
----
--n_expand :: Int -> [Ring] -> [Ring]
--n_expand n lr | n>0 = n_expand (n-1) (rec_expand lr)
--             | n<=0 = lr
--
----
