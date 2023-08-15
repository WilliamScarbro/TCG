{-# LANGUAGE FlexibleInstances #-}
module Search.EquivalenceLibrary where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad
import Data.List (permutations,nub)
import Data.Maybe (maybeToList,isJust)
import System.Random (randomRIO)
import Control.Exception

import Search.HillClimbing
import Search.Annealing
import Search.Search
import Algebra.FField
import Algebra.PolyRings
import Algebra.NTT
import Algebra.Fourier
import Util.Logger
import Util.Util
import Compile.Compilers
import Compile.OptimizeIR
import Search.Decompose (find_replacements)

-- Symbolic Floating Morphism
data SymbolicMorphism =
  SymExtend VarProd SymbolicMorphism |
  SymRepeat VarProd SymbolicMorphism |
  SymFactor VarProd |
  SymLabel VarProd |
  SymDefine |
  SymNorm |
  SymSwapQQ |
  SymSwapQP |
  SymSwapJoinProd |
  SymJoinProd
  deriving (Show,Ord)

eq_helper :: SymVariableMap -> SymbolicMorphism -> SymbolicMorphism -> [SymVariableMap]
eq_helper var_map (SymExtend vp1 sm1) (SymExtend vp2 sm2) | length vp1 /= length vp2 = []
                                                          | otherwise =
  join ( do -- []
           new_var_map <- matchVariables vp1 vp2 var_map :: [SymVariableMap]
           return ( eq_helper new_var_map sm1 sm2 ) :: [[SymVariableMap]]
       )
eq_helper var_map (SymRepeat vp1 sm1) (SymRepeat vp2 sm2) | length vp1 /= length vp2 = []
                                                          | otherwise =
  join ( do -- []
           new_var_map <- matchVariables vp1 vp2 var_map :: [SymVariableMap]
           return ( eq_helper new_var_map sm1 sm2 )
       )
eq_helper var_map (SymFactor vp1) (SymFactor vp2) | length vp1 /= length vp2 = []
                                                  | otherwise = matchVariables vp1 vp2 var_map
eq_helper var_map (SymLabel vp1) (SymLabel vp2) | length vp1 /= length vp2 = []
                                                | otherwise = matchVariables vp1 vp2 var_map
eq_helper var_map SymDefine SymDefine = return var_map
eq_helper var_map SymNorm SymNorm = return var_map
eq_helper var_map SymSwapQQ SymSwapQQ = return var_map
eq_helper var_map SymSwapQP SymSwapQP = return var_map
eq_helper var_map SymSwapJoinProd SymSwapJoinProd = return var_map
eq_helper var_map SymJoinProd SymJoinProd = return var_map
eq_helper _ _ _ = []

instance Eq SymbolicMorphism where
  (==) sm1 sm2 = length (eq_helper Map.empty sm1 sm2) >= 1
    
type SymVariableMap = Map.Map String String

matchVariables :: [String] -> [String] -> SymVariableMap -> [SymVariableMap]
matchVariables strs varNames variableMap 
    | null unmappedStrs = [variableMap]
    | otherwise = nub $ generateSymVarMaps unmappedStrs unmappedVars variableMap
  where
    mappedVars = Map.intersection variableMap (Map.fromList [(v, "") | v <- varNames])
    unmappedVars = filter (\v -> not $ Map.member v mappedVars) varNames
    unmappedStrs = filter (\s -> not $ Map.member s mappedVars) strs

generateSymVarMaps :: [String] -> [String] -> SymVariableMap -> [SymVariableMap]
generateSymVarMaps strs varNames variableMap = join [generateNewSymVarMap sGroup varNames variableMap | sGroup <- permutations strs]

generateNewSymVarMap :: [String] -> [String] -> SymVariableMap -> [SymVariableMap]
generateNewSymVarMap strs varNames variableMap = [Map.union (Map.fromList $ zip varNames perm) variableMap | perm <- permutations strs]
      
type VarProd = [String]

-- data Morphism =
--   MInverse Morphism |
--   Extend Int Morphism |
--   Repeat Int Morphism |
--   Factor Int |
--   Label Int |
--   Norm |
--   Define |
--   SwapQQ | SwapQP | 
--   JoinProd | SwapJoinProd |
--   IdR
--   deriving (Show,Eq,Ord)


type FactorMap = Map.Map Int String

_get_factor_names :: FactorMap -> Int -> IO [String]
_get_factor_names factor_map n =
  let
    factors = prime_factors n :: [Int]
    lookup :: Int -> IO String
    lookup factor = maybeToIO ("_get_factor_names: missing factor "++(show factor)) (Map.lookup factor factor_map) :: IO String
  in
    sequence (fmap lookup factors)



type VariableMap = Map.Map String Int

_to_variable_map :: FactorMap -> VariableMap
_to_variable_map factor_map = Map.fromList (fmap (\(a,b) -> (b,a)) (Map.toList factor_map))

_to_factor_map :: VariableMap -> FactorMap
_to_factor_map variable_map = Map.fromList (fmap (\(a,b) -> (b,a)) (Map.toList variable_map))

matchFactors :: Int -> [String] -> VariableMap -> [VariableMap]
matchFactors n varNames variableMap
  | n `mod` product (Map.elems mappedVars) /= 0 = []
  | otherwise = generateVarMaps unmappedN unmappedVars variableMap
  where
    mappedVars = Map.intersection variableMap (Map.fromList [(v, 0) | v <- varNames])
    unmappedVars = filter (\v -> not $ Map.member v mappedVars) varNames
    unmappedN = foldl (\acc v -> acc `div` fromMaybe 1 (Map.lookup v mappedVars)) n (Map.keys mappedVars)

generateVarMaps :: Int -> [String] -> VariableMap -> [VariableMap]
generateVarMaps n varNames variableMap | null varNames && n == 1 = return variableMap
                                       | otherwise = join [ generateNewVarMap factorGroup varNames variableMap | factorGroup <- generateFactorizations n (length varNames) ]

generateNewVarMap :: [Int] -> [String] -> VariableMap -> [VariableMap]
generateNewVarMap factors varNames variableMap = 
  [ Map.union (Map.fromList $ zip varNames perm) variableMap | perm <- permutations factors ]
    
-- combinations :: Int -> [a] -> [[a]]
-- combinations 0 _      = [[]]
-- combinations _ []     = []
-- combinations k (x:xs) = x_start ++ others
--   where x_start = [ x : rest | rest <- combinations (k-1) xs ]
--         others  = if k <= length xs then combinations k xs else []

-- getFactorGroups :: Int -> Int -> [[Int]]
-- getFactorGroups l n = do
--     combo <- combinations l $ non_triv_factors n
--     guard $ product combo == n
--     return combo


--


cannon_factor_map = Map.fromList [(2,"a"),(3,"b"),(5,"c"),(7,"d")] :: Map.Map Int String
cannon_variable_map = _to_variable_map cannon_factor_map

--

symbolize_morphism :: FactorMap -> Morphism -> IO SymbolicMorphism
symbolize_morphism factor_map (Extend n m) =
  do
    factor_names <- _get_factor_names factor_map n
    sm <- symbolize_morphism factor_map m
    return (SymExtend factor_names sm)
symbolize_morphism factor_map (Repeat n m) =
  do
    factor_names <- _get_factor_names factor_map n
    sm <- symbolize_morphism factor_map m
    return (SymRepeat factor_names sm)
symbolize_morphism factor_map (Factor n) =
  do
    factor_names <- _get_factor_names factor_map n
    return (SymFactor factor_names)
symbolize_morphism factor_map (Label n) =
  do
    factor_names <- _get_factor_names factor_map n
    return (SymLabel factor_names)
symbolize_morphism factor_map Norm = return SymNorm
symbolize_morphism factor_map Define = return SymDefine
symbolize_morphism factor_map SwapQQ = return SymSwapQQ
symbolize_morphism factor_map SwapQP = return SymSwapQP
symbolize_morphism factor_map JoinProd = return SymJoinProd
symbolize_morphism factor_map SwapJoinProd = return SymSwapJoinProd


unsymbolize_morphism :: VariableMap -> SymbolicMorphism -> Maybe Morphism
unsymbolize_morphism variable_map (SymExtend var_prod m) =
  do
    prod <- _multiply_variables variable_map var_prod
    usm <- unsymbolize_morphism variable_map m
    return (Extend prod usm)
unsymbolize_morphism variable_map (SymRepeat var_prod m) =
  do
    prod <- _multiply_variables variable_map var_prod
    usm <- unsymbolize_morphism variable_map m
    return (Repeat prod usm)
unsymbolize_morphism variable_map (SymFactor var_prod) =
  do
    prod <- _multiply_variables variable_map var_prod
    return (Factor prod)
unsymbolize_morphism variable_map (SymLabel var_prod) =
  do
    prod <- _multiply_variables variable_map var_prod
    return (Label prod)
unsymbolize_morphism variable_map SymNorm = return Norm
unsymbolize_morphism variable_map SymDefine = return Define
unsymbolize_morphism variable_map SymSwapQQ = return SwapQQ
unsymbolize_morphism variable_map SymSwapQP = return SwapQP
unsymbolize_morphism variable_map SymJoinProd = return JoinProd
unsymbolize_morphism variable_map SymSwapJoinProd = return SwapJoinProd

_multiply_variables :: VariableMap -> [String] -> Maybe Int
_multiply_variables variable_map var_list =
  do
    --var_vals <- sequence (fmap (\v -> maybeToIO ("_multiply_variables: missing variable name"++v) $ Map.lookup v variable_map) var_list)
    var_vals <- sequence (fmap (\v -> Map.lookup v variable_map) var_list)
    return (foldr (*) 1 var_vals)


    
match_symbolic_morphism :: Morphism -> SymbolicMorphism -> [VariableMap]
match_symbolic_morphism m sm = msm_help Map.empty m sm
  
msm_help :: VariableMap -> Morphism -> SymbolicMorphism -> [VariableMap]
msm_help variable_map (Extend n m) (SymExtend var_list sm) =
  join (do -- List
           new_variable_map <- matchFactors n var_list variable_map :: [VariableMap]
           return (msm_help new_variable_map m sm) :: [[VariableMap]]
       )
msm_help variable_map (Repeat n m) (SymRepeat var_list sm) =
  join (do -- List
           new_variable_map <- matchFactors n var_list variable_map
           return (msm_help new_variable_map m sm)
       )
msm_help variable_map (Factor n) (SymFactor var_list) = matchFactors n var_list variable_map
msm_help variable_map (Label n) (SymLabel var_list) = matchFactors n var_list variable_map
msm_help variable_map Norm SymNorm = return variable_map
msm_help variable_map Define SymDefine = return variable_map
msm_help variable_map SwapQQ SymSwapQQ = return variable_map
msm_help variable_map SwapQP SymSwapQP = return variable_map
msm_help variable_map JoinProd SymJoinProd = return variable_map
msm_help variable_map SwapJoinProd SymSwapJoinProd = return variable_map
msm_help _ _ _ = []


-- only matches if all morphs match symmorphs using same variable map
match_symbolic_identity :: [Morphism] -> [SymbolicMorphism] -> [VariableMap]
match_symbolic_identity morphs symmorphs = msi_help Map.empty morphs symmorphs
  where
    msi_help :: VariableMap -> [Morphism] -> [SymbolicMorphism] -> [VariableMap]
    msi_help variable_map [] [] = [variable_map]
    msi_help variable_map (m:morphs) (sm:symmorphs) =
      join (do -- List
               new_factor_map <- msm_help variable_map m sm
               return (msi_help new_factor_map morphs symmorphs)
           )

match_symbolic_identity_autofunctor :: [Morphism] -> [SymbolicMorphism] -> [(Morphism -> Morphism, Morphism -> Maybe Morphism, VariableMap)]
match_symbolic_identity_autofunctor morphs symmorphs =
  let
    unwrapped_maps = join ( maybeToList (
        do -- Maybe
          (wrap_func,unwrap_func,uw_morphs) <- unwrap_morph_list morphs
          let variable_maps = match_symbolic_identity_autofunctor uw_morphs symmorphs
          return [(wrap_func . wrap_func2, (\m -> unwrap_func m >>= unwrap_func2),vm) | (wrap_func2,unwrap_func2,vm) <- variable_maps]
      ))
    inplace_maps = [ (id,return,vm) | vm <- match_symbolic_identity morphs symmorphs ]
  in
    inplace_maps ++ unwrapped_maps
  where
    unwrap_morph_list :: [Morphism] -> Maybe (Morphism -> Morphism, Morphism -> Maybe Morphism,[Morphism])
    unwrap_morph_list ((Extend n m):morphs) =
      do
          unwrapped_morphs <- iterate_unwrap_extend n morphs
          let unwrap_func (Extend en m) | en == n = Just m
                                        | otherwise = Nothing
              unwrap_func _ = Nothing
              wrap_func m = Extend n m
          return (wrap_func,unwrap_func,m:unwrapped_morphs)  
    unwrap_morph_list ((Repeat n m):morphs) =
      do
          unwrapped_morphs <- iterate_unwrap_repeat n morphs
          let unwrap_func (Repeat en m) | en == n = Just m
                                        | otherwise = Nothing
              unwrap_func _ = Nothing
              wrap_func m = Repeat n m
          return (wrap_func,unwrap_func,m:unwrapped_morphs)
    unwrap_morph_list _ = Nothing

    iterate_unwrap_extend n1 ((Extend n2 m):morphs) | n1 == n2 = iterate_unwrap_extend n1 morphs >>= (\mlist -> Just (m:mlist))
                                                    | otherwise = Nothing
    iterate_unwrap_extend n (_:morphs) = Nothing 
    iterate_unwrap_extend _ [] = Just []

    iterate_unwrap_repeat n1 ((Repeat n2 m):morphs) | n1 == n2 = iterate_unwrap_repeat n1 morphs >>= (\mlist -> Just (m:mlist))
                                                  | otherwise = Nothing
    iterate_unwrap_repeat n (_:morphs) = Nothing 
    iterate_unwrap_repeat _ [] = Just []


type EquivalenceLibrary = [[[SymbolicMorphism]]]

-- hardcoded with initial ring
buildEquivalenceLibrary :: Int -> IO EquivalenceLibrary
buildEquivalenceLibrary search_depth =
  let
    --initial_ring = Base 30 0 30 31
    initial_ring = Base 6 0 6 7
  in
  do -- IO
    all_reachable <- allReachableRings initial_ring :: IO [Ring]
    let all_pairs = (filter (\(a,b) -> a /= b && prod_dimension a <= prod_dimension b)) . join $ [[(r1,r2) | r1 <- all_reachable ] | r2 <- all_reachable ] :: [(Ring,Ring)]
    logObj "buildEL: finished finding reachable (total,valid_pairs)" (length all_pairs,length all_reachable)
    nonsym_classes <- sequence $ do -- List
          (start,end) <- all_pairs :: [(Ring,Ring)]
          return $ do -- IO
            replacements <- find_replacements search_depth start end :: IO [[Morphism]]
            logObj "buildEL: found replacements" (start,end,length replacements)
            return replacements
    lib <- sequence $ fmap (sequence . fmap (sequence . fmap (symbolize_morphism cannon_factor_map))) nonsym_classes
    equiv_lib <- return . nub . (filter (\x -> length x > 1)) $ lib
    return equiv_lib

    --sequence . sequence . sequence $ (fmap . fmap . fmap $ symbolize_morphism cannon_factor_map) $ nonsym_classes :: IO [[[SymbolicMorphism]]] 

    
prettyPrintELib :: EquivalenceLibrary -> String
prettyPrintELib elib =
  foldr (++) "" [show eclass++"\n" | eclass <- (filter (\x -> length x > 1) elib)]

apply_identity :: [[SymbolicMorphism]] -> [Morphism] -> [[Morphism]]
apply_identity identity morphs =
  let
    possible_matches = filter (\sm_list -> length sm_list == length morphs) identity :: [[SymbolicMorphism]]
  in
    nub $ do -- List
      (mfunc,_,var_map) <- get_first_match morphs possible_matches :: [(Morphism->Morphism,Morphism->Maybe Morphism,VariableMap)]
      nub $ do
        mlist <- identity :: [[SymbolicMorphism]]
        maybeToList (sequence (fmap (\m -> unsymbolize_morphism var_map m >>= return . mfunc ) mlist)) :: [[Morphism]]
  where
    get_first_match :: [Morphism] -> [[SymbolicMorphism]] -> [(Morphism->Morphism,Morphism->Maybe Morphism,VariableMap)]
    get_first_match morphs (pm:possible_matches) =
      let
        representations = match_symbolic_identity_autofunctor morphs pm
      in
        if null representations then
          get_first_match morphs possible_matches
        else
          representations
    get_first_match _ [] = []
    
get_equiv_morphs :: EquivalenceLibrary -> [Morphism] -> [[Morphism]]
get_equiv_morphs elib morphs =
  nub . join  $ do -- []
    identity <- elib :: [[[SymbolicMorphism]]]
    return (apply_identity identity morphs) :: [[[Morphism]]]

equivalence_expansion :: EquivalenceLibrary -> Int -> Int -> Path -> IO [Path]
equivalence_expansion elib slice_len sample_size current_path =
  let
    morphs = path_get_morphs current_path
    start = path_get_start current_path
    slices = get_slices slice_len (length morphs +1) :: [(Int,Int)]
  in
    do
      sample_slices <- takeRandom sample_size slices :: IO [(Int,Int)]
      alt_morphs <- return (join (fmap (\slice -> get_alt_slice elib slice morphs) sample_slices)) :: IO [[Morphism]]
      alt_paths <- return (nub (fmap (\morphs -> Path start morphs) alt_morphs)) :: IO [Path]
      legal_alt_paths <- return (filter check_legal_path alt_paths) :: IO [Path]
      logObj "equivalence expansion: slice_len, sample_size, all_paths, legal_paths" (slice_len,sample_size,length alt_paths,length legal_alt_paths)-- filter (not . check_legal_path) alt_paths)
      if null legal_alt_paths then
        return [current_path]
        else
        return (nub legal_alt_paths) :: IO [Path]
  where
    get_slices :: Int -> Int -> [(Int,Int)]
    get_slices max_len len =
      join [[(i,j) |j<-[i+1..(min (i+max_len) (len-1))]] | i<-[0..(len-1)]]

    check_legal_path :: Path -> Bool
    check_legal_path path = isJust (path_get_end path)
        
    takeRandom :: Int -> [a] -> IO [a]
    takeRandom n xs | n <= 0 = return xs
                    | otherwise = do
                        indices <- replicateM n (randomRIO (0, length xs - 1))
                        return $ map (xs !!) indices


get_alt_slice :: EquivalenceLibrary -> (Int,Int) -> [Morphism] -> [[Morphism]]
get_alt_slice elib slice morphs =
  let
    (start,end) = slice
    morph_slice = take (end - start) . drop start $ morphs :: [Morphism]
    equiv_morphs = get_equiv_morphs elib morph_slice :: [[Morphism]]
    alt_morphs = fmap (\mslice -> take start morphs ++ mslice ++ drop end morphs) equiv_morphs :: [[Morphism]]
  in
    alt_morphs

--
    
elib_equiv_expand_annealing :: (EquivalenceLibrary,Int,Int) -> Path -> IO ((EquivalenceLibrary,Int,Int),[Path])
elib_equiv_expand_annealing (elib,slice_len,sample_size) path =
  do
    expansion <- equivalence_expansion elib slice_len sample_size path
    return ((elib,slice_len,sample_size),expansion)

instance AnnealingEnv (EquivalenceLibrary,Int,Int) where
  update_env_with_temp temp (elib,slice_len,sample_size) = (elib,round (temp*10+1),sample_size)





-- identity guided search

-- wrap func, unwrap func
type SymbolicMatchContext = (Morphism->Morphism,Morphism->Maybe Morphism,VariableMap,[SymbolicMorphism])

instance Eq (Morphism-> Morphism) where
  (==) f1 f2 = (f1 IdR) == (f2 IdR)

instance Show (Morphism->Morphism) where
  show mfunc = show . mfunc $ IdR

instance Eq (Morphism-> Maybe Morphism) where
  (==) f1 f2 = True -- rely on forward function to assess equality

instance Show (Morphism->Maybe Morphism) where
  show mfunc = "<unwrap_func>"

match_identity :: [Morphism] -> [[SymbolicMorphism]] -> [SymbolicMatchContext]
match_identity morphs identity =
  let
    possible_matches = filter (\sm_list -> length sm_list == length morphs) identity :: [[SymbolicMorphism]]
  in
    nub $ do -- List
      (wfunc,uwfunc,var_map) <- get_first_match morphs possible_matches :: [(Morphism->Morphism,Morphism->Maybe Morphism,VariableMap)]
      do
        el <- identity  -- return the entire identity if any elements match, assumes same wfunc and var_map for every identity
        return (wfunc,uwfunc,var_map,el)
  where
    get_first_match :: [Morphism] -> [[SymbolicMorphism]] -> [(Morphism->Morphism,Morphism->Maybe Morphism,VariableMap)]
    get_first_match morphs (pm:possible_matches) =
      let
        representations = match_symbolic_identity_autofunctor morphs pm
      in
        if null representations then
          get_first_match morphs possible_matches
        else
          representations
    get_first_match _ [] = []


--msm_help :: VariableMap -> Morphism -> SymbolicMorphism -> [VariableMap]

match_against_context :: Morphism -> SymbolicMatchContext -> [SymbolicMatchContext]
match_against_context _ (wfunc,uwfunc,var_map,[]) = [] 
match_against_context morph (wfunc,uwfunc,var_map,sm:sym_morphs) =
  do -- []
    mapped_morph <- maybeToList (uwfunc morph)
    new_var_map <- msm_help var_map mapped_morph sm
    return (wfunc,uwfunc,new_var_map,sym_morphs)
      
identity_guided_search :: EquivalenceLibrary -> Ring -> Ring -> [Morphism] -> IO [[Morphism]]
identity_guided_search elib start end morphs =
  let
    smcs = join (fmap (match_identity morphs) elib) :: [SymbolicMatchContext]
  in
    do
      res <- igs_help smcs start end []
      --logObj "igs call" (start,end,morphs,res)
      return res
  where
    igs_help :: [SymbolicMatchContext] -> Ring -> Ring -> [Morphism] -> IO [[Morphism]]
    igs_help smcs cur end so_far | cur == end =  (return . return  $ so_far) -- logObj "found path" so_far >>
                                 | null smcs = (return []) --  logObj "aborted path" so_far >>
                                 | prod_dimension cur > prod_dimension end = return []
                                 | otherwise =
      do -- IO
        --logObj "expanding " (cur,smcs,so_far)
        mm <- morphismMatch
        new_morphs <- match mm cur :: IO [Morphism]
        next_rings_with_morph <- sequence . fmap (\m -> io_apply m cur>>= (\r -> return (r,m))) $ new_morphs :: IO [(Ring,Morphism)]
        --logObj "morphs " (fmap snd next_rings_with_morph)
        fmap (filter (not . null)) . fmap nub . fmap join . sequence $ do
              (next_ring,new_morph) <- next_rings_with_morph :: [(Ring,Morphism)]
              do -- []
                smc <- smcs :: [SymbolicMatchContext]
                let new_smcs = match_against_context new_morph smc :: [SymbolicMatchContext]

                if not . null $  new_smcs then
                  return . fmap nub $ igs_help new_smcs next_ring end (so_far++[new_morph]) :: [IO [[Morphism]]]
                  else
                  []

identity_guided_expansion :: EquivalenceLibrary -> Int -> Int -> Path -> IO [Path]
identity_guided_expansion elib slice_len sample_size path =
  let
    morphs = path_get_morphs path
    path_start = path_get_start path
    slices = get_slices slice_len (length morphs +1) :: [(Int,Int)]
  in
    do -- IO
      path_end <- maybeToIO "failed get end" $ path_get_end path
      sample_slices <- takeRandom sample_size slices :: IO [(Int,Int)]
      states <- maybeToIO "identity guided expansion: failed getting states" (path_get_states path) :: IO [Ring]

      expansion <- fmap nub . fmap join . sequence $ do -- []
        (start,end) <- sample_slices :: [(Int,Int)]
        let (start_state,end_state) = (states!!start,states!!end) :: (Ring,Ring)
        let morphism_slice = drop start (take end morphs) :: [Morphism]

        return $ do -- IO
          alt_morph_lists <- identity_guided_search elib start_state end_state morphism_slice :: IO [[Morphism]]
          let alt_paths = fmap (\alt_morph_list -> Path path_start (take start morphs ++ alt_morph_list ++ drop end morphs)) alt_morph_lists
          let unique_alt_paths = nub alt_paths
          let legal_alt_paths = filter (check_legal_path path_end) unique_alt_paths
          if length legal_alt_paths < length unique_alt_paths then
            logObj "all paths, legal paths" (alt_paths,legal_alt_paths)
            else
            return ()
          return legal_alt_paths :: IO [Path]
      logObj "identity_guided_expansion" (length expansion)
      return expansion
      
  where
    get_slices :: Int -> Int -> [(Int,Int)]
    get_slices max_len len =
      join [[(i,j) |j<-[i+1..(min (i+max_len) (len-1))]] | i<-[0..(len-1)]]

    check_legal_path :: Ring -> Path -> Bool
    check_legal_path end path =
      fromMaybe False $ do
        found_end <- (path_get_end path)
        return (end == found_end)
        
    takeRandom :: Int -> [a] -> IO [a]
    takeRandom n xs | n <= 0 = return xs
                    | otherwise = do
                        indices <- replicateM n (randomRIO (0, length xs - 1))
                        return $ map (xs !!) indices
                      
id_guided_expand_annealing :: (EquivalenceLibrary,Int,Int) -> Path -> IO ((EquivalenceLibrary,Int,Int),[Path])
id_guided_expand_annealing (elib,slice_len,sample_size) path =
  do
    expansion <- identity_guided_expansion elib slice_len sample_size path
    return ((elib,slice_len,sample_size),expansion)
