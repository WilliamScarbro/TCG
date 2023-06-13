module Compile.CompileKernel where

-- assume b p are common accross all kernels, this allows a common w

import Algebra.NTT
import Data.Maybe
import Data.List
import qualified Data.Map as Map (fromList,empty,insert,Map,member,lookup,mapWithKey,mapAccum,mapAccumWithKey)
 
--- [Kernel] --> C file

type KList=[Kernel]
type PCC = Map.Map Kernel String


-- name -> path -> code
compileKList :: KList -> PCC -> String
compileKList path pcc = let compKers = fmap (\ker -> compileKernel 0 ker pcc) path in
    let steps = foldr (++) [] (fmap maybeToList compKers) in
      "\n  //Computation\n" ++ foldl (\x y -> (x++swap)++y) (head steps) (tail steps)

--  fmap (Kernel -> String) [Kernel]
-- Maybe (Integer,String) >>= (String -> Maybe String)

--compileKList p km = let steps = foldr (++) [] (fmap maybeToList (fmap (\ker -> compileKernel 0 ker (Map.lookup km) p)) in

showTuple :: Show a => [a] -> String
showTuple t = let st = fmap show t in foldl (\x y -> (x++","++y)) (head st) (tail st)

compileKernel :: Int -> Kernel -> PCC -> Maybe String
--compileKernel = undefined
compileKernel o (Phi n k d b p) pcc = Just ("  Phi("++(showTuple [n,k,d,b,p])++","++(endXY o)++","++(endW (Phi n k d b p) pcc)++");\n")
compileKernel o (KL n k) pcc = Just ("  LPerm("++(showTuple [n,k])++","++(endXY o)++");\n")
compileKernel o (KT n k l) pcc = Just ("  TPerm("++(showTuple [n,k,l])++","++(endXY o)++");\n")
compileKernel o (KId n) pcc = Nothing
compileKernel o (Gamma n d b p) pcc = Just ("  Gamma("++(showTuple [n,d,b,p])++","++(endXY o)++","++(endW (Gamma n d b p) pcc)++");\n")
compileKernel o (Kernel_Repeat n k ker) pcc = Just (foldr (++) "" (foldr (++) [] (fmap maybeToList [compileKernel (o+(div n k)*i) ker pcc | i<-[0..k-1]])))
compileKernel o (Kernel_Extend n k f) pcc = Just (foldr (++) "" (foldr (++) [] (fmap maybeToList [(f i) >>= (\ker -> compileKernel (o+(div n k)*i) ker pcc) | i<-[0..k-1]]))) 

endW :: Kernel -> PCC -> String
endW ker pcc = squashMaybeString (Map.lookup ker pcc) "Error couldn't fid kernel in PCC Map"
--endW ker pcc = squashMaybeString (do { mtup <- Map.lookup ker pcc;
--                                          (\x -> Just (snd x)) mtup }) "<Error: couldn't find kernel in PCC Map>"

swap :: String
swap = "  swap(X,Y);\n"

endXY :: Int -> String
endXY o = "*X+"++show o++",*Y+"++show o

squashMaybeString :: Maybe String -> String -> String
squashMaybeString (Just str) msg = str
squashMaybeString Nothing msg = msg
---

squashMaybeBool :: Maybe a -> (a -> Bool) -> Bool
squashMaybeBool (Just t) f = f t
squashMaybeBool Nothing _ = False

reqPCC :: Kernel -> Bool
reqPCC (Phi _ _ _ _ _) = True
reqPCC (KT _ _ _) = False
reqPCC (KL _ _) = False
reqPCC (KId _) = False
reqPCC (Gamma _ _ _ _) = True
reqPCC (Kernel_Repeat _ _ k) = reqPCC k
reqPCC (Kernel_Extend _ _ f) = squashMaybeBool (f 0) reqPCC

---

listLeafs :: Kernel -> [Kernel]
listLeafs (Phi n k d b p) = [Phi n k d b p]
listLeafs (KL _ _) = []
listLeafs (KT _ _ _) = []
listLeafs (KId _) = []
listLeafs (Gamma n d b p) = [Gamma n d b p]
listLeafs (Kernel_Repeat n k ker) = listLeafs ker
listLeafs (Kernel_Extend n k f) = let maybeKernels = [f i >>= (\ker -> Just (listLeafs ker)) | i<-[0..k-1]] in
  foldl (++) [] (foldl (++) [] (fmap maybeToList maybeKernels))

precompute :: Kernel -> String -> String
precompute (Phi n k d b p) pcc_name = "  Phi_W(w,"++showTuple [d,b,k,p]++","++pcc_name++");\n"
precompute (Gamma n d b p) pcc_name = "  Gamma_W(w,"++showTuple [n,d,b,p]++","++pcc_name++");\n"
--precompute (Kernel_Repeat n k ker) pcc_name = precompute ker pcc_name
--precompute (Kernel_Extend n k f) pcc_name = foldr (++) "" [precompute (f i) pcc_name | i<-[0..k]]
--
--
----
associateKernels :: KList -> PCC
associateKernels p = let rp = nub (filter reqPCC (foldr (++) [] (fmap listLeafs p))) in
  Map.fromList [(rp!!i, associateKernel i (rp!!i)) | i<-[0..(length rp)-1]]
--
associateKernel :: Int -> Kernel -> String
associateKernel c (Phi n k d b p) = "W"++show c
associateKernel c (Gamma n d b p) = "W"++show c
associateKernel c _ = "Error: Kernel does not need PCC"

--
reducePCC :: PCC -> PCC
reducePCC pcc = fst (Map.mapAccumWithKey reducePCC_help pcc pcc)

reducePCC_help :: PCC -> Kernel -> String -> (PCC,String)
reducePCC_help pcc curK curV = (Map.mapWithKey (\k v -> if samePCC k curK then curV else v) pcc,"garbage")

samePCC :: Kernel -> Kernel -> Bool
samePCC (Phi n1 k1 d1 b1 p1) (Phi n2 k2 d2 b2 p2) = k1==k2 && b1==b2 && d1==d2 && p1==p2 
samePCC k1 k2 = False

sizePCC :: Kernel -> Int
sizePCC (Phi n k d b p) = k*k
sizePCC _ = -1
----

uniquePCC :: PCC -> [(Int,String,String)]
uniquePCC pcc = nub (fst (Map.mapAccumWithKey (\pred kernel name -> (pred++[(sizePCC kernel,name,precompute kernel name)],0)) [] pcc))

--

initializePCC :: [(Int,String,String)] -> String
initializePCC pcc = let allocs = foldl (\pred (size,name,preComp) -> (pred++"  int* "++name++" = malloc("++show size++"*sizeof(int));\n")) "" pcc in
  let assigns = foldl (\pred (size,name,preComp) -> (pred++preComp)) "" pcc in
    "\n  //Pre-Compute Constants\n"++allocs ++ assigns

  --  let assigns = Map.mapWithKey (\ker (size,name) -> precompute ker name) pcc in
--    (Map.mapAccum (\x y -> (x++y,y)) allocs) ++ (Map.mapAccum (\x y -> (x++y,y)) assigns)
    
destroyPCC :: [(Int,String,String)] -> String
destroyPCC pcc = "\n  //free Pre-Computed Constants\n" ++ foldl (\pred (size,name,preComp) -> (pred++"  free("++name++");\n")) "" pcc

--

initialize_w :: (Int,Int) -> String
initialize_w (b,p) = "  int w = Nth_root("++show p++",generator("++show p++"),"++show b++");\n"
--

imports = "#include <stdlib.h>\n#include \"../timer.h\"\n#include \"../NTLib.h\"\n#include \"../Util.h\"\n#include \"../LPerm.h\"\n#include \"../Phi.h\"\n#include \"../Gamma.h\"\n\n"


main_func :: Int -> String -> Int -> String
main_func size name path_parity = let fsig = "int main(int argc, char** argv){\n" in
                        let alloc = "  int* X = malloc(sizeof(int)*"++show size++");\n  int* Y = malloc(sizeof(int)*"++show size++");\n\n" in
                          let init = "  for(int i=0; i<"++show size++"; i++){\n    X[i]=i;\n  }\n" in
                            let callfunc = "  "++name++"(&X,&Y);\n\n" in
                              let res_var = if path_parity==0 then "Y" else "Y" in -- apparently we don't need this? short circuted for now
                                let print_res = "  print_array(\"result\","++res_var++","++show size++");\n\n" in
                                  let free = "  free(X);\n  free(Y);\n" in
                                    fsig++alloc++init++callfunc++print_res++free++"}\n"

start_timer = "\n  initialize_timer();\n  start_timer();\n"
stop_timer = "\n  stop_timer();\n"
report_timer = "  printf(\"Elapsed time: %f\\n\",elapsed_time());\n"
--

compile :: (Int,Int,Int) -> String -> KList -> String
compile (n,b,p) name path = let filtered_path = filter (\k -> not (is_identity k)) path in
  let iw = initialize_w (b,p) in
    let ipcc = associateKernels filtered_path in
      let pcc = reducePCC ipcc in
        let upcc = uniquePCC pcc in
          let ip = initializePCC upcc in
            let cp = compileKList filtered_path pcc in
              let dp = destroyPCC upcc in
                let mf = main_func n name (mod (length path) 2) in
                  imports++"void "++name++"(int** X,int** Y){\n"++iw++ip++start_timer++cp++stop_timer++report_timer++dp++"}\n"++mf

--

--

is_identity :: Kernel -> Bool
is_identity (Phi _ _ _ _ _) = False
is_identity (Gamma _ _ _ _) = False
is_identity (KL _ _) = False
is_identity (KT _ _ _) = False
is_identity (KId _) = True
is_identity (Kernel_Extend _ _ f) = squashMaybeBool (f 0) is_identity
is_identity (Kernel_Repeat _ _ k) = is_identity k
