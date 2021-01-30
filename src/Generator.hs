module Generator where

import           Lisp
import           Python
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )

-- * currently not analyzing the env (not now)
-- data Env = Local [String] Env | Global [String] deriving Show

-- primitive :: Env
-- primitive = Global $ M.keys prim

transpile :: LispStruct -> String
transpile = gen . mapSt convertPrim

prim :: M.Map String String
prim = M.fromList
  [ ("+"    , "add_")
  , ("-"    , "sub_")
  , ("*"    , "mul_")
  , ("/"    , "div_")
  , ("="    , "eq_")
  , ("<"    , "lt_")
  , ("<="   , "le_")
  , (">"    , "gt_")
  , (">="   , "ge_")
  , ("list" , "list_")
  , ("list?", "listq_")
  , ("pair?", "listq_")
  , ("null?", "null_")
  , ("not"  , "not_")
  , ("and"  , "and_")
  , ("or"   , "or_")
  , ("map"  , "map_")
  , ("filter", "filter_")
  ]

convertPrim :: String -> String
convertPrim x = fromMaybe x (M.lookup x prim)

gen :: LispStruct -> String
gen (Atom    x ) = x
gen (Number  x ) = show x
gen (String  x ) = "\"" ++ x ++ "\""
gen (Boolean x ) = if x then "True" else "False"
gen (Define x y) = define (gen x) (gen y)
gen (IfExpr p r a) =
  "(" ++ gen r ++ " if " ++ gen p ++ " else " ++ gen a ++ ")"
gen (Quote q       ) = genQuote q
gen (Lambda a b    ) = lambda (map gen a) (gen b)
gen (List l        ) = genList l
gen (DottedList l x) = genDList l x

genQuote :: LispStruct -> String
genQuote (List l) = list $ map (\x -> if isList x then genQuote x else gen x) l
 where
  isList (List _) = True
  isList _        = False
genQuote x = gen x

genList :: [LispStruct] -> String
genList []                = "[]"
genList (Atom fun : args) = funcApp fun (map gen args)

genDList :: [LispStruct] -> LispStruct -> String
genDList [x     ] y = pair (gen x) (gen y)
genDList (x : xs) y = pair (gen x) (genDList xs y)

mapSt :: (String -> String) -> LispStruct -> LispStruct
mapSt f (Atom x        ) = Atom (f x)
mapSt f (Define n b    ) = Define n (mapSt f b)
mapSt f (IfExpr p r a  ) = IfExpr (mapSt f p) (mapSt f r) (mapSt f a)
mapSt f (Quote q       ) = Quote (mapSt f q)
mapSt f (Lambda a b    ) = Lambda a (mapSt f b)
mapSt f (List l        ) = List $ map (mapSt f) l
mapSt f (DottedList l x) = DottedList (map (mapSt f) l) (mapSt f x)
mapSt _ x                = x
