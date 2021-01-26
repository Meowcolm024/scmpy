module Generator where

import           Lisp
import           Python
import qualified Data.Map                      as M

-- * currently not analyzing the env
-- data Env = Local [String] Env | Global [String] deriving Show

-- primitive :: Env
-- primitive = Global $ M.keys prim

prim :: M.Map String String
prim = M.fromList
    [ ("+"    , "_add")
    , ("-"    , "_sub")
    , ("*"    , "_mul")
    , ("/"    , "_div")
    , ("list" , "_list")
    , ("null?", "_null")
    ]

convertPrim :: String -> String
convertPrim x = case M.lookup x prim of
    Just y  -> y
    Nothing -> x

gen :: LispStruct -> String
gen (Atom    x     ) = x
gen (Number  x     ) = show x
gen (String  x     ) = "\"" ++ x ++ "\""
gen (Boolean x     ) = if x then "True" else "False"
gen (Define x y    ) = define (gen x) (gen y)
gen (IfExpr p r a  ) = "_if(" ++ gen p ++ "," ++ gen r ++ "," ++ gen a ++ ")"
gen (Quote q       ) = genQuote q
gen (Lambda a b    ) = lambda (map gen a) (gen b)
gen (List l        ) = genList l
gen (DottedList l x) = genDList l x

genQuote :: LispStruct -> String
genQuote = undefined

genList :: [LispStruct] -> String
genList = undefined

genDList :: [LispStruct] -> LispStruct -> String
genDList = undefined
