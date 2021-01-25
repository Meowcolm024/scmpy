module Generator where

import           Lisp                           ( )
import           Python                         ( )

data Env = Local [String] Env | Global [String] deriving Show

primitive :: Env
primitive =
    Global ["+", "-", "*", "/", "display", "displayln", "list", "quote"]
