{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
module Python where

import Text.InterpolatedString.Perl6 ( q, qq )

{- 
Library for translating to Python
might be changed to using `qq` instead of `q` later
-}

listQ :: String
listQ = [q|def _list(*kwargs): return list(kwargs)|]

consQ :: String
consQ = [q|
def cons(x, xs):
    if type(xs) == list:
        return [x] + xs
    if type(xs) == tuple:
        return (x, xs)
|]

carQ :: String
carQ = [q|def car(xs): return xs[0]|]

cdrQ :: String
cdrQ = [q|
def cdr(xs):
    if type(xs) == list:
        return xs[1:]
    if type(xs) == tuple:
        return xs[2]
|]

ifQ :: String
ifQ = [q|def _if(pred, result, alt): return result if pred else alt|]

-- * example of converting (temp)
-- * later will be something like :: LispVal -> String
define :: String -> String -> String
define var content = [qq|{var} = {content}|]
