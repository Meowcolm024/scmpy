module Lib
  ( entry
  )
where

import           Data.Semigroup                 ( (<>) )
import           Options.Applicative
import           System.IO
import           Generator                      ( transpile )
import           Parser                         ( parseLisp )
import           Lisp                           ( eval )
import           Python                         ( scmlib )

entry :: IO ()
entry = handle =<< execParser opts
 where
  opts = info
    (cli <**> helper)
    (fullDesc <> progDesc "Transpile Scheme to Python" <> header
      "scmpy - A Scheme to Python transpiler"
    )

data Options = Options
  { _file     :: String
  , _output   :: String
  , _header   :: Bool
  , _lispList :: Bool
  }

cli :: Parser Options
cli =
  Options
    <$> strOption
          (long "file" <> short 'f' <> metavar "SOURCE" <> help
            "scheme source file"
          )
    <*> option
          auto
          (  long "output"
          <> short 'o'
          <> metavar "OUTPUT"
          <> value "a.py"
          <> help "output Python file"
          )
    <*> switch
          (long "header" <> help "generate separate file for helper functions")
    <*> switch (long "llist" <> help "use lisp style lists")

handle :: Options -> IO ()
handle (Options f o True _) =
  ("from scmpylib import *\n\n" ++)
    .   scmpy
    <$> iFile f
    >>= oFile o
    >>  oFile "scmpylib.py" scmlib
handle (Options f o False _) = (scmlib ++) . scmpy <$> iFile f >>= oFile o

oFile :: FilePath -> String -> IO ()
oFile name content = do
  handle <- openFile name WriteMode
  hPutStrLn handle content
  hClose handle

iFile :: FilePath -> IO String
iFile = readFile

scmpy :: String -> String
scmpy x = case map (transpile . eval) <$> parseLisp x of
  Left  err -> error (show err)
  Right x   -> unlines x
