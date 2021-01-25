module Lib
  ( entry
  )
where

import           Data.Semigroup                 ( (<>) )
import           Options.Applicative

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
          (long "output" <> short 'o' <> metavar "OUTPUT" <> value "" <> help
            "output Python file"
          )
    <*> switch
          (long "header" <> help "generate separate file for helper functions")
    <*> switch (long "llist" <> help "use lisp style lists")

handle :: Options -> IO ()
handle _ = putStrLn "[<Enrty Point>]"
