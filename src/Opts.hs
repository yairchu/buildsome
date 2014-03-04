module Opts (DeleteUnspecifiedOutputs(..), Opt(..), getOpt) where

import Options.Applicative
import Data.Monoid

data DeleteUnspecifiedOutputs = DeleteUnspecifiedOutputs | DontDeleteUnspecifiedOutputs

deleteUnspecifiedOutputs :: Bool -> DeleteUnspecifiedOutputs
deleteUnspecifiedOutputs False = DontDeleteUnspecifiedOutputs
deleteUnspecifiedOutputs True = DeleteUnspecifiedOutputs

data Opt = Opt { optMakefilePath :: FilePath
               , optParallelism :: Maybe Int
               , optGitIgnore :: Maybe FilePath
               , optDeleteUnspecifiedOutputs :: DeleteUnspecifiedOutputs
               }

opt :: Read a => Char -> String -> String -> Parser (Maybe a)
opt s l doc = optional $ option $ short s <> long l <> help doc

getOpt :: IO Opt
getOpt = execParser opts
  where
    parser = Opt <$> argument str (metavar "MakefilePath")
                 <*> opt 'j' "parallelism" "How many commands to execute in parallel"
                 <*> opt 'g' "gitignore" "Write a .gitignore file at the specified path"
                 <*> (deleteUnspecifiedOutputs <$>
                      switch (short 'D' <>
                              long "delete-unspecified" <>
                              help "Delete unspecified outputs"))
    opts = info (helper <*> parser) mempty
