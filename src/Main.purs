module Main where

import Prelude

import Day1 (part1, part2)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Process (argv)

interactWith :: (String -> String) -> String -> Effect Unit
interactWith function inputFile = do
  input <- readTextFile UTF8 inputFile
  log (function input)

main :: Effect Unit
main = mainWith myFunction
  where mainWith function = do
          args <- argv
          case args of
            [_, _, input] -> interactWith function input
            _ -> log "error: exactly one argument needed"

        myFunction = part2
