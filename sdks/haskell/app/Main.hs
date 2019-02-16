module Main where

import Beam

wordcount :: PCollection
wordcount =
  let lines = readLines "gs://apache-beam-samples/shakespeare/*"
  let words = fmap (Regex.split "\\s+") lines
  let counted = counts words
  let formatted = fmap (KV k v -> sprintf "%s: %s" k v) counted
  writeLines "wordcounts.txt" formatted

main :: IO ()
main = runDirect wordcount
