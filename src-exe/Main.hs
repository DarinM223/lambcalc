module Main where

import Lambcalc (compile, parse)

example :: String
example = "(fn g => (fn x => g (fn v => x x v)) (fn x => g (fn v => x x v))) (fn f => fn x => if x then (if x - 1 then x * f (x - 1) else 1) else 1) 5"

main :: IO ()
main = do
  let ll = compile $ parse example
  putStrLn ll
  writeFile "main.ll" ll