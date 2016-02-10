module Main where
import System.Environment

main :: IO()
main =
  head <$> getArgs >>= putStrLn
