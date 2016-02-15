module Main where

import Network.Wai.Handler.Warp (run)

import Files

main :: IO ()
main = run 8083 app
