module Main where

import System.Environment
import Parse
import qualified Data.ByteString as B

main = do
    args <- getArgs
    klass <- if null args
            then B.getContents
            else B.readFile (head args)
    let k = parse klass
    putStrLn (show k)
