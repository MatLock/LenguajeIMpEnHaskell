module IMP where

import IMPDef
import IMPParser
import IMPChecker
import IMPEval

main :: IO ()
main = do code <- readFile "test.imp"
          let program   = parseProgram code
              typeCheck = checkP program
           in 
            case typeCheck of
                Just msg -> print msg
                Nothing  -> print (start program)

