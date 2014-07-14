module IMP where

import IMPDef
import IMPParser
import IMPEval

-- Punto de entrada del interprete
--   (lee el código en el archivo test.imp y lo interpreta).
main :: IO ()
main = do code <- readFile "testOrFailed.imp"
          let program   = parseProgram code
           in print (start program)

