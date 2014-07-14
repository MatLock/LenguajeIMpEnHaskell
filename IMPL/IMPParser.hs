module IMPParser (parseProgram) where

import Parser
import IMPDef

parseProgram :: String -> Program
parseProgram code = case parse ((×)stmt·end >· buildBlock) code of
                      Right p -> p
                      _       -> error "parsing error"

buildAssign [Left e, Left (Var s)]     = Right $ Assign s e
buildBlock  ss                         = Right $ Block (map (\(Right s) -> s) $ reverse ss)
buildIf     [Right e, Right t, Left c] = Right $ If c t e
buildIf     [Right t, Left c]          = Right $ If c t (Block [])
buildWhile  [Right b, Left c]          = Right $ While c b
buildPrint                             = Right . Print
buildFail                              = Right . Fail

buildBin op [Left x]         = Left $ x
buildBin op [Left x, Left y] = Left $ Bin op y x
buildUn  op [Left x]         = Left $ Un op x

stmt = id · (assignstmt ¦ blockstmt ¦ ifstmt ¦ whilestmt ¦ printstmt ¦ failstmt)

assignstmt = variable · (#)"=" · expr · (#)";"                      >· buildAssign
blockstmt  = (#)"{" · (×)stmt · (#)"}"                              >· buildBlock
ifstmt     = (#)"if" · expr · (#)"then" · stmt ·¦ (#)"else" · stmt  >· buildIf
whilestmt  = (#)"while" · expr · (#)"do" · stmt                     >· buildWhile
printstmt  = (#)"print" · (#)"\"" · ((×)(satisfy ('"'/=)) ·> buildPrint) · (#)"\""
failstmt   = (#)"fail" · (#)"\"" · ((×)(satisfy ('"'/=)) ·> buildFail) · (#)"\""

expr   = orexp
orexp  = andexp ·¦ (#)"||" · orexp   >· buildBin Or
andexp = eqexp  ·¦ (#)"&&" · andexp  >· buildBin And
eqexp  = ltexp  ·¦ (#)"==" · eqexp   >· buildBin Eq
ltexp  = gtexp  ·¦ (#)"<"  · ltexp   >· buildBin Lt
gtexp  = addexp ·¦ (#)">"  · gtexp   >· buildBin Gt
addexp = subexp ·¦ (#)"+"  · addexp  >· buildBin Add
subexp = mulexp ·¦ (#)"-"  · subexp  >· buildBin Sub
mulexp = divexp ·¦ (#)"*"  · mulexp  >· buildBin Mul
divexp = unexp  ·¦ (#)"/"  · divexp  >· buildBin Div

unexp  = number                                        ¦
         ((#)"true"  ·> (const (Left $ ConstB True)))  ¦
         ((#)"false" ·> (const (Left $ ConstB False))) ¦
         variable                                      ¦
         ((#)"-" · unexp >· buildUn Neg)               ¦
         ((#)"!" · unexp >· buildUn Not)               ¦
         (#)"(" · expr · (#)")"

number   = (·×)digit ·> (Left . ConstI . (read :: String -> Int))
variable = (·×)alpha ·> Left . Var
digit    = range '0' '9'
alpha    = range 'A' 'z'

