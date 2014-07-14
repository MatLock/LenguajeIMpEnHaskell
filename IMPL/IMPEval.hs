module IMPEval where

import IMPDef
import Store
import IMPMonad


-- Función que evalua una expresión
eval :: Expression -> Interpreter Value
eval (ConstI i) = return (VInt i) 
eval (ConstB b) = return (VBool b)
eval (Var v)    = load v
eval (Un up e)  = (eval e) >>= evalUn up 
eval (Bin bp e1 e2) = do r1 <- eval e1 -- >>=  
                         r2 <- eval e2 -- >>=
			 evalBin bp r1 r2 


-- Función que evalua una expresión unaria
evalUn :: UnOp -> Value -> Interpreter Value
evalUn Neg value = case value of 
			VInt x -> return (VInt (-x))  
			VBool x -> fail "no puedo evaluar"
evalUn Not value = case value of
			VInt x ->  fail "no puedo hacerle un NOT a un int"
			VBool x -> return (VBool (not x))


-- Función que evalua una expresión binaria
evalBin :: BinOp -> Value -> Value -> Interpreter Value
evalBin Add v1 v2 = evaluarInts  v1 v2 (+)
evalBin Sub v1 v2 = evaluarInts  v1 v2 (-)
evalBin Mul v1 v2 = evaluarInts  v1 v2 (*)
evalBin Div v1 v2 = evaluarInts  v1 v2 (div)
evalBin Eq  v1 v2 = evaluarEqual v1 v2 
evalBin Lt  v1 v2 = evaluarLt    v1 v2 
evalBin Gt  v1 v2 = evaluarGt    v1 v2 
evalBin Or  v1 v2 = evaluarBools v1 v2 (||)
evalBin And v1 v2 = evaluarBools v1 v2 (&&) 

evaluarLt :: Value -> Value -> Interpreter Value
evaluarLt (VInt i) (VInt i2) = return (VBool (i < i2))
evaluarLt _ _ = fail "elementos no comparables"

evaluarGt :: Value -> Value -> Interpreter Value
evaluarGt (VInt i) (VInt i2) = return (VBool (i > i2))
evaluarGt _ _ = fail "elementos no comparables"

evaluarEqual :: Value -> Value -> Interpreter Value
evaluarEqual (VInt i) (VInt i2) = return (VBool (i == i2))
evaluarEqual (VBool b) (VBool b2) = return (VBool (b == b2))
evaluarEqual _ _ = fail "tipos diferentes, no puedo comparar"

evaluarInts :: Value -> Value -> (Int -> Int -> Int) -> Interpreter Value
evaluarInts (VInt i) (VInt i2) op = return (VInt (op i i2) )
evaluarInts _ _ _ = fail "operacion no valida con booleanos"

evaluarBools :: Value -> Value -> (Bool -> Bool -> Bool) -> Interpreter Value
evaluarBools (VBool b1) (VBool b2) op = return (VBool (op b1 b2))
evaluarBools _ _ _  = fail "operacion no valida con enteros"

-- Función que ejecuta una sentencia
exec :: Statement -> Interpreter ()
exec (Assign v e) = eval e >>= register v
exec (If e s1 s2) = (eval e) >>= (\value -> checkVBool value >>= (\bool -> if bool then exec s1 else exec s2))
		   -- monada                  --monada

exec (Print m) = impPrint m
exec (Fail m) = fail m
exec (Block ss) = combinarI (map exec ss)
exec (While exp st ) = do value <- (eval exp) -- >>=
			  bool <- checkVBool value -- >>=
			  if bool then exec st >>= (\s -> exec(While exp st)) else return () 

 

combinarI :: [Interpreter ()] -> Interpreter ()
combinarI [] = return ()
combinarI (x:xs) =   x >>= (\v -> combinarI xs) 
                
-- Función que ejecuta un programa desde un inicio "limpio"
start :: Program -> Execution ()
start p = (step (exec p)) empty


-- --------------------------
-- AUXILIARES
-- --------------------------

-- Función que verifica si un Value es un booleano 
--  (útil en la ejecución de ifs y whiles)
checkVBool :: Value -> Interpreter Bool
checkVBool (VBool b) = return b
checkVBool _         = fail "TYPE ERROR: condicion no booleana!"

