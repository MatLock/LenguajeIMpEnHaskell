module IMPChecker where

import IMPDef
import Store

data Type = IntT | BoolT deriving (Eq,Show)

data Typing v =  -- tipo del contexto de ejecuci�n de un programa
     OkT v (Store Type) | -- contexto exitoso (con memoria)
     ErrorT Message       -- contexto de error (con mensaje)
     deriving Show

newtype Checker v =  -- tipo del estado de un interprete de un programa IMP
     Check { stepT :: (Store Type) -> Typing v }

-- es lo mismo que decir:
-- data Checker v =  Check (Store Type -> Typing v)

-- step :: Store v -> (Store Type -> Typing v)
-- step (Check f) = f

instance Monad Checker where  -- declaraci�n del interprete como m�nada
  fail msg = Check (\st -> ErrorT msg)
  return x = Check (\st -> OkT x st)
  i >>= f  = Check (\st -> case (stepT i) st of
				ErrorT msg -> ErrorT msg
				OkT v st' -> case (stepT (f v)) st' of
						ErrorT msg' -> ErrorT msg'
						OkT w st''  ->  OkT w st'')



-- Funci�n para leer el tipo de una variable en la memoria
loadT :: Variable -> Checker Type
loadT x = Check (\st -> case get st x of
			 	Nothing -> ErrorT "no esta en memoria"
				Just v -> OkT v st) 

-- Funci�n para escribir el tipo de una variable en memoria
registerT :: Variable -> Type -> Checker ()
registerT v t = Check (\st -> OkT () (set st v t)) 


unify :: Checker () -> Checker () -> Checker ()
unify (Check f) (Check g) = Check $ \r ->
  case (f r, g r) of
    (ErrorT msg, _)     -> ErrorT msg
    (_, ErrorT msg)     -> ErrorT msg
    (OkT _ r1, OkT _ r2) -> case unification r1 r2 of
                            Just r' -> OkT () r'
                            _       -> ErrorT "Unification error."
  where unification = join (\t1 t2 -> if t1 == t2 then Just t1 else Nothing)


-- Funci�n que checkea el tipo de una expresi�n
checkE :: Expression -> Checker Type
checkE (ConstI n)   = return IntT
checkE (ConstB n)   = return BoolT
checkE (Var x)      = loadT x
checkE (Un op e)    = do v <- checkE e
                         checkUn op v
checkE (Bin op e q) = do v <- checkE e
                         t <- checkE q
                         checkBin op v t


-- Funci�n que checkea el tipo de una expresi�n unaria
checkUn :: UnOp -> Type -> Checker Type
checkUn Neg IntT  = return IntT
checkUn Neg _     = fail "Type error on Neg."
checkUn Not BoolT = return BoolT
checkUn Not _     = fail "Type error on Not."


-- Funci�n que checkea el tipo de una expresi�n binaria
checkBin :: BinOp -> Type -> Type -> Checker Type
checkBin Add IntT IntT   = return IntT
checkBin Add _    _      = fail "Type error on Add."
checkBin Sub IntT IntT   = return IntT
checkBin Sub _    _      = fail "Type error on Sub."
checkBin Mul IntT IntT   = return IntT
checkBin Mul _    _      = fail "Type error on Mul."
checkBin Div IntT IntT   = return IntT
checkBin Div _    _      = fail "Type error on Div."
checkBin Eq  n    m      | n == m    = return BoolT
                         | otherwise = fail "Type error on Eq."
checkBin Lt  IntT IntT   = return BoolT
checkBin Lt  _    _      = fail "Type error on Lt."
checkBin Gt  IntT IntT   = return BoolT
checkBin Gt  _    _      = fail "Type error on Gt."
checkBin Or  BoolT BoolT = return BoolT
checkBin Or  _     _     = fail "Type error on Or."
checkBin And BoolT BoolT = return BoolT
checkBin And _     _     = fail "Type error on And."



-- Funci�n que checkea el tipo de una sentencia
checkS :: Statement -> Checker ()
checkS (Assign v e) = checkE e >>=  registerT v
checkS (Block xs)   = combinarC (map checkS xs)
checkS (If e s1 s2) = checkE e >>= (\boolT -> if esBoolT boolT  then unify (checkS s1) (checkS s2) else fail "no es un BoolT")
checkS (While e s)  = checkE e >>= (\boolT -> if esBoolT boolT then checkS s else fail "no es un BoolT")
checkS (Print msg)  = impPrint msg
checkS (Fail msg)   = fail msg


esBoolT :: Type -> Bool
esBoolT (BoolT) = True
esBoolT _ = False


impPrint :: String -> Checker ()
impPrint s = Check (\st -> OkT () st)


combinarC :: [Checker ()] -> Checker ()
combinarC [] = return ()
combinarC (x:xs) =  x >>= (\v -> combinarC xs) 

-- Funci�n que checkea el tipo de un programa y retorna el mensaje de error si lo hay
checkP :: Program -> Maybe Message
checkP p = case (stepT (checkS p)) empty of
		ErrorT msg -> Just msg
		OkT _ _ -> Nothing




