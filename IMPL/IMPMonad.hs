module IMPMonad where

import IMPDef
import Store

type Screen = String  -- tipo para representar el estado de la consola

data Execution v =  -- tipo del contexto de ejecución de un programa
     Ok v Screen (Store Value) | -- contexto exitoso (con memoria)
     Error Message               -- contexto de error (con mensaje)
     deriving Show

newtype Interpreter v =  -- tipo del estado de un interprete de un programa IMP
     Imp { step :: (Store Value) -> Execution v }

-- es lo mismo que decir:
-- data Interpreter v =  Imp (Store Value -> Execution v)

-- step :: Interpreter v -> (Store Value -> Execution v)
-- step (Imp f) = f

instance Monad Interpreter where  -- declaración del interprete como mónada
  fail msg = Imp (\ sv -> Error msg)
  return x = Imp (\ sv -> Ok x "" sv) 
  m >>= f  = Imp (\sv -> case (step m) sv  of
			         Ok v s sv' -> case (step (f v)) sv' of	
						Ok w s' sv'' -> Ok w (s++s') sv''
                        			Error m' -> Error m'                   
			         Error m -> Error m)



-- Función para el valor de una variable de la memoria
load :: Variable -> Interpreter Value
load v = Imp (\ sv -> case get sv v of
				Nothing -> Error "no esta en memoria"
				Just x -> Ok x "" sv )  

-- Función para escribir un valor de una variable en memoria
register ::Variable -> Value -> Interpreter ()
register s v  = Imp (\sv -> Ok () "" (set sv s v))  

-- Función para imprimir en la pantalla
impPrint :: String -> Interpreter ()
impPrint s = Imp (\sv -> Ok () s sv)



