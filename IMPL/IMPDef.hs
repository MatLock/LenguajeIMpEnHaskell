module IMPDef where

type Variable = String  -- tipo para los identificadores de variables

data Value    =  -- tipo para los valores procesados
       VInt Int   |  -- constructor para un valor entero
       VBool Bool    -- constructor para un valor booleano
       deriving Show

type Message  = String  -- tipo de los mensajes de error

data UnOp  =  -- tipo de los operadores unarios
       Neg |  -- invierte el signo de un entero
       Not    -- negaci�n booleana

data BinOp =  -- tipo de los operadores binarios
       Add |  -- suma de enteros
       Sub |  -- resta de enteros
       Mul |  -- producto de enteros
       Div |  -- divisi�n de enteros
       Eq  |  -- comparaci�n por igualdad
       Lt  |  -- comparaci�n por menor estricto
       Gt  |  -- comparaci�n por mayor estricto
       Or  |  -- disyunci�n l�gica de booleanos
       And    -- conjunci�n l�gica de booleanos

data Expression =  -- tipo de las Expressionesiones
     ConstI Int                      | -- expressionesi�n constante entera
     ConstB Bool                     | -- expressionesi�n constante booleana
     Var Variable                    | -- expressionesi�n variable (lectura de memoria)
     Un  UnOp Expression             | -- expressionesi�n de aplicaci�n de un operador unario
     Bin BinOp Expression Expression   -- expressionesi�n de aplicaci�n de un operador binario

data Statement =  -- tipo de las sentencias
     Assign Variable Expression        | -- asignaci�n de una variable en memoria
     Block [Statement]                 | -- bloque de sentencias
     If Expression Statement Statement | -- construcci�n condicional
     While Expression Statement        | -- construcci�n de ciclo
     Print Message                     | -- impresi�n en la pantalla
     Fail Message                        -- reporte de un error

type Program = Statement  -- tipo de los programas IMP

