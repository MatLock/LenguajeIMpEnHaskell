module IMPDef where

type Variable = String  -- tipo para los identificadores de variables

data Value    =  -- tipo para los valores procesados
       VInt Int   |  -- constructor para un valor entero
       VBool Bool    -- constructor para un valor booleano
       deriving Show

type Message  = String  -- tipo de los mensajes de error

data UnOp  =  -- tipo de los operadores unarios
       Neg |  -- invierte el signo de un entero
       Not    -- negación booleana

data BinOp =  -- tipo de los operadores binarios
       Add |  -- suma de enteros
       Sub |  -- resta de enteros
       Mul |  -- producto de enteros
       Div |  -- división de enteros
       Eq  |  -- comparación por igualdad
       Lt  |  -- comparación por menor estricto
       Gt  |  -- comparación por mayor estricto
       Or  |  -- disyunción lógica de booleanos
       And    -- conjunción lógica de booleanos

data Expression =  -- tipo de las Expressionesiones
     ConstI Int                      | -- expressionesión constante entera
     ConstB Bool                     | -- expressionesión constante booleana
     Var Variable                    | -- expressionesión variable (lectura de memoria)
     Un  UnOp Expression             | -- expressionesión de aplicación de un operador unario
     Bin BinOp Expression Expression   -- expressionesión de aplicación de un operador binario

data Statement =  -- tipo de las sentencias
     Assign Variable Expression        | -- asignación de una variable en memoria
     Block [Statement]                 | -- bloque de sentencias
     If Expression Statement Statement | -- construcción condicional
     While Expression Statement        | -- construcción de ciclo
     Print Message                     | -- impresión en la pantalla
     Fail Message                        -- reporte de un error

type Program = Statement  -- tipo de los programas IMP

