module Store (Store, empty, get, set, del, join, fromJust) where

data Store a = Store [String]               (String->a)
                  -- Dominio de la funcion  Función de strings a valores

instance Show a => Show (Store a) where  -- declaración del Store como instancia de la clase Show
  show (Store dom f) = "{" ++ showRep dom f ++ "}"

showRep [] f     = ""
showRep (k:ks) f = k ++ " -> "  ++ (show (f k)) ++" , " ++ showRep ks f          

-- Expresión que denota on Store vacío.
empty :: Store a
empty = Store [] (\ s -> undefined)

get :: Store a -> String -> Maybe a
get (Store xs f) a = get' xs f a
		where
		get' :: [String] -> (String -> a) -> String -> Maybe a
		get' [] f a = Nothing
		get' (x:xs) f a | x == a = Just (f x)
 				| otherwise = get' xs f a 


-- Asocia un valor a un String en un Store dado.

set :: Store a -> String -> a -> Store a
set (Store [] f) s v = Store [s] (\s -> v)
set e@(Store ss f) s v = case get e s of
			       Nothing -> Store (s:ss) (\k -> if k==s  then v  else f k  )
			       Just x -> Store  ss (\k -> if k==s then v else f k)

delete:: String -> [String] -> [String]
delete s [] = []
delete s (x:xs) | s == x = xs
		| otherwise = x: delete s xs

existeVEnDom ::[String] -> String -> Bool
existeVEnDom [] _ = False
existeVEnDom (x:xs) s  = s == x || existeVEnDom xs s 

	
-- Elimina la asociación con un String.
del :: Store a -> String -> Store a
del (Store ss f) s= Store (delete s ss) f 

fromJust :: Maybe a -> a
fromJust (Just x) = x

-- totalmente conciente que es una cagada, pero te juro que lo pensé con subtareas y no lo podia hacer, porque necesito todos las partes
-- Une dos Stores dada una función de unificación para valores con claves iguales.
join :: (a -> a -> Maybe a) -> Store a -> Store a -> Maybe (Store a)
join g (Store [] f) e1@(Store zs j) = Just e1  
join g e@(Store (x:xs) f) e1@(Store zs j) | existeVEnDom zs x = case g (fromJust (get e x)) (fromJust (get e1 x)) of
								 Just v ->  Just( set (fromJust (join g (Store xs f) (Store(delete x zs) j))) x v)  
								 Nothing -> Nothing
                                          | otherwise = Just (set (fromJust (join g (Store xs f) e1))  x (fromJust (get e x)) )   	



-- funcion para probar el join
g :: (Integer -> Integer -> Maybe Integer)
g x y = Just (x + y) 


f "a" = 2
f "b" = 3
f "p" = 1

h = Store ["a","b","p"] f

q "a" = 4
q "b" = 2


j = Store ["a","b"] q


