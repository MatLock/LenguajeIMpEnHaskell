module Parser where

data ParserState a b = ParserState
  { input :: [a], index :: Int, output :: [b], size :: Int, valid :: Bool }

liftps :: [a] -> ParserState a b
liftps xs = ParserState { input = xs, index = 0, output = [], size = 0, valid = True }

next :: ParserState a b -> a
next = head . input

skip :: ParserState a b -> ParserState a b
skip ps = ps { input = tail (input ps), index = 1 + index ps }

push :: b -> ParserState a b -> ParserState a b
push b ps = ps { output = b : output ps, size = size ps + 1 }

popn :: Int -> ParserState a b -> ParserState a b
popn n ps = ps { output = drop n (output ps), size = size ps - n }

infixl 1 ?
(?) :: ParserState a b -> Parser a b -> ParserState a b
(?) ps p = if valid ps then p ps else ps


type Parser a b = ParserState a b -> ParserState a b

satisfy :: (a -> Bool) -> Parser a b
satisfy f ps = if not (null (input ps)) && f (next ps)
               then skip ps else ps { valid = False }

terminal :: Eq a => a -> Parser a b
terminal a = satisfy (a==)

range :: Ord a => a -> a -> Parser a b
range inf sup = satisfy (\c -> inf <= c && c <= sup)

choice :: Parser a b -> Parser a b -> Parser a b
choice p1 p2 ps = let { ps1 = p1 ps; ps2 = p2 ps }
                   in if valid ps1 then ps1 else ps2

succession :: Parser a b -> Parser a b -> Parser a b
succession p1 p2 ps = let { ps1 = p1 ps; ps2 = p2 ps1 }
                       in ps1 ? const ps2

closure :: Parser a b -> Parser a b
closure p1 ps = closure' ps
  where closure' ps0 = let ps1 = p1 ps0
                        in if valid ps1 then closure' ps1 else ps0

process :: ([a] -> b) -> Parser a b -> Parser a b
process f p1 ps = let { ps1 = p1 ps; n = index ps1 - index ps }
                   in ps1 ? push (f (take n $ input ps))

derive :: ([b] -> b) -> Parser a b -> Parser a b
derive f p1 ps = let { ps1 = p1 ps; n = size ps1 - size ps }
                  in ps1 ? push (f (take n $ output ps1)) . (popn n)

chain :: Eq a => [a] -> Parser a b
chain [] = id
chain xs = foldr1 succession (map terminal xs)

end :: Parser a b
end ps = if null (input ps) then ps else ps { valid = False }

match :: Parser a b -> [a] -> Bool
match p xs = valid $ (succession p end) (liftps xs)

parse :: Parser a b -> [a] -> b
parse p xs = let ps = p (liftps xs)
              in if valid ps && not (null $ output ps)
                 then head (output ps)	
                 else error "parsing error"

infix 9 #
(#) :: Eq a => [a] -> Parser a b
(#) = chain

infix 8 ×
(×) = closure

infix 8 ·×
(·×) p = p `succession` (×)p

infixl 7 ·
(·) :: Parser Char b -> Parser Char b -> Parser Char b
(·) p1 p2 = succession p1 (succession (closure (choice (terminal ' ') (terminal '\n'))) p2)

infixl 6 ¦
(¦) = choice

infixl 6 ·¦
(·¦) p1 p2 = p1 · (p2 ¦ id)

infixl 5 ·>
(·>) = flip process

infixl 4 >·
(>·) = flip derive

