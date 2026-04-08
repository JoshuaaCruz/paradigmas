matriz :: [[(Int, Int, Int)]]
matriz = [
    [(0, 0, -1), (12, 0, -1), (21, 0, -1), (0, 0, -1), (16, 0, -1), (13, 0, -1)],
    [(0, 17, -1), (0, 0, 0),   (0, 0, 0),   (22, 11, -1), (0, 0, 0),  (0, 0, 0)],
    [(0, 15, -1), (0, 0, 0),   (0, 0, 0),   (0, 0, 0),    (0, 0, 0),  (0, 0, 0)],
    [(0, 0, -1),  (4, 13, -1), (0, 0, 0),   (0, 0, 0),    (0, 0, 0),  (10, 0, -1)],
    [(0, 18, -1), (0, 0, 0),   (0, 0, 0),   (0, 0, 0),    (0, 0, 0),  (0, 0, 0)],
    [(0, 10, -1), (0, 0, 0),   (0, 0, 0),   (0, 14, -1),  (0, 0, 0),  (0, 0, 0)]
 ]

pegaTerceiro :: (Int, Int, Int) -> Int
pegaTerceiro (x, y, z) = z

isValueBlock :: [[(Int, Int, Int)]] -> Int -> Int -> Bool
isValueBlock m r c = 
    let
        linha    = m !! r          -- selecionamos a linha 
        bloco    = linha !! c      -- selecionamos a coluna
        terceiro = pegaTerceiro bloco -- pegamos o valor que define o tipo do bloco
    in
        if terceiro == -1 then False else True
    
inverte :: [t] -> [t]

inverte [] = []
inverte [t] = [t]

inverte (a:b) = (inverte b)++[a]

main = do
    print (inverte [1..10])