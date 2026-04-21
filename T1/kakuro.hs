type Bloco = (Int, Int, Int)
type Matriz = [[Bloco]]

matriz :: Matriz
matriz = [
    [(0, 0, -1), (12, 0, -1), (21, 0, -1), (0, 0, -1), (16, 0, -1), (13, 0, -1)],
    [(0, 17, -1), (0, 0, 0),   (0, 0, 0),   (22, 11, -1), (0, 0, 0),  (0, 0, 0)],
    [(0, 15, -1), (0, 0, 0),   (0, 0, 0),   (0, 0, 0),    (0, 0, 0),  (0, 0, 0)],
    [(0, 0, -1),  (4, 13, -1), (0, 0, 0),   (0, 0, 0),    (0, 0, 0),  (10, 0, -1)],
    [(0, 18, -1), (0, 0, 0),   (0, 0, 0),   (0, 0, 0),    (0, 0, 0),  (0, 0, 0)],
    [(0, 10, -1), (0, 0, 0),   (0, 0, 0),   (0, 14, -1),  (0, 0, 0),  (0, 0, 0)]
 ]
pegaPrimeiro :: Bloco -> Int
pegaPrimeiro (x, _, _) = x

pegaSegundo :: Bloco -> Int
pegaSegundo (_, y, _) = y

pegaTerceiro :: Bloco -> Int
pegaTerceiro (x, y, z) = z

getValor :: Matriz -> Int -> Int -> Int
getValor mat row col = pegaTerceiro ((mat !! row) !! col)

--quando achar o valor certo para um block
setValor :: Matriz -> Int -> Int -> Int -> Matriz
setValor mat row col valorAchado =
    let
        linha = mat !! row -- selecionamos a linha
        bloco = linha !! col -- selecionamos o bloco
        novoBloco = (pegaPrimeiro bloco, pegaSegundo bloco, valorAchado) -- criamos um novo bloco com o valor encontrado
        novaLinha = take col linha ++ [novoBloco] ++ drop (col + 1) linha -- criamos uma nova linha com o bloco atualizado
    in
        take row mat ++ [novaLinha] ++ drop (row + 1) mat -- criamos uma nova matriz com a linha atualizada

isValueBlock :: Matriz -> Int -> Int -> Bool
isValueBlock m r c = 
    let
        linha    = m !! r          -- selecionamos a linha 
        bloco    = linha !! c      -- selecionamos a coluna
        terceiro = pegaTerceiro bloco -- pegamos o valor que define o tipo do bloco
    in
        if terceiro == -1 then False else True


getTotalCol :: Matriz -> Int -> Int -> Int -- acima e abaixo
getTotalCol mat row col = somaAcima + somaAbaixo
    where
        tamanho = length mat

        somaAcima = getTotalColAcima mat row col (row - 1)

        somaAbaixo = getTotalColAbaixo mat row col (row + 1) tamanho

getTotalColAcima :: Matriz -> Int -> Int -> Int -> Int
getTotalColAcima mat row col rowAtual
    | rowAtual < 0 = 0 -- caso base, passamos do row 0
    | not (isValueBlock mat rowAtual col) = 0
    | otherwise = getValor mat rowAtual col + getTotalColAcima mat row col (rowAtual - 1) -- esta dentro limite e eh block de valor ent soma ele e chama rec blocks acima

getTotalColAbaixo :: Matriz -> Int -> Int -> Int -> Int -> Int
getTotalColAbaixo mat row col rowAtual tamanho --precisamos do tamanho pois vai ser nosso limite superior da matriz
    | rowAtual == tamanho = 0 --chegou no ultimo row
    | not (isValueBlock mat rowAtual col) = 0
    | otherwise = getValor mat rowAtual col + getTotalColAbaixo mat row col (rowAtual + 1) tamanho -- esta dentro limite e eh block de valor ent soma ele e chama rec blocks abaixo




getTotalRow :: Matriz -> Int -> Int -> Int -- esquerda e direita
getTotalRow mat row col = somaEsquerda + somaDireita
    where
        tamanho = length mat 

        somaEsquerda = getTotalRowEsquerda mat row col (col - 1)

        somaDireita = getTotalRowDireita mat row col (col + 1) tamanho

getTotalRowEsquerda :: Matriz -> Int -> Int -> Int -> Int
getTotalRowEsquerda mat row col colAtual
    | colAtual < 0 = 0 -- caso base, passamos da col 0
    | not (isValueBlock mat row colAtual) = 0
    | otherwise = getValor mat row colAtual + getTotalRowEsquerda mat row col (colAtual - 1) -- esta dentro limite e eh block de valor ent soma ele e chama rec blocks a esquerda

getTotalRowDireita :: Matriz -> Int -> Int -> Int -> Int -> Int
getTotalRowDireita mat row col colAtual tamanho --precisamos do tamanho pois vai ser nosso limite da direita da matriz
    | colAtual == tamanho = 0 --chegou na ultima col
    | not (isValueBlock mat row colAtual) = 0
    | otherwise = getValor mat row colAtual + getTotalRowDireita mat row col (colAtual + 1) tamanho -- esta dentro limite e eh block de valor ent soma ele e chama rec blocks a direita




isLastBlockRow :: Matriz -> Int -> Int -> Bool
isLastBlockRow mat row col
    | col == (length (mat !! row) - 1) = True -- caso seja o ultimo bloco da linha
    | not (isValueBlock mat row (col + 1)) = True --caso proximo bloco seja um bloco de dica
    | otherwise = False

isLastBlockCol :: Matriz -> Int -> Int -> Bool
isLastBlockCol mat row col
    | row == (length mat - 1) = True -- se esta no ultimo row ent nenhum block abaixo
    | not (isValueBlock mat (row + 1) col) = True --caso block abaixo seja dica
    | otherwise = False



getDicaCol :: Matriz -> Int -> Int -> Int
getDicaCol mat row col = getDicaColRec mat row col row

getDicaColRec :: Matriz -> Int -> Int -> Int -> Int
getDicaColRec mat row col rowAtual
    | not (isValueBlock mat rowAtual col) = pegaPrimeiro ((mat !! rowAtual) !! col) -- caso seja um bloco de dica, retorna a dica
    | otherwise = getDicaColRec mat row col (rowAtual - 1) --caso nao, chama rec bloco acima (row superior)

getDicaRow :: Matriz -> Int -> Int -> Int
getDicaRow mat row col = getDicaRowRec mat row col col

getDicaRowRec :: Matriz -> Int -> Int -> Int -> Int
getDicaRowRec mat row col colAtual
    | not (isValueBlock mat row colAtual) = pegaSegundo ((mat !! row) !! colAtual) -- caso seja um bloco de dica, retorna a dica
    | otherwise = getDicaRowRec mat row col (colAtual - 1) --caso nao, chama rec bloco a esquerda (coluna anterior)


-- inverte uma lista
inverte :: [t] -> [t]

inverte [] = []
inverte [t] = [t]

inverte (a:b) = (inverte b)++[a]

-- retorna quantos valores tem numa lista
compr :: [t] -> Int
compr [] = 0

compr (a:b) = 1 + len b

main = do
    print (inverte [1..10])
    print (len [1..10])