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

existeNaLinha :: Matriz -> Int -> Int -> Int -> Bool
existeNaLinha mat row col num = existeEsq mat row (col - 1) num || existeDir mat row (col + 1) num
  where
    tamanho = length (mat !! row)
    
    existeEsq m r c n
        | c < 0 = False -- saiu da matriz pela esquerda
        | not (isValueBlock m r c) = False -- bateu num bloco de dica, para de procurar
        | getValor m r c == n = True -- achou o numero repetido
        | otherwise = existeEsq m r (c - 1) n -- continua olhando pra esquerda
        
    existeDir m r c n
        | c == tamanho = False -- saiu da matriz pela direita
        | not (isValueBlock m r c) = False -- bateu num bloco de dica
        | getValor m r c == n = True -- achou repetido
        | otherwise = existeDir m r (c + 1) n -- continua olhando pra direita

existeNaColuna :: Matriz -> Int -> Int -> Int -> Bool
existeNaColuna mat row col num = existeCima mat (row - 1) col num || existeBaixo mat (row + 1) col num
  where
    tamanho = length mat
    
    existeCima m r c n
        | r < 0 = False -- saiu por cima
        | not (isValueBlock m r c) = False -- achou bloco de dica
        | getValor m r c == n = True -- ja tem o numero acima
        | otherwise = existeCima m (r - 1) c n
        
    existeBaixo m r c n
        | r == tamanho = False -- saiu por baixo
        | not (isValueBlock m r c) = False -- achou bloco de dica
        | getValor m r c == n = True -- ja tem o numero embaixo
        | otherwise = existeBaixo m (r + 1) c n

isSafe :: Matriz -> Int -> Int -> Int -> Bool
isSafe mat row col num
    | not (isValueBlock mat row col) = False -- so pode por numero em bloco de valor
    | existeNaLinha mat row col num = False -- se ja tem o numero na linha, nao eh safe
    | existeNaColuna mat row col num = False -- se ja tem na coluna, nao eh safe
    | otherwise =
        let 
            dicaCol = getDicaCol mat row col -- pega o valor alvo da coluna
            dicaLin = getDicaRow mat row col -- pega o valor alvo da linha
            somaCol = getTotalCol mat row col + num -- soma atual + numero novo
            somaLin = getTotalRow mat row col + num
            
            -- se for o ultimo bloco da sequencia, a soma tem que ser igual a dica
            -- se nao for o ultimo, a soma so nao pode estourar (ser menor ou igual)
            okSomaCol = if isLastBlockCol mat row col then somaCol == dicaCol else somaCol <= dicaCol
            okSomaLin = if isLastBlockRow mat row col then somaLin == dicaLin else somaLin <= dicaLin
        in 
            okSomaCol && okSomaLin

solveKakuroRec :: Matriz -> Int -> Int -> Maybe Matriz
solveKakuroRec mat row col
    | row == length mat = Just mat -- percorreu tudo
    | col == length (mat !! row) = solveKakuroRec mat (row + 1) 0 -- fim da linha, vai pra proxima
    | not (isValueBlock mat row col) = solveKakuroRec mat row (col + 1) -- pula bloco de dica
    | getValor mat row col /= 0 = solveKakuroRec mat row (col + 1) -- pula se ja tiver numero
    | otherwise = tentaNumeros mat row col 1 -- tenta encaixar de 1 a 9

tentaNumeros :: Matriz -> Int -> Int -> Int -> Maybe Matriz
tentaNumeros mat row col num
    | num > 9 = Nothing -- testou todos e nenhum serviu, volta o backtracking
    | isSafe mat row col num = 
        let 
            novaMatriz = setValor mat row col num -- coloca o numero
            resultadoDaFrente = solveKakuroRec novaMatriz row (col + 1) -- tenta o proximo passo
        in 
            case resultadoDaFrente of
                Just solucao -> Just solucao -- deu certo ate o fim
                Nothing -> tentaNumeros mat row col (num + 1) -- deu erro na frente, tenta o proximo num
    | otherwise = tentaNumeros mat row col (num + 1) -- numero atual nao serve, tenta proximo

inverte :: [t] -> [t]
inverte [] = []
inverte (a:b) = inverte b ++ [a] -- inverte a lista

-- retorna quantos valores tem numa lista
compr :: [t] -> Int
compr [] = 0
compr (_:b) = 1 + compr b 

imprimeMatriz :: Matriz -> IO ()
imprimeMatriz [] = return ()
imprimeMatriz (linha:linhas) = do
    print linha 
    imprimeMatriz linhas 

main :: IO ()
main = do
    print (inverte [1..10])
    print (compr [1..10])
    
    putStrLn "\nResolvendo o Kakuro...\n"
    
    case solveKakuroRec matriz 0 0 of
        Just solucao -> do
            putStrLn "Solucao encontrada:"
            imprimeMatriz solucao 
        Nothing -> putStrLn "Nenhuma solucao foi encontrada." 