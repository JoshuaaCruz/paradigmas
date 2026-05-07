compr :: [t] -> Int
compr [] = 0
compr (a:b) = 1 + compr b

get :: Int -> [t] -> t
get 0 (a:b) = a
get n (a:b) = get (n - 1) b

pegarTerceiro :: (t, t, t) -> t
pegarTerceiro (_, _, a) = a

pegarSegundo :: (t, t, t) -> t
pegarSegundo (_, a, _) = a

pegarPrimeiro :: (t, t, t) -> t
pegarPrimeiro (a, _, _) = a

splitRight :: Int -> [t] -> [t]
splitRight 0 (a:b) = b
splitRight num (a:b) = splitRight (num - 1) b

splitLeft :: Int -> [t] -> [t]
splitLeft 0 (a:b) = []
splitLeft num (a:b) = [a] ++ splitLeft (num - 1) b

alterarValor :: Int -> Int -> Int -> [[(Int, Int, Int)]] -> [[(Int, Int, Int)]]
alterarValor row col num mat = (splitLeft row mat) ++ [(splitLeft col (get row mat)) ++ [((pegarPrimeiro (get col (get row mat))), (pegarSegundo (get col (get row mat))), num)] ++ (splitRight col (get row mat))] ++ (splitRight row mat)

-- próprio
isValueBlock :: [[(Int, Int, Int)]] -> Int -> Int -> Bool
isValueBlock mat row col =
    if pegarTerceiro (get col (get row mat)) == -1 then
        False
    else
        True

-- próprio
isLastBlockRow :: [[(Int, Int, Int)]] -> Int -> Int -> Bool
isLastBlockRow mat row col =
    if col == (compr (get row mat) - 1) then
        True
    else
        if pegarTerceiro (get (col + 1) (get row mat)) == -1 then
            True
         else
            False

-- próprio
isLastBlockCol :: [[(Int, Int, Int)]] -> Int -> Int -> Bool
isLastBlockCol mat row col =
    if row == ((compr mat) - 1) then
        True
    else
        if pegarTerceiro (get col (get (row + 1) mat)) == -1 then
            True
         else
            False

-- auxíliar de getTotalRow
totalSumForRow :: [Int] -> [[(Int, Int, Int)]] -> Int -> Int -> Int -> Int
totalSumForRow [] mat row col var = var
totalSumForRow (a:b) mat row col var =
    if (isValueBlock mat row a) == True then
        totalSumForRow b mat row col (var + pegarTerceiro (get a (get row mat)))
    else
        var

-- auxíliar de getTotalCol
totalSumForCol :: [Int] -> [[(Int, Int, Int)]] -> Int -> Int -> Int -> Int
totalSumForCol [] mat row col var = var
totalSumForCol (a:b) mat row col var =
    if (isValueBlock mat a col) == True then
        totalSumForCol b mat row col (var + pegarTerceiro (get col (get a mat)))
    else
        var

-- próprio
getTotalRow :: [[(Int, Int, Int)]] -> Int -> Int -> Int
getTotalRow mat row col = totalSumForRow ([(col - 1), (col - 2) .. 0]) mat row col (totalSumForRow [(col + 1)..((compr (get row mat)) - 1)] mat row col 0)

-- próprio
getTotalCol :: [[(Int, Int, Int)]] -> Int -> Int -> Int
getTotalCol mat row col = totalSumForCol ([(row - 1), (row - 2) .. 0]) mat row col (totalSumForCol [(row + 1)..((compr mat) - 1)] mat row col 0)

-- próprio
isSafe :: [[(Int, Int, Int)]] -> Int -> Int -> Int -> Bool
isSafe mat rowAtual colAtual num =
    if (not (isValueBlock mat rowAtual colAtual)) == True then
        False
    else
        if (checkNumForRow [(colAtual - 1),(colAtual - 2) .. 0] mat rowAtual colAtual num) == False then
            False
        else
            if (checkNumForRow [(colAtual + 1)..((compr (get 0 mat)) - 1)] mat rowAtual colAtual num) == False then
                False
            else
                if (checkNumForCol [(rowAtual - 1),(rowAtual - 2) .. 0] mat rowAtual colAtual num) == False then
                    False
                else
                    if (checkNumForCol [(rowAtual + 1)..((compr mat) - 1)] mat rowAtual colAtual num) == False then
                        False
                    else
                        isSafe1 mat rowAtual colAtual num

-- auxíliar de isSafe
isSafe1 :: [[(Int, Int, Int)]] -> Int -> Int -> Int -> Bool
isSafe1 mat rowAtual colAtual num =
    if ((getTotalCol mat rowAtual colAtual) + num) > (dicaColFor [rowAtual, (rowAtual - 1) .. 0] mat rowAtual colAtual) then
        False
    else
        if ((getTotalRow mat rowAtual colAtual) + num) > (dicaRowFor [colAtual, (colAtual - 1) .. 0] mat rowAtual colAtual) then
            False
        else
            ((rowDicaMatch mat rowAtual colAtual num) && (colDicaMatch mat rowAtual colAtual num))

-- auxíliar de isSafe1
rowDicaMatch :: [[(Int, Int, Int)]] -> Int -> Int -> Int -> Bool
rowDicaMatch mat row col num =
    if (isLastBlockRow mat row col) == True then
        ((getTotalRow mat row col) + num) == (dicaRowFor [col, (col - 1) .. 0] mat row col)
    else
        True

-- auxíliar de isSafe1
colDicaMatch :: [[(Int, Int, Int)]] -> Int -> Int -> Int -> Bool
colDicaMatch mat row col num =
    if (isLastBlockCol mat row col) == True then
        ((getTotalCol mat row col) + num) == (dicaColFor [row, (row - 1) .. 0] mat row col)
    else
        True

-- auxíliar de isSafe1
dicaColFor :: [Int] -> [[(Int, Int, Int)]] -> Int -> Int -> Int
dicaColFor [] mat row col = 0
dicaColFor (a:b) mat row col =
    if (not (isValueBlock mat a col)) == True then
        pegarPrimeiro (get col (get a mat))
    else
        dicaColFor b mat row col

-- auxíliar de isSafe1
dicaRowFor :: [Int] -> [[(Int, Int, Int)]] -> Int -> Int -> Int
dicaRowFor [] mat row col = 0
dicaRowFor (a:b) mat row col =
    if (not (isValueBlock mat row a)) == True then
        pegarSegundo (get a (get row mat))
    else
        dicaRowFor b mat row col

-- auxíliar de isSafe
checkNumForRow :: [Int] -> [[(Int, Int, Int)]] -> Int -> Int -> Int -> Bool
checkNumForRow [] mat row col num = True
checkNumForRow (a:b) mat row col num =
    if (not (isValueBlock mat row a)) == True then
        True
    else
        if (pegarTerceiro (get a (get row mat))) == num then
            False
        else
            checkNumForRow b mat row col num

-- auxíliar de isSafe
checkNumForCol :: [Int] -> [[(Int, Int, Int)]] -> Int -> Int -> Int -> Bool
checkNumForCol [] mat row col num = True
checkNumForCol (a:b) mat row col num =
    if (not (isValueBlock mat a col)) == True then
        True
    else
        if (pegarTerceiro (get col (get a mat))) == num then
            False
        else
            checkNumForCol b mat row col num

-- próprio
solveKakuroRec :: [[(Int, Int, Int)]] -> Int -> Int -> [[(Int, Int, Int)]]
solveKakuroRec mat rowAtual colAtual =
    if rowAtual == compr mat then
        mat
    else
        if colAtual == compr mat then
            solveKakuroRec mat (rowAtual + 1) 0
        else
            if (not (isValueBlock mat rowAtual colAtual)) == True then
                solveKakuroRec mat rowAtual (colAtual + 1)
            else
                backtrackingForStep [1..9] mat rowAtual colAtual

-- auxíliar de solveKakuroRec
backtrackingForStep :: [Int] -> [[(Int, Int, Int)]] -> Int -> Int -> [[(Int, Int, Int)]]
backtrackingForStep [] mat rowAtual colAtual = [[(-1, -1, -1)]]
backtrackingForStep (a:b) mat rowAtual colAtual =
    if isSafe mat rowAtual colAtual a then
        let resultado = solveKakuroRec (alterarValor rowAtual colAtual a mat) rowAtual (colAtual + 1)
                in if resultado /= [[(-1, -1, -1)]]
                   then resultado
                   else backtrackingForStep b (alterarValor rowAtual colAtual 0 mat) rowAtual colAtual
    else
        backtrackingForStep b mat rowAtual colAtual

solveKakuro :: [[(Int, Int, Int)]] -> [[(Int, Int, Int)]]
solveKakuro mat = solveKakuroRec mat 0 0

matriz :: [[(Int, Int, Int)]]
matriz =
    [ [(0, 0, -1), (0, 0, -1), (0, 0, -1), (21, 0, -1), (10, 0, -1), (0, 0, -1), (0, 0, -1), (0, 0, -1), (10, 0, -1), (15, 0, -1)]
        , [(0, 0, -1), (0, 0, -1),(14, 16,-1), (  0, 0, 0), ( 0, 0,  0), (9, 0, -1), (0, 0, -1), (26, 7, -1), ( 0, 0,  0), ( 0, 0,  0)]
        , [(0, 0, -1), (6,14, -1), (0, 0,  0), (  0, 0, 0), ( 0, 0,  0), (0, 0,  0), (21,19,-1), ( 0, 0,  0), ( 0, 0,  0), ( 0, 0,  0)]
        , [(0, 7, -1), (0, 0,  0), (0, 0,  0), ( 0, 0,  0), ( 0,29, -1), (0, 0,  0), (0, 0,  0), ( 0, 0,  0), ( 0, 0,  0), ( 0, 0, -1)]
        , [(0, 7, -1), (0, 0,  0), (0, 0,  0), ( 12, 0,-1), (18,19, -1), (0, 0,  0), (0, 0,  0), ( 0, 0,  0), (15, 0, -1), ( 0, 0, -1)]
        , [(0, 0, -1), (0,20, -1), (0, 0,  0), ( 0, 0,  0), ( 0, 0,  0), (22, 7,-1), (0, 0,  0), ( 0, 0,  0), ( 0, 0,  0), ( 7, 0, -1)]
        , [(0, 0, -1), (0, 0, -1), (7, 8, -1), ( 0, 0,  0), ( 0, 0,  0), (0, 0,  0), (0, 0, -1), (20, 4, -1), ( 0, 0,  0), ( 0, 0,  0)]
        , [(0, 0, -1), (8,27, -1), (0, 0,  0), ( 0, 0,  0), ( 0, 0,  0), (0, 0,  0), ( 8,23,-1), ( 0, 0,  0), ( 0, 0,  0), ( 0, 0,  0)]
        , [(0, 8, -1), (0, 0,  0), (0, 0,  0), ( 0, 0,  0), ( 0,14, -1), (0, 0,  0), (0, 0,  0), ( 0, 0,  0), ( 0, 0,  0), ( 0, 0, -1)]
        , [(0, 4, -1), (0, 0,  0), (0, 0,  0), ( 0, 0, -1), ( 0, 0, -1), (0,16, -1), (0, 0,  0), ( 0, 0,  0), ( 0, 0,  -1), ( 0, 0, -1)]
        ]

main = do
    print(solveKakuro matriz)


