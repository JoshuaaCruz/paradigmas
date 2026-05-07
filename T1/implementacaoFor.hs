-- Padrão de for usado como referência (Java, C):
--  for (int i; funcCond(i, var); funcCont(i, var));
--      funcaoLoop(i, var);

-- Todas as funções inseridas no for terão que seguir o seguinte padrão de parâmetros:
-- funcCond(icional) :: Int -> t -> bool
-- funcCont(adora) :: Int -> t -> Int
-- funcLoop :: Int -> t -> t

for :: Int -> (Int -> t -> Bool) -> (Int -> t -> Int) -> (Int -> t -> t) -> t -> t

-- for None funcCond None funcLoop var = ...

-- for i funcCond None funcLoop var = ...

for i funcCond funcCont funcLoop var =
    if (funcCond i var) == True then
        for (funcCont i var) funcCond funcCont funcLoop (funcLoop i var)
    else
        var

-- retorna quantos valores tem numa lista
compr :: [t] -> Int

compr [] = 0

compr (a:b) = 1 + compr b

-- dobra um dos valores de uma lista de inteiros
dobrarValor :: Int -> [Int] -> [Int]

dobrarValor 0 (a:b) = [(a * 2)]++b
dobrarValor i (a:b) = [a]++(dobrarValor (i - 1) b)

main = do
    print(compr [1..10])
    let lista1 = [1..40]
    let lista2 = [1..40]
    print(dobrarValor 20 lista2)
    print(for 0 (\x y -> x < (compr y)) (\x y -> x + 1) dobrarValor lista2)
    print([5,(5-1) .. 0])