# 3x3

#tupla x,y 
#x col abaixo, y row direita 

#0,0 0,9) 0,9) 0,9) 
# 0,9) 4 2 3
# 0,9) 2 3 4
# 0,8) 3 4 1

#seleciona um numero 9 até 1

#checagens:
#1. numero nao repetido nessa linha/coluna
#2. total da linha/coluna não ultrapassa a dica da tupla

#adiciona e parte para o próximo

#se nao for possivel, volta e tenta outro no passo anterior (backtracking) e tenta outro num

def isValueBlock(mat, row, col):
    if mat[row][col][2] == -1:
        return False
    return True

def getTotalCol(mat, row, col):
    total = 0
    for x in range( row + 1, len(mat)): #somo blocos abaixo
        if isValueBlock(mat, x, col):
            total += mat[x][col][2]
        else:
            break #quebra pois encontrou um bloco de dica (soma deve ser parada)
    
    for x in range( row - 1, -1 , -1): #somo blocos acima
        if isValueBlock(mat, x, col):
            total += mat[x][col][2]
        else:
            break #quebra pois encontrou um bloco de dica (soma deve ser parada)
    
    return total

def getTotalRow(mat, row, col):
    total = 0
    for x in range( col + 1, len(mat)): #somo blocos da direita
        if isValueBlock(mat, row, x):
            total += mat[row][x][2]
        else:
            break #quebra pois encontrou um bloco de dica (soma deve ser parada)
    
    for x in range( col - 1, -1 , -1): #somo blocos da esquerda
        if isValueBlock(mat, row, x):
            total += mat[row][x][2]
        else:
            break #quebra pois encontrou um bloco de dica (soma deve ser parada)
    
    return total

def isLastBlockRow(mat, row, col):
    if ( col == len(mat) - 1 ): #se estamos na ultima coluna dessa linha 
        return True
    elif mat[row][col + 1][2] == -1: # proximo bloco e uma dica ent eh o ultimo bloco do row
        return True
    
    return False #ambos casos acima falharem, ent ainda ha blocos adiante nessa linha


def isLastBlockCol(mat, row, col):
    if row == len(mat) - 1: #se estamos na ultima linha dessa coluna 
        return True
    elif mat[row + 1][col][2] == -1: # proximo bloco e uma dica ent eh o ultimo bloco do col
        return True

    return False

def isSafe(mat, rowAtual, colAtual, num):
    if not isValueBlock(mat, rowAtual, colAtual):
        return False

    else:

        # Check if num exists in the col or row
        # SEGMENTO (esquerda e direita)
        for i in range(colAtual - 1, -1, -1):
            if not isValueBlock(mat, rowAtual, i): break
            if mat[rowAtual][i][2] == num: return False
        for i in range(colAtual + 1, len(mat[0])):
            if not isValueBlock(mat, rowAtual, i): break
            if mat[rowAtual][i][2] == num: return False

        # Verificar repetição apenas no SEGMENTO (cima e baixo)
        for i in range(rowAtual - 1, -1, -1):
            if not isValueBlock(mat, i, colAtual): break
            if mat[i][colAtual][2] == num: return False
        for i in range(rowAtual + 1, len(mat)):
            if not isValueBlock(mat, i, colAtual): break
            if mat[i][colAtual][2] == num: return False

        
            
        #achar dica da coluna - vai pra cima até achar bloco de dica
        dicaCol = 0
        for i in range(len(mat)):
            if not isValueBlock(mat, rowAtual - i, colAtual):
                dicaCol = mat[rowAtual - i][colAtual][0] #position dica col e 0
                break

        #achar dica da linha - vai pra esquerda até achar bloco de dica
        dicaLin = 0
        for i in range(len(mat)):
            if not isValueBlock(mat, rowAtual, colAtual - i):
                dicaLin = mat[rowAtual][colAtual-i][1] #position dica col e 1
                break


        if getTotalCol(mat, rowAtual, colAtual) + num > dicaCol:
            return False

        if getTotalRow(mat, rowAtual, colAtual) + num > dicaLin:
            return False

        #sabemos que não vai ultrapassar a dia, mas se for o ultimo bloco daquele row ou col entao deve ser = a dica da row/col

        rowDicaMatch = True
        colDicaMatch = True

        #verificar se é o ultimo bloco da linha
        if isLastBlockRow(mat, rowAtual, colAtual):
            rowDicaMatch =  getTotalRow(mat, rowAtual, colAtual) + num == dicaLin #retorna comparacao com o valor dica do row
        
        #verificar se é o ultimo bloco da col
        if isLastBlockCol(mat, rowAtual, colAtual):
            colDicaMatch = getTotalCol(mat, rowAtual, colAtual) + num == dicaCol #retorna comparacao com o valor dica do col

        #Se passou por todos os passos entao é um num que pode ser colocado
        return rowDicaMatch and colDicaMatch

def solveKakuroRec(mat, rowAtual, colAtual):

    #caso base, se não há mais linhas para preencher no kakuro
    if rowAtual == len(mat):
        return True
    
    # se passou da ultima coluna disponivel chama a proxima row -len(mat) - sempre mat quadrada
    if colAtual == len(mat): 
        return solveKakuroRec(mat, rowAtual + 1, 0)

    # se encontro um bloco de dica eu só passo pra frente indo a direita
    if not isValueBlock(mat, rowAtual, colAtual):
        return solveKakuroRec(mat, rowAtual, colAtual + 1)
    
    #backtracking
    for num in range(1, 10): # de 1 a 9
        if isSafe(mat, rowAtual, colAtual, num):
            mat[rowAtual][colAtual][2] = num #achamos um num que pode ser colocado
            
            #chamamos o bloco seguinte, se ele tambem der certo vai retornar true, else não podemos seguir com esse num e vamos mudar
            if solveKakuroRec(mat, rowAtual, colAtual + 1):
                return True
            
            #se nao der certo, volta e tenta outro num (o backtracking)
            mat[rowAtual][colAtual][2] = 0

    return False #não encontrou nenhum num que deu certo, essa ramificação nao tem solução



def solveKakuro(mat):
    if solveKakuroRec(mat, 0, 0):
        print("Solução encontrada")
    else:
        print("Nenhuma solução encontrada")

                
if __name__ == "__main__":
    mat = [ 
    [[0, 0, -1], [0, 0, -1], [0, 0, -1], [21, 0, -1], [10, 0, -1], [0, 0, -1], [0, 0, -1], [0, 0, -1], [10, 0, -1], [15, 0, -1]],
    [[0, 0, -1], [0, 0, -1], [14, 16, -1], [0, 0, 0], [0, 0, 0], [9, 0, -1], [0, 0, -1], [26, 7, -1], [0, 0, 0], [0, 0, 0]],
    [[0, 0, -1], [6, 14, -1], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [21, 19, -1], [0, 0, 0], [0, 0, 0], [0, 0, 0]],
    [[0, 7, -1], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 29, -1], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, -1]],
    [[0, 7, -1], [0, 0, 0], [0, 0, 0], [12, 0, -1], [18, 19, -1], [0, 0, 0], [0, 0, 0], [0, 0, 0], [15, 0, -1], [0, 0, -1]],
    [[0, 0, -1], [0, 20, -1], [0, 0, 0], [0, 0, 0], [0, 0, 0], [22, 7, -1], [0, 0, 0], [0, 0, 0], [0, 0, 0], [7, 0, -1]],
    [[0, 0, -1], [0, 0, -1], [7, 8, -1], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, -1], [20, 4, -1], [0, 0, 0], [0, 0, 0]],
    [[0, 0, -1], [8, 27, -1], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [8, 23, -1], [0, 0, 0], [0, 0, 0], [0, 0, 0]],
    [[0, 8, -1], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 14, -1], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, -1]],
    [[0, 4, -1], [0, 0, 0], [0, 0, 0], [0, 0, -1], [0, 0, -1], [0, 16, -1], [0, 0, 0], [0, 0, 0], [0, 0, -1], [0, 0, -1]]
]

    solveKakuro(mat)

    for row in mat:
        print(" ".join(map(str, row)))