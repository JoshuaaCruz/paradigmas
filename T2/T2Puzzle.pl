%criando regras gerais que servem para qualquer puzzle

:- use_module(library(clpfd)).

%regras de tamanho da matriz
length(Rows,9) %Rows -lista exterior- temq ter tamanho 9
maplist(same_length(Rows),Rows) %verifica se todas as listas tem o mesmo tamanho

%all_distinct vem do clpfd, garantir que nao haja duplicatas
maplist(all_distinct, Rows),
transpose(Rows, Columns), %transposicao da matriz/Rows para Columns
maplist(all_distinct, Columns), %verificando nas Columns se nao tem duplicatas