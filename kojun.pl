:- use_module(library(clpfd)).

%Pode ser lateral ou horizontal. É só transpor a matriz
check_adjacencia([]).
check_adjacencia([_|[]]).
check_adjacencia([_-Left,Region-Right|Tail]) :-
    %Valor a esquerda deve ser diferente do valor a direita.
    %Com a matriz transposta é possível fazer a mesma verficação
    %Para adjacencia vertical.
    Left #\= Right,
    %Recursividade até o final da linha/coluna
    adjacencia([Region-Right|Tail]).

%Regra de valor inferior/superior
check_regra_vertical([]).
check_regra_vertical([_|[]]).
check_regra_vertical([Regiao_superior-Valor_superior,Regiao_inferior-Valor_inferior|Tail]) :-
    %Regiao_superior diferente de Regiao_inferior OU Valor_superior > Valor_Inferior
    (Regiao_superior #\= Regiao_inferior) #\/ (Valor_superior #> Valor_inferior),
    %Recursividade até o final da linha
    check_regra_vertical([Regiao_inferior-Valor_inferior|Tail]).
