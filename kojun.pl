:- use_module(library(clpfd)).

tabuleiro(1, [[(00-_), (00-_), (01-_), (01-_), (02-_), (03-_), (04-_), (04-_)],
              [(00-_), (00-1), (05-3), (01-_), (06-_), (03-_), (03-_), (04-_)],
              [(05-_), (05-_), (05-_), (07-_), (06-_), (08-3), (09-_), (09-_)],
              [(10-_), (10-_), (10-3), (07-_), (06-_), (08-_), (08-_), (09-_)],
              [(11-_), (07-5), (07-_), (07-3), (07-_), (08-_), (08-_), (09-_)],
              [(11-_), (12-2), (13-_), (13-_), (13-_), (14-_), (15-_), (09-_)],
              [(12-_), (12-_), (12-_), (12-_), (16-_), (14-_), (14-3), (14-_)],
              [(17-_), (16-_), (16-5), (16-3), (16-_), (14-_), (18-_), (18-_)]]).

%Pode ser lateral ou horizontal. É só transpor a matriz
check_adjacencia([]).
check_adjacencia([_|[]]).
check_adjacencia([_-Left,Region-Right|Tail]) :-
    %Valor a esquerda deve ser diferente do valor a direita.
    %Com a matriz transposta é possível fazer a mesma verficação
    %Para adjacencia vertical.
    Left #\= Right,
    %Recursividade até o final da linha/coluna
    check_adjacencia([Region-Right|Tail]).

%Regra de valor inferior/superior
check_regra_vertical([]).
check_regra_vertical([_|[]]).
check_regra_vertical([Regiao_superior-Valor_superior,Regiao_inferior-Valor_inferior|Tail]) :-
    %Regiao_superior diferente de Regiao_inferior OU Valor_superior > Valor_Inferior
    (Regiao_superior #\= Regiao_inferior) #\/ (Valor_superior #> Valor_inferior),
    %Recursividade até o final da linha
    check_regra_vertical([Regiao_inferior-Valor_inferior|Tail]).

region_max(Board, Region, Max) :-
    append(Board, FlatBoard), sort(FlatBoard, Sorted), group_pairs_by_key(Sorted, Groups), member(Region-Value, Groups), length(Value, Max).

limits(_, []).
limits(Board, [Region-Value|[]]) :-
    region_max(Board, Region, Max),
    Value in 1..Max.
limits(Board, [Region-Value, Next|Tail]) :-
    region_max(Board, Region, Max),
    Value in 1..Max,
    limits(Board, [Next|Tail]).

distinct(_-List) :- all_distinct(List).
distinct_by_region(Board) :-
    append(Board, FlatBoard),
    sort(FlatBoard, Sorted),
    group_pairs_by_key(Sorted, Groups),
    maplist(distinct, Groups).

value_only([], []).
value_only([H|T], [H2|T2]) :- pairs_values(H, H2), value_only(T, T2).

guess(Rows) :-
    value_only(Rows, Values),
    maplist(label, Values).

pretty(Rows) :- value_only(Rows, Values), maplist(portray_clause, Values).

kojun(Rows) :-
    maplist(limits(Rows), Rows),
    distinct_by_region(Rows),
    maplist(check_adjacencia, Rows),
    transpose(Rows, Columns),
    maplist(check_adjacencia, Columns),
    maplist(check_regra_vertical, Columns).

resolve(N) :-
    tabuleiro(N,Tab),
    kojun(Tab),
    guess(Tab),
    pretty(Tab).

