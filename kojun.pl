:- use_module(library(clpfd)).
horizontal([]).
horizontal([_| []]).
horizontal([Left, Right| Row]) :- % faz a verificação horizontal
    different_val(Left, Right), % células adjacentes devem ter valores diferentes
    horizontal([Right| Row]).

vertical([]).
vertical([_| []]).
vertical([Higher, Lower| Column]) :- % faz a verificação vertical
    different_val(Higher, Lower), % células adjacentes devem ter valores diferentes
    vertical([Lower| Column]).

different_val(cell(_, Val1), cell(_, Val2)) :- % as duas células devem ter valores diferentes
    Val1 \= Val2.

adjacencias(Tab) :-
    maplist(horizontal, Tab),   % Faz as limitações dentro de cada linha
    transpose(Tab, TabColuna),   % Pega as colunas
    maplist(vertical, TabColuna).  % Faz as limitações dentro de cada coluna

list_regions(LCells, Regions) :-
    findall(Region, member(cell(Region, _), LCells), RegionsList),
    sort(RegionsList, Regions).

verify_region(LCells, Region) :-
    vals_in_region(LCells, Region, Vals),
    length(Vals, Len), 
    Vals ins 1..Len,    % Valores estão entre 1 e Len
    all_distinct(Vals), % Todos os valores tem q ser diferentes
    maplist(portray_clause, Vals).

kojun(Tab) :-     
    adjacencias(Tab),
    append(Tab, Celulas),
    list_regions(Celulas, Regioes),
    print(Regioes).
   
