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

kojun(Board) :-     
    maplist(horizontal, Board),   % Faz as limitações dentro de cada linha
    
    transpose(Board, BoardCol),   % Pega as colunas
    
    maplist(vertical, BoardCol).  % Faz as limitações dentro de cada coluna
   