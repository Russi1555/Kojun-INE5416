:- use_module(library(clpfd)).

%tabuleiro com 2+ soluções: retorna uma solução apenas.
tabuleiro(-1, [[(00-_),(00-_)],
               [(01-_),(01-_)]]).

%tabuleiro sem solução/inválido: retorna falso
tabuleiro(0,[[(00-1),(00-1)]
             [(01-_),(01-2)]]).

%https://www.janko.at/Raetsel/Kojun/001.a.htm
tabuleiro(1, [[(00-2),(00-_),(01-_),(01-_),(01-1),(02-_)],
               [(03-_),(03-_),(03-_),(03-3),(03-_),(02-_)],
               [(04-_),(05-3),(05-_),(05-_),(03-5),(06-3)],
               [(04-_),(04-_),(04-_),(05-_),(06-_),(06-_)],
               [(07-_),(07-_),(08-3),(09-_),(09-4),(09-2)],
               [(10-_),(10-_),(08-_),(08-_),(09-_),(09-_)]]).

%https://www.janko.at/Raetsel/Kojun/010.a.htm
tabuleiro(10, [ [(00-5), (01-_), (01-2), (01-_), (02-2), (02-_), (02-3), (02-1), (03-3), (03-1)],
               [(00-_), (00-4), (00-_), (01-1), (04-_), (04-5), (05-_), (05-5), (03-_), (05-4)],
               [(06-7), (06-5), (00-1), (04-7), (04-_), (07-_), (08-3), (05-1), (05-3), (05-_)],
               [(06-_), (06-4), (04-_), (04-_), (09-_), (07-_), (08-_), (08-_), (08-_), (10-3)],
               [(06-2), (06-_), (06-3), (04-4), (09-_), (09-2), (11-_), (10-_), (10-4), (10-_)],
               [(12-5), (12-_), (13-2), (13-_), (13-6), (09-_), (14-_), (14-_), (15-_), (15-_)],
               [(12-_), (12-1), (12-3), (13-_), (13-1), (16-_), (17-_), (18-4), (15-_), (15-3)],
               [(19-6), (19-7), (12-_), (13-3), (16-_), (16-1), (17-4), (18-_), (20-_), (20-1)],
               [(19-4), (19-_), (21-3), (21-_), (21-4), (21-_), (17-_), (18-_), (18-_), (22-3)],
               [(19-_), (19-1), (19-_), (21-2), (21-_), (21-6), (17-2), (17-_), (22-2), (22-1)]]).

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

maximo_regiao(Tab, Regiao, Max) :-
    append(Tab, Linha_unica), %Concatena todas as linhas do tabuleiro em uma única linha
    sort(Linha_unica, Linha_ordenada), %Ordena em ordem crescente todos os itens da lista
    group_pairs_by_key(Linha_ordenada, Grupos), %Agrupa os itens da lista ordenada de acordo com o valor da região
    member(Regiao-Valor, Grupos), %Pega as tuplas região-valor de dada região
    length(Valor, Max). %valor máximo possível da região é o tamanho da lista de tuplas.

%limits(_, []). %condição de parada da recursão (acho que é desnecessário)
limites(Tab, [Regiao-Valor|[]]) :- %tratamento da última célula do tabuleiro
    maximo_regiao(Tab, Regiao, Max),
    Valor in 1..Max.
limites(Tab, [Regiao-Valor, Next|Tail]) :- %recursividade para buscar os limites da célula
    maximo_regiao(Tab, Regiao, Max), %chamamos maximo_regiao para encontrar o valor máximo da célula de acordo com a região
    Valor in 1..Max, %estabelecemos que Valor deve estar entre 1 e Max
    limites(Tab, [Next|Tail]). %chamada recursiva de limites.

distintos(_-List) :- all_distinct(List). %retorna verdadeiro se não há repetições na lista recebida
check_regra_repetição(Tab) :- %distinct sendo chamado para verificar a regra 
    append(Tab, Linha_unica), %Tabuleiro vira uma linha única ao inves de uma matriz
    sort(Linha_unica, Linha_ordenada), %linha é ordenada em ordem crescente
    group_pairs_by_key(Linha_ordenada, Grupos), % são separados por regiões
    maplist(distintos, Grupos). %distintos é aplicado em todas as regiões

get_valores([], []). %condição de parada da recursão
get_valores([H|T], [H2|T2]) :-  %H = tupla atual, T = cauda da lista, H2 = lista de valores de retorno, T2 = Cauda da lista de retorno
    pairs_values(H, H2), %coloca o valor de H em H2
    get_valores(T, T2). %chamada recursiva de get_valores

chutes(Linhas) :-
    get_valores(Linhas, Valores), %pega a lista de valores
    %aplica o predicado imbutido "label" na lista de valores. atribui uma solução válida para cada valor vazio da lista
    %de acordo com as restrições estabelecidas anteriormente.
    maplist(label, Valores).

printa_tabuleiro(Linhas) :- 
    get_valores(Linhas, Valores), 
    maplist(portray_clause, Valores).

kojun(Linhas) :-
    maplist(limites(Linhas), Linhas),
    check_regra_repetição(Linhas),
    maplist(check_adjacencia, Linhas),
    transpose(Linhas, Colunas),
    maplist(check_adjacencia, Colunas),
    maplist(check_regra_vertical, Colunas).

resolve(N) :-
    tabuleiro(N,Tab), %Escolhe o tabuleiro
    kojun(Tab), %Estabelece regras
    chutes(Tab), %Chuta valores/backtracking
    printa_tabuleiro(Tab). %exibe o tabuleiro resolvido.
