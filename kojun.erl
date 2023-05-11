% hello world program
-module(kojun). 
-export([start/0]). 

substitui_valor_lista(Lista,Indice,Valor)->
    % poderia ter usado isso lists:enumerate(Lista) mas não consegui atualizar meu Erlang
    Lista_enumerada = lists:zip(lists:seq(1,length(Lista)), Lista), 
    [if I == Indice -> Valor; true -> Val end || {I, Val} <- Lista_enumerada].


substitui_valor_matrix(Tab,{X,Y},Valor_novo)->
    Linha = lists:nth(X,Tab),
    Nova_Linha = substitui_valor_lista(Linha,Y,Valor_novo),
    substitui_valor_lista(Tab,X,Nova_Linha).

get_regiao_size(Reg, {X, Y}) ->
  Value = get_valor_coord(Reg, {X,Y}),
  Count = length([C || R <- Reg, C <- R, C =:= Value]),
  Count.

get_valor_coord(Matrix, {X,Y}) -> 
    lists:nth(Y, lists:nth(X, Matrix)).

get_coords_vazias(Tab)->
    Lim = length(Tab),
    Coords = [{Row, Col} || Row <- lists:seq(1, Lim), Col <- lists:seq(1, Lim)],
    Coords_zero = lists:filter(fun({X,Y})->get_valor_coord(Tab,{X,Y}) == 0 end, Coords),
    Coords_zero.

valoresRegiao(Tab, Reg, R) ->
    Coords = coordsRegiao(Reg, R),
    [lists:nth(J, lists:nth(I, Tab)) || {I,J} <- Coords].

coordsRegiao(Reg, Reg_id) ->
    Rows = length(Reg),
    Cols = length(lists:nth(1, Reg)),
    Coords = [{X,Y} || X <- lists:seq(1, Rows), Y <- lists:seq(1, Cols)],
    lists:filter(fun({X,Y}) -> get_valor_coord(Reg, {X,Y}) == Reg_id end, Coords).

getAdjacentCells(Tab,{X,Y}) -> 
    Coord_Up = {X-1,Y},
    Coord_Down = {X+1,Y},
    Coord_Left = {X,Y-1},
    Coord_Right = {X,Y+1},
    if
        (X>1)-> 
            Up = get_valor_coord(Tab,Coord_Up);
        true->
            Up = 0
    end,

    if
        (X < length(Tab))->
            Down = get_valor_coord(Tab,Coord_Down);
        true->
            Down = 0
    end,

    if 
        (Y < length(Tab))->
            Right = get_valor_coord(Tab,Coord_Right);
        true->
            Right = 0
    end,

    if 
        (Y>1)->
            Left = get_valor_coord(Tab, Coord_Left);
        true->
            Left = 0
    end,

    Resultado = [Up,Down,Left,Right],
    Resultado.

restricaoVerticalCima(Tab,Reg,{X,Y},Prefiltro)->
    if 
        (X == length(Tab))->
            Resultado = Prefiltro;
        true->
            Coord_Down = {X+1,Y},
            Regiao_id = get_valor_coord(Reg,{X,Y}),
            Regiao_id_abaixo = get_valor_coord(Reg,Coord_Down),
            Valor_abaixo = get_valor_coord(Tab,Coord_Down),
            if
                (Regiao_id_abaixo /= Regiao_id)->
                    Resultado = Prefiltro;
                true->
                    Resultado = lists:filter(fun(A)-> A > Valor_abaixo end, Prefiltro)
            end
    end,
    Resultado.

restricaoVerticalBaixo(Tab,Reg,{X,Y},Prefiltro)->
    if 
        (X == 1)->
            Resultado = Prefiltro;
        true->
            Coord_Up = {X-1,Y},
            Regiao_id = get_valor_coord(Reg,{X,Y}),
            Regiao_id_acima = get_valor_coord(Reg,Coord_Up),
            Valor_acima = get_valor_coord(Tab,Coord_Up),
            if
                Regiao_id_acima /= Regiao_id orelse Valor_acima == 0 ->
                    Resultado = Prefiltro;
                true->
                    Resultado = lists:filter(fun(A)-> A < Valor_acima end, Prefiltro)
            end
    end,
    Resultado.

filtraRestricoesVerticals(Tab,Reg,{X,Y},Prefiltro)->
    FiltradoVertical1 = restricaoVerticalCima(Tab,Reg,{X,Y},Prefiltro),
    restricaoVerticalBaixo(Tab,Reg,{X,Y},FiltradoVertical1).

lowestLonelyOfRegion(Reg,{X,Y})->
    Regiao_id = get_valor_coord(Reg,{X,Y}),
    if
        (X == length(Reg))->
            Regiao_id_abaixo = -1;
        true->
            Coord_Down = {X+1,Y},
            Regiao_id_abaixo = get_valor_coord(Reg,Coord_Down)
    end, 
    Linha = lists:nth(X,Reg),
    Num_mesma_regiao_linha = length(lists:filter(fun(A)-> A==Regiao_id end, Linha)),
   % io:format("NUM_MESMA = ~w~n", [Num_mesma_regiao_linha]),
   % io:format("Regiao_id = ~w~n",[Regiao_id]),
   % io:format("Regiao_id_abaixo = ~w~n",[Regiao_id_abaixo]),
    if
        (Num_mesma_regiao_linha == 1 andalso Regiao_id /= Regiao_id_abaixo) ->
            true;
        true->
            false
    end.

highestLonelyOfRegion(Reg,{X,Y})->
    Regiao_id = get_valor_coord(Reg,{X,Y}),
    if
        (X == 1)->
            Regiao_id_acima = -1;
        true->
            Coord_Up = {X-1,Y},
            Regiao_id_acima = get_valor_coord(Reg,Coord_Up)
    end, 
    Linha = lists:nth(X,Reg),
    Num_mesma_regiao_linha = length(lists:filter(fun(A)-> A==Regiao_id end, Linha)),
   % io:format("NUM_MESMA = ~w~n", [Num_mesma_regiao_linha]),
   % io:format("Regiao_id = ~w~n",[Regiao_id]),
   % io:format("Regiao_id_abaixo = ~w~n",[Regiao_id_abaixo]),
    if
        (Num_mesma_regiao_linha == 1 andalso Regiao_id /= Regiao_id_acima) ->
            true;
        true->
            false
    end.

filtraSolitarioRegiao(Reg, {X,Y}, Prefiltro) ->
    Tamanho_Prefiltro = length(Prefiltro),
    IsLowestLonely = lowestLonelyOfRegion(Reg,{X,Y}),
    IsHighestLonely = highestLonelyOfRegion(Reg,{X,Y}),
    if 
        Tamanho_Prefiltro > 1 andalso IsLowestLonely ->
            Valor_max = lists:max(Prefiltro),
            io:format("Valor_max = ~w~n",[Valor_max]),
            lists:subtract(Prefiltro, [Valor_max]);
        Tamanho_Prefiltro > 1 andalso IsHighestLonely ->
            Valor_min = lists:min(Prefiltro),
            io:format("Coordenada = ~w~n",[{X,Y}]),
            io:format("Prefiltro = ~w~n",[Prefiltro]),
            io:format("Valor_min = ~w~n",[Valor_min]),
            lists:subtract(Prefiltro, [Valor_min]);
        true ->
            Prefiltro
    end.

restricaoPossibilidades(Tab, Reg, {X,Y}) ->
    Reg_id = get_valor_coord(Reg,{X,Y}),
    PossibilidadesBasicas = lists:seq(1, get_regiao_size(Reg, {X,Y})),
    PossibilidadesFiltrada1 = lists:subtract(PossibilidadesBasicas, valoresRegiao(Tab, Reg,Reg_id)),
    PossibilidadesFiltrada2 = lists:subtract(PossibilidadesFiltrada1, getAdjacentCells(Tab,{X,Y})),
    PossibilidadesFiltrada3 = filtraRestricoesVerticals(Tab,Reg,{X,Y},PossibilidadesFiltrada2),
    Final = filtraSolitarioRegiao(Reg,{X,Y},PossibilidadesFiltrada3),
    %io:format("Debug 1 = ~w~n",[Final]),
    Final.
    %PossibilidadesFiltrada3.

encontraUnicaPossibilidade(Tabuleiro,Possibilidades_Posicoes)->
    


valoresPossiveisDaRegiao(Tab,Reg,Reg_id)->
    Coords = coordsRegiao(Reg,Reg_id),
    Coords_zeros = lists:filter(fun({X,Y})-> get_valor_coord(Tab, {X,Y}) == 0 end, Coords),
    Lista_resultado = [{Coordenada,restricaoPossibilidades(Tab,Reg,Coordenada)} || Coordenada <- Coords_zeros],
    Lista_resultado.

pre_solucionador(Tab, Reg, [{X,Y} | Resto])->
    Possibilidades = restricaoPossibilidades(Tab,Reg,{X,Y}),
    
    if 
        length(Possibilidades) == 1 ->
            Novo_Tab = substitui_valor_matrix(Tab,{X,Y},hd(Possibilidades));
        true->
            Novo_Tab = Tab
    end,
    if
        Resto == [] ->
            Novo_Tab;
        true->            
            pre_solucionador(Novo_Tab, Reg, Resto)
    end.

solucionador(Tab,Reg)->
    Coords_zero = get_coords_vazias(Tab),
    Novo_tab = pre_solucionador(Tab,Reg,Coords_zero),   
    if
        Novo_tab == Tab ->
            Novo_tab;
        true->
            io:format("Iterou solucionador = ~w~n",[1]),
            solucionador(Novo_tab,Reg)
    end.

print_matrix(Matrix) ->
    lists:foreach(
        fun(Row) -> 
            io:format("~p~n", [Row])
        end,
        Matrix
    ).


start() -> 
    Tabuleiro1=[
        [2,0,0,0,1,0],
        [0,0,0,3,0,0],
        [0,3,0,0,5,3],
        [0,0,0,0,0,0],
        [0,0,3,0,4,2],
        [0,0,0,0,0,0]],

    Regioes1 = [
        [1,1,2,2,  2,  3],
        [4,4,4,4,  4,  3],
        [5,6,6,6,  4,  7],
        [5,5,5,6,  7,  7],
        [8,8,9,10, 10,10],
        [11,11,9,9,10,10]],

    Lista = [1,2,3,4],

    Coord = {6,6},
    Coords_zero = get_coords_vazias(Tabuleiro1),
    Size = get_regiao_size(Regioes1, Coord),
    Poss = restricaoPossibilidades(Tabuleiro1, Regioes1, Coord), %restricaoPossibilidades(Tabuleiro1, Regioes1, Coord),
    Teste = restricaoVerticalBaixo(Tabuleiro1,Regioes1,Coord,Poss),
   % Nova = substitui_valor_lista(Lista,3,99),
    io:format("~p~n",[Tabuleiro1]),
    io:format("~w~n",[Poss]),
    io:format("~w~n",[highestLonelyOfRegion(Regioes1,{1,6})]),
    print_matrix(solucionador(Tabuleiro1,Regioes1)),
    io:format("~w~n",[valoresPossiveisDaRegiao(Tabuleiro1,Regioes1,10)]).

    %kojun:start().
