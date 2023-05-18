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
            lists:subtract(Prefiltro, [Valor_max]);
        Tamanho_Prefiltro > 1 andalso IsHighestLonely ->
            Valor_min = lists:min(Prefiltro),
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

encontraUnicaPossibilidade(Tab,Possibilidades_Posicoes)->
    
    Coords_e_valores_unicos = valoresUnicos(Possibilidades_Posicoes),
    %io:format("DEBUG 1 ~w~n",[Coords_e_valores_unicos]),
    if
        Coords_e_valores_unicos == [] ->
            Tab;
        true->
            atualizaValoresComUnicos(Tab,Coords_e_valores_unicos)
    end.

iteraProcurandoPossibilidade(Tab,Reg)->
    Comeco_Count = lists:max(lists:max(Reg)),
    iteraProcurandoPossibilidade(Tab,Reg,Comeco_Count).

iteraProcurandoPossibilidade(Tab,Reg,Count)->
    Possibilidades = valoresPossiveisDaRegiao(Tab,Reg,Count),
    Novo_tab = encontraUnicaPossibilidade(Tab,Possibilidades),
    if
        Count==1->
            Novo_tab;
        true->
            iteraProcurandoPossibilidade(Novo_tab,Reg,Count-1)
    end.
    
atualizaValoresComUnicos(Tab,[{{X,Y},Valor} | Resto])->
    Novo_tab = substitui_valor_matrix(Tab,{X,Y},hd(Valor)),
    if
        Resto == [] ->
            Novo_tab;
        true->
            atualizaValoresComUnicos(Novo_tab,Resto)
    end.

valoresUnicos(Lista_Coords_Poss)->
    Lista_Possibilidades = lists:flatten([Poss || {_, Poss} <- Lista_Coords_Poss]),
    Lista_Unicos = 
        lists:filter(fun(A) -> 
        length(lists:filter(fun(B) -> B == A end, Lista_Possibilidades)) == 1 end, Lista_Possibilidades),
    Res = coordsValoresUnicos(Lista_Coords_Poss,Lista_Unicos),
    %io:format("DEBUG ~w~n",[Res]),
    Res.

coordsValoresUnicos(Lista_Coords_Poss, Lista_Unicos) ->
    Lista = lists:map(fun({{X, Y}, List}) ->
        Unicos = lists:filter(fun(Value) ->
            lists:member(Value, List)
        end, Lista_Unicos),
        if length(Unicos) == 1 ->
            {{X, Y}, Unicos};
           true -> {}
        end
    end, Lista_Coords_Poss),
    lists:filter(fun(A)-> A /= {} end, Lista).

getAllPossibilidadesVazios(_,_,[],Resultado)->
    Resultado;
getAllPossibilidadesVazios(Tab,Reg,[{X,Y} | Resto],Resultado)->
    Possibilidades_atual= restricaoPossibilidades(Tab,Reg,{X,Y}),
    Outras_possibilidades = getAllPossibilidadesVazios(Tab,Reg,Resto,Resultado),
    Resultado ++ [Possibilidades_atual] ++ Outras_possibilidades.

testaPossibilidades(Tab,_,_,[])->
    Tab;
testaPossibilidades(Tab,Reg,{X,Y}, [Possibilidade | Resto])->
    Novo_tab = substitui_valor_matrix(Tab,{X,Y},Possibilidade),
    Coords_zero = get_coords_vazias(Tab),
    Poss_vazios = getAllPossibilidadesVazios(Tab,Reg,Coords_zero,[]),
    Novo_Coords_zero = get_coords_vazias(Novo_tab),
    Novo_Poss_vazios = getAllPossibilidadesVazios(Novo_tab,Reg,Novo_Coords_zero,[]),
    Tamanho_Novo_Poss = length(Novo_Poss_vazios),
    Tamanho_Poss = length(Poss_vazios),
    if
        %Se o número de célula com possibilidades diminui em mais de um ao preencher
        %uma célula, isso significa que o preenchimento desta célula com o valor X
        %impossibilita que qualquer valor seja inserido em outra célula, ou seja,
        %X é o valor errado.
        Tamanho_Novo_Poss < (Tamanho_Poss - 1) ->
            testaPossibilidades(Tab,Reg,{X,Y},Resto);
        true ->
            solucionador(Novo_tab,Reg)
    end.




valoresPossiveisDaRegiao(Tab,Reg,Reg_id)->
    Coords = coordsRegiao(Reg,Reg_id),
    Coords_zeros = lists:filter(fun({X,Y})-> get_valor_coord(Tab, {X,Y}) == 0 end, Coords),
    Lista_resultado = [{Coordenada,restricaoPossibilidades(Tab,Reg,Coordenada)} || Coordenada <- Coords_zeros],
    Lista_resultado.

pre_solucionador(Tab, Reg, [{X,Y} | Resto])->
    %io:format("~w~n",[{X,Y}]),
    Novo_tab1 = iteraProcurandoPossibilidade(Tab,Reg),
    Possibilidades = restricaoPossibilidades(Novo_tab1,Reg,{X,Y}),
    
    if 
        length(Possibilidades) == 1 ->
            Novo_Tab2 = substitui_valor_matrix(Novo_tab1,{X,Y},hd(Possibilidades));
        true->
            Novo_Tab2 = Novo_tab1
    end,
    if
        Resto == [] ->
            Novo_Tab2;
        true->            
            pre_solucionador(Novo_Tab2, Reg, Resto)
    end.

solucionador(Tab,Reg)->
    Coords_zero = get_coords_vazias(Tab),
    if
        length(Coords_zero) == 0 ->
            Tab;
        true->
            Novo_tab = pre_solucionador(Tab,Reg,Coords_zero),   
            if
                Novo_tab == Tab ->
                    Novo_Coords_zero = get_coords_vazias(Novo_tab),
                    if length(Novo_Coords_zero) == 0 -> %Se o tabuleiro está resolvido, devolve
                        Novo_tab;
                        true-> %Caso não esteja resolvido ainda vai chutar valor e tentar de novo
                            Novo_poss = restricaoPossibilidades(Tab,Reg,hd(Novo_Coords_zero)),
                            testaPossibilidades(Tab,Reg,hd(Novo_Coords_zero), Novo_poss)
                    end;
                true->
                    solucionador(Novo_tab,Reg)
            end
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

Tabuleiro10 =
    [[5,0,2,0,2,0,3,1,3,1],
                        [0,4,0,1,0,5,0,5,0,4],
                        [7,5,1,7,0,0,3,1,3,0],
                        [0,4,0,0,0,0,0,0,0,3],
                        [2,0,3,4,0,2,0,0,4,0],
                        [5,0,2,0,6,0,0,0,0,0],
                        [0,1,3,0,1,0,0,4,0,3],
                        [6,7,0,3,0,1,4,0,0,1],
                        [4,0,3,0,4,0,0,0,0,3],
                        [0,1,0,2,0,6,2,0,2,1]],

Regioes10=
                        [[1,2,2,2,3,3,3,3,4,4],
                        [1,1,1,2,6,6,7,7,4,7],
                        [5,5,1,6,6,9,8,7,7,7],
                        [5,5,6,6,10,9,8,8,8,11],
                        [5,5,5,6,10,10,30,11,11,11],
                        [12,12,15,15,15,10,22,22,21,21],
                        [12,12,12,15,15,16,17,18,21,21],
                        [13,13,12,15,16,16,17,18,20,20],
                        [13,13,14,14,14,14,17,18,18,19],
                        [13,13,13,14,14,14,17,17,19,19]],

Tabuleiro0 = [
    [0,0,1],
    [0,1,3],
    [0,3,0]],

Regioes0= [
    [1,1,1],
    [2,2,2],
    [3,3,3]],

   Coords_zero = get_coords_vazias(Tabuleiro1),
    print_matrix(solucionador(Tabuleiro0,Regioes0)).



    %kojun:start().
