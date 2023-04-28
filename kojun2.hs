import Data.List (find,nub,sortBy,elemIndices,transpose,intersect,findIndex)
import Data.List ((\\))
import Data.Maybe (fromJust)
import Data.ByteString (count)
import Distribution.Compat.CharParsing (tab)


type Regiao = [[Int]]
type Tabuleiro = [[Int]]

{-
TABULEIRO1 E REGIOES1 = https://www.janko.at/Raetsel/Kojun/001.a.htm
TABULEIRO10 E REGIOES10 = https://www.janko.at/Raetsel/Kojun/010.a.htm
-}

tabuleiro0::Tabuleiro
tabuleiro0 = [
    [0,0,0],
    [2,1,3],
    [0,0,0]]

regioes0::Regiao
regioes0= [
    [1,1,1],
    [2,2,2],
    [3,3,3]]

tabuleiro01::Tabuleiro
tabuleiro01= [
  [1,2,0],
  [0,3,1],
  [0,2,0]]


regioes01::Regiao
regioes01=[
  [1,1,1],
  [2,2,2],
  [3,3,3]]

tabuleiro1_sol::Tabuleiro
tabuleiro1_sol=[
     [2,1,3,2,1,2],
     [1,4,2,3,6,1],
     [4,3,4,2,5,3],
     [3,1,2,1,2,1],
     [1,2,3,5,4,2],
     [2,1,2,1,3,1]]

tabuleiro1::Tabuleiro
tabuleiro1=[
     [2,0,0,0,1,0],
     [0,0,0,3,0,0],
     [0,3,0,0,5,3],
     [0,0,0,0,0,0],
     [0,0,3,0,4,2],
     [0,0,0,0,0,0]]



regioes1::Regiao
regioes1=[
    [1,1,2,2,  2,  3],
    [4,4,4,4,  4,  3],
    [5,6,6,6,  4,  7],
    [5,5,5,6,  7,  7],
    [8,8,9,10, 10,10],
    [11,11,9,9,10,10]]

tabuleiro10::Tabuleiro
tabuleiro10 =
    [[5,0,2,0,2,0,3,1,3,1],
                        [0,4,0,1,0,5,0,5,0,4],
                        [7,5,1,7,0,0,3,1,3,0],
                        [0,4,0,0,0,0,0,0,0,3],
                        [2,0,3,4,0,2,0,0,4,0],
                        [5,0,2,0,6,0,0,0,0,0],
                        [0,1,3,0,1,0,0,4,0,3],
                        [6,7,0,3,0,1,4,0,0,1],
                        [4,0,3,0,4,0,0,0,0,3],
                        [0,1,0,2,0,6,2,0,2,1]]

regioes10::Regiao
regioes10=
                        [[1,2,2,2,3,3,3,3,4,4],
                        [1,1,1,2,6,6,7,7,4,7],
                        [5,5,1,6,6,9,8,7,7,7],
                        [5,5,6,6,10,9,8,8,8,11],
                        [5,5,5,6,10,10,30,11,11,11],
                        [12,12,15,15,15,10,22,22,21,21],
                        [12,12,12,15,15,16,17,18,21,21],
                        [13,13,12,15,16,16,17,18,20,20],
                        [13,13,14,14,14,14,17,18,18,19],
                        [13,13,13,14,14,14,17,17,19,19]]



-- Esta função retorna uma lista com os valores presentes em dada regiao --
valoresRegiao :: Tabuleiro -> Regiao -> Int -> [Int]
valoresRegiao tab reg r = [tab !! i !! j | (i,j) <- coordsRegiao reg r]
    where
        coordsRegiao reg r = [(i,j) | i <- [0..n-1], j <- [0..n-1], reg !! i !! j == r]
        n = length tab

--esta função retorna uma lista com os valores  das celular adjacentes a uma dada coordenada
--os if's em up,down,left,right evitam index out of bound ao checar celular presentes nas bordas
getAdjacentCells :: Tabuleiro -> (Int, Int) -> [Int]
getAdjacentCells matrix (row, col) =
  let numRows = length matrix
      numCols = length (matrix !! 0)
      up = if row == 0 then 0 else  (matrix !! (row - 1) !! col) --celula acima
      down = if row == numRows - 1 then 0 else  (matrix !! (row + 1) !! col) --celula abaixo
      left = if col == 0 then 0 else  (matrix !! row !! (col - 1)) --celula a esquerda
      right = if col == numCols - 1 then 0 else  (matrix !! row !! (col + 1)) -- celula a direita
  in nub [up, down, left, right]

--Filtra em etapas os valores possiveis que dada coordenada pode assumir de acordo com as restrições do jogo
restricaoPossibilidades::Tabuleiro->Regiao->(Int,Int)-> [Int] 
restricaoPossibilidades tab reg (x,y)= do
    let possibilidades_basicas = [1..getRegiaoSize reg (x,y)] --Possibilidades basicas: Qualquer numero entre 1 e (tamanho da regiao)
    let possibilidades_filtradas1 =possibilidades_basicas \\ valoresRegiao tab reg (reg!!x!!y) --Remove itens da lista de possibilidades que já estão presentes na regiao
    let possibilidades_filtradas2  = possibilidades_filtradas1 \\ (getAdjacentCells tab (x,y)) --Remove itens da lista de possibilidades que já estão presentes em alguma adjacencia
    let possibilidades_filtradas3 = filtraRestricoesVerticais tab reg (x,y) possibilidades_filtradas2 --Remove itens da lista de possibilidades de acordo com as restrições verticais (maiores que a celula acima ou menores que a celula abaixo)
    filtraSolitarioRegiao tab reg (x,y) possibilidades_filtradas3 -- caso a coordenada esteja na posição mais alta/baixa da região, remove o menor/maior item da lista de possibilidades

--Retorna itens maiores que o valor da célula acima da coordenada especificada (caso sejam da mesma regiao) para serem filtradas depois
restricaoVerticalCima :: Tabuleiro -> Regiao -> (Int, Int) -> [Int]
restricaoVerticalCima tab reg (x, y)
    | x == length tab-1 = [] --evita index out of bounds. Itens da ultima linha não tem que se preocupar em ser maiores que o numero abaixo
    | reg!!(x+1)!!y /= id_regiao = [] --nao faz nada se a celula abaixo for de outra regiao
    | otherwise = filter (> valor_celula) [1..getRegiaoSize reg (x,y)+1] --devolve os valores maiores que o valor da celula acima para serem filtradas depois
  where
    id_regiao = reg!!x!!y
    valor_celula = tab!!(x+1)!!y

--Retorna itens menores que o valor da célula abaixo da coordenada especificada (caso sejam da mesma regiao) para serem filtradas depois
restricaoVerticalBaixo:: Tabuleiro -> Regiao -> (Int, Int) -> [Int]
restricaoVerticalBaixo tab reg (x,y)
     | x == 0 = [] --evita index out of bounds. Itens da primeira linha não tem que se preocupar com ser menor que a célula acima
    | reg!!(x-1)!!y /= id_regiao = [] -- se a celula acima não for da mesma regiao, retorna lista vazia
    | otherwise = filter (<valor_celula)[1..getRegiaoSize reg (x,y)+1]
   where
    id_regiao = reg!!x!!y
    valor_celula=tab!!(x-1)!!y

--Realiza a filtragem das restrições verticais realizando a intersecção com a lista de possibilidades atual com o retorno das restricoes verticais
filtraRestricoesVerticais:: Tabuleiro->Regiao->(Int,Int)->[Int]->[Int]
filtraRestricoesVerticais tab reg (x,y) prefiltro = do
    let verticalUP = restricaoVerticalCima tab reg (x,y)
    let verticalDOWN = restricaoVerticalBaixo tab reg (x,y)
    if verticalUP == [] && verticalDOWN == []
        then prefiltro
        else
            if verticalUP /= [] && verticalDOWN == []
                then prefiltro `intersect` verticalUP
                else
                    if verticalUP == [] && verticalDOWN /= []
                        then prefiltro `intersect` verticalDOWN
                        else
                            (prefiltro `intersect` verticalUP ) `intersect` verticalDOWN

--Retorna uma lista de tuplas de (coordenada, possibilidades) de cada coordenada com valor 0 no tabuleiro
valoresPossiveisDaRegiao::Tabuleiro->Regiao->Int->[((Int,Int),[Int])]
valoresPossiveisDaRegiao tab reg reg_id= do
  [((i,j),l) | i <-[0.. length tab-1], j <- [0.. length tab-1],isBlank tab i j && (reg!!i!!j) == reg_id, l <- [restricaoPossibilidades tab reg (i,j)]] 

--Com a lista de valoresPossiveisDaRegiao analisa se em alguma das lista há um valor que só aparece nela. Se existe, é inserido na posicao.
encontrarUnicaPossibilidade::Tabuleiro->[((Int,Int),[Int])]->Tabuleiro
encontrarUnicaPossibilidade tab [] = tab
encontrarUnicaPossibilidade tab possibilidades = do 
  let valor_unico = uniqueNumbers $ concatValues possibilidades
  let coordendadas_valor_unico = findCoordinate possibilidades $ head valor_unico --pode ser que exista mais de um valor unico presentes nessas listas por região. Cada iteração trata do primeiro caso apenas
  if valor_unico == []
    then tab
    else atualizaTabuleiro tab (fst (coordendadas_valor_unico), snd (coordendadas_valor_unico), head valor_unico)

concatValues :: [((Int,Int), [Int])] -> [Int] --Retorna a lista de coords,possibilidades em uma lista apenas de possibilidades
concatValues xs = concatMap snd xs

uniqueNumbers :: [Int] -> [Int] --Retorna lista de numeros unicos
uniqueNumbers xs = filter (\x -> length (filter (== x) xs) == 1) xs

findCoordinate :: [((Int,Int), [Int])] -> Int -> (Int, Int) --retorna a coordenada onde um dado numero esta presente como possibilidade
findCoordinate coords num =
  case filter (elem num . snd) coords of
    [(coord, _)] -> coord

--Faz a iteracao do encontrarUnicaPossibilidade
iteraProcurandoPossibilidade::Tabuleiro->Regiao->Int->Tabuleiro
iteraProcurandoPossibilidade tab _ 0 = tab
iteraProcurandoPossibilidade tab reg n =
  iteraProcurandoPossibilidade (encontrarUnicaPossibilidade tab (valoresPossiveisDaRegiao tab reg n)) reg (n-1)


--Retorna uma lista das coordenadas do tabuleiro cujo valor seja 0
getZeroCoords :: Tabuleiro -> [(Int, Int)]
getZeroCoords matrix = [(row, col) | (row, rowVals) <- zip [0..] matrix,
                                     (col, val) <- zip [0..] rowVals,
                                     val == 0]

--Retorna um booleano indicando se a coordenada está na linha mais baixa da região e é a única da sua região na linha
lowestLonelyOfRegion::Regiao -> (Int,Int) -> Bool
lowestLonelyOfRegion reg (x,y) = do
    let regiao_id = (reg!!x)!!y
    let regiao_id_abaixo = if x == length reg -1 then -1 else (reg!!(x+1)!!y)
    length (filter (==regiao_id) (reg!!x)) == 1 && regiao_id /= regiao_id_abaixo

--Retorna um booleano indicando se a coordenada está na linha mais alta da região e é a única da sua região na linha
highestLonelyOfRegion :: Regiao -> (Int, Int) -> Bool
highestLonelyOfRegion reg (x, y) = do
    let regiao_id = (reg!!x)!!y
    let regiao_id_acima = if x == 0 then -1 else (reg!!(x-1)!!y)
    length (filter (==regiao_id) (reg!!x)) == 1 && regiao_id /= regiao_id_acima

-- caso a coordenada esteja na posição mais alta/baixa da região, remove o menor/maior item da lista de possibilidades
filtraSolitarioRegiao::Tabuleiro->Regiao->(Int,Int)->[Int]->[Int]
filtraSolitarioRegiao tab reg (x,y) prefiltro
    |lowestLonelyOfRegion reg (x,y) && length prefiltro /= 1 = prefiltro \\ [maximum prefiltro]
     |highestLonelyOfRegion reg (x,y) && length prefiltro /= 1 = prefiltro \\ [minimum prefiltro]
    |otherwise = prefiltro

--Função que insere valor novo no tabuleiro
atualizaTabuleiro :: Tabuleiro -> (Int, Int, Int) -> Tabuleiro
atualizaTabuleiro board (row, col, val) = 
  let (xs, ys) = splitAt row board
      row' = atualizaLinha (head ys) col val
  in xs ++ [row'] ++ tail ys

--Função que insere valor na linha
atualizaLinha :: [Int] -> Int -> Int -> [Int]
atualizaLinha row col val =
  let (xs, ys) = splitAt col row
  in xs ++ [val] ++ tail ys

--Função que retorna se o valor da coordenada é 0 (Usado para montar uma lista por isso o Int->Int ao invés de (Int,Int))
isBlank :: Tabuleiro -> Int -> Int -> Bool
isBlank board row col = board !! row !! col == 0

--Recursivamente testa chuta valores possíveis, caso seja um valor que não causa que outra celula tenha possibilidades = [], chama o solucionador de novo. 
--Bom para casos onde uma pequena alteração pode implicar em diversas descobertas para o pre-solucionador
testaPossibilidades::Tabuleiro->Regiao->(Int,Int)->[Int]->Maybe Tabuleiro
testaPossibilidades tab _ _ [] = Just tab --Quando a lista de coordenadas com valor 0 esta vazia, retorna o tabuleiro atual
testaPossibilidades tab reg (x,y) (possibilidade:resto) = do
  let newTab = atualizaTabuleiro tab (x,y,possibilidade) --newTab = tabuleiro atualizado com o chute atual
  let newZeroCoords = getZeroCoords newTab -- nova lista de coordenadas com zero
  let num_impossibilidades = length (filter (==[]) $ getAllPossibilidadesVazios newTab reg newZeroCoords []) --numero de celulas com valor 0 que tem possibilidades = [] (naõ tem possibilidades)
  if num_impossibilidades  > 0 --Se o chute atual causou alguma impossibilidade:
    then testaPossibilidades tab reg (x,y) resto -- Ignora o chute atual e testa o próximo
    else --Se não, chama o solucionador de novo para iterar nas coordenadas == 0
      solucionador newZeroCoords newTab reg--testaPossibilidades tab reg (head newZeroCoords) (restricaoPossibilidades tab reg $ head newZeroCoords) 

--Recebendo uma lista de coordenadas (com valor ==0) retorna uma lista de possibilidades de cada célula
getAllPossibilidadesVazios::Tabuleiro->Regiao->[(Int,Int)]->[[Int]]->[[Int]]
getAllPossibilidadesVazios _ _ [] _ = []
getAllPossibilidadesVazios tab reg (coord:resto) lista_r= do
   let possibilidades_atual = restricaoPossibilidades tab reg coord
   lista_r ++ [possibilidades_atual] ++ getAllPossibilidadesVazios tab reg resto []

--preSolucionador ≃ solucionador analítico
--Chama a filtragem das restricoes, procura possibilidades unicas e atualiza o tabuleiro
preSolucionador :: [(Int,Int)] -> Tabuleiro -> Regiao ->Maybe Tabuleiro
preSolucionador [] tab _ = Just tab
preSolucionador ((linha,coluna):resto) tab reg = do
    let newTab = iteraProcurandoPossibilidade tab reg (reg!!(length reg-1)!!(length reg -1))
    if newTab!!linha!!coluna == 0
        then let valores_possiveis = restricaoPossibilidades newTab reg (linha,coluna) \\ [newTab!!linha!!coluna]
            in if length valores_possiveis == 1 
            then preSolucionador resto (atualizaTabuleiro newTab (linha,coluna,head valores_possiveis)) reg
            else preSolucionador resto newTab reg
        else preSolucionador resto newTab reg
    
--"loop" principal de solucionar o tabuleiro. Se o preSolucionador retornar o mesmo tabuleiro que lhe foi dado, isso reflete que chegamos ao limite da nossa solução analítica
--Neste caso, chamamos o testaPossibilidades onde será chutado um valor de possibilidade na primeira célula == 0 e dentro de testaPossibilidades solucionador é chamado de novo. 
solucionador :: [(Int, Int)] -> Tabuleiro -> Regiao -> Maybe Tabuleiro
solucionador coords tab reg = 
    let newTab = preSolucionador coords tab reg
        newZeroCoords = getZeroCoords $ fromJust newTab
    in case newTab of
        Just t -> if t == tab 
          then if length newZeroCoords /= 0 
            then testaPossibilidades t reg (head $ getZeroCoords t) $ restricaoPossibilidades t reg (head $ getZeroCoords t) 
            else Just t
          else solucionador coords t reg
        Nothing -> Nothing

--Retorna o tamanho da regiao de dada coordenada
getRegiaoSize :: Regiao -> (Int,Int) ->  Int
getRegiaoSize reg (x,y) = length $ filter (==(reg!!x!!y)) $ concat reg

printTabuleiro ::Tabuleiro -> IO ()
printTabuleiro = mapM_ print


main :: IO ()
main = do
  let tab = tabuleiro10
  let reg = regioes10
  let x = [(i,j)|i <- [0..length tab -1] , j <- [0..length tab - 1]]
  let r = fromJust $ solucionador x tab reg
  if length (getZeroCoords r) > 0
    then print "Sem Solucao"
    else printTabuleiro r

  
