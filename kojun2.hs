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

tabuleiro100::Tabuleiro
tabuleiro100=
  [
  [3, 0, 2, 5, 6, 1, 0, 2, 0, 2, 0, 0, 1, 3, 5, 0, 0], 
  [1, 0, 0, 0, 4, 0, 4, 0, 0, 0, 6, 3, 2, 0, 0, 1, 0], 
  [7, 5, 0, 0, 2, 4, 0, 0, 0, 0, 0, 4, 0, 0, 1, 0, 5], 
  [5, 2, 0, 0, 0, 0, 0, 0, 4, 0, 0, 1, 0, 3, 0, 5, 0], 
  [0, 6, 2, 0, 0, 0, 7, 0, 0, 0, 3, 0, 5, 6, 0, 0, 3], 
  [0, 4, 1, 0, 0, 4, 0, 0, 3, 0, 0, 2, 0, 0, 0, 2, 0], 
  [0, 0, 0, 3, 0, 0, 0, 0, 1, 0, 3, 6, 4, 0, 0, 0, 0], 
  [1, 0, 0, 0, 0, 0, 0, 5, 0, 4, 0, 5, 0, 0, 2, 6, 0], 
  [0, 0, 0, 4, 0, 0, 3, 0, 0, 0, 0, 2, 0, 0, 0, 4, 0], 
  [3, 4, 0, 2, 0, 2, 0, 0, 3, 0, 0, 6, 4, 5, 0, 0, 0], 
  [0, 1, 0, 1, 0, 0, 3, 7, 5, 0, 0, 2, 3, 0, 2, 6, 0], 
  [0, 6, 0, 0, 0, 3, 4, 0, 0, 1, 0, 0, 0, 0, 0, 3, 5], 
  [0, 2, 1, 0, 0, 7, 0, 0, 3, 0, 0, 3, 0, 6, 0, 0, 1], 
  [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6], 
  [0, 0, 1, 3, 0, 3, 5, 0, 5, 0, 2, 0, 6, 3, 4, 0, 5], 
  [3, 0, 0, 0, 6, 0, 7, 4, 0, 5, 0, 0, 4, 0, 0, 0, 0], 
  [0, 2, 4, 0, 0, 0, 0, 0, 3, 0, 1, 6, 2, 0, 2, 4, 1]]

regioes100::Regiao
regioes100=
  [
  [1, 2, 2, 4,  4,  4, 4,  4,    5, 5,  7, 7, 7, 7, 8,     9, 9], 
  [1, 1, 3, 4, 13, 13, 4, 63,    5, 6, 11, 8, 8, 8, 8,     9, 9], 
  [16, 1, 1, 14, 13, 12, 12, 12, 5, 11, 11, 11, 11, 8, 8, 10, 9], 
  [16, 1, 14, 14, 13, 18, 12, 19, 20, 20, 20, 20, 11, 10, 10, 10, 9], 
  [16, 17, 14, 15, 18, 18, 18, 19, 20, 27, 20, 21, 21, 21, 10, 10, 9], 
  [16, 17, 14, 18, 18, 18, 27, 27, 27, 27, 26, 21, 21, 21, 22, 22, 23], 
  [16, 17, 17, 17, 17, 28, 28, 28, 27, 26, 26, 25, 25, 24, 23, 23, 23], 
  [16, 16, 31, 17, 30, 29, 28, 36, 36, 36, 26, 25, 25, 39, 39, 39, 40], 
  [31, 31, 31, 30, 30, 29, 28, 36, 36, 36, 25, 25, 25, 39, 39, 39, 40], 
  [32, 31, 31, 30, 33, 29, 34, 34, 34, 35, 37, 37, 38, 38, 38, 41, 40], 
  [32, 32, 32, 30, 33, 29, 29, 29, 29, 37, 37, 37, 38, 46, 38, 38, 40], 
  [60, 62, 62, 33, 33, 54, 54, 54, 54, 37, 37, 47, 45, 45, 38, 42, 42], 
  [60, 62, 62, 62, 56, 55, 53, 54, 53, 64, 48, 48, 48, 45, 42, 42, 42], 
  [60, 61, 61, 62, 56, 55, 53, 53, 53, 64, 48, 49, 48, 45, 44, 43, 43], 
  [60, 60, 60, 58, 55, 55, 55, 52, 52, 52, 49, 49, 49, 45, 44, 44, 43], 
  [59, 58, 58, 58, 58, 55, 52, 52, 51, 50, 50, 49, 49, 45, 44, 43, 43], 
  [59, 59, 58, 57, 57, 55, 52, 52, 50, 50, 50, 50, 50, 45, 44, 43, 43]]



--DAQUI PRA CIMA TA FUNCIONANDO, SÓ FALTA ESTUDAR E ENTENDER 100%--
findEmptyCells :: Tabuleiro -> [(Int, Int)]
findEmptyCells board = [(r, c) | r <- [0..(length board)-1], c <- [0..(length (board !! 0))-1], board !! r !! c == 0]


valoresRegiao :: Tabuleiro -> Regiao -> Int -> [Int]
valoresRegiao tab reg r = [tab !! i !! j | (i,j) <- coordsRegiao reg r]
    where
        coordsRegiao reg r = [(i,j) | i <- [0..n-1], j <- [0..n-1], reg !! i !! j == r]
        n = length tab
--DAQUI PRA CIMA TUDO FUNCIONA--
getAdjacentCells :: Tabuleiro -> (Int, Int) -> [Int]
getAdjacentCells matrix (row, col) =
  let numRows = length matrix
      numCols = length (matrix !! 0)
      up = if row == 0 then 0 else  (matrix !! (row - 1) !! col)
      down = if row == numRows - 1 then 0 else  (matrix !! (row + 1) !! col)
      left = if col == 0 then 0 else  (matrix !! row !! (col - 1))
      right = if col == numCols - 1 then 0 else  (matrix !! row !! (col + 1))
  in nub [up, down, left, right]

restricaoPossibilidades::Tabuleiro->Regiao->(Int,Int)-> [Int] --ESSA BOSTA NÃO FUNCIONA DIREITO. OLHA BEM.
restricaoPossibilidades tab reg (x,y)= do
    let possibilidades_basicas = [1..getRegiaoSize reg (x,y)] --Possibilidades basicas: Qualquer numero entre 1 e (tamanho da regiao)
    let possibilidades_filtradas1 =possibilidades_basicas \\ valoresRegiao tab reg (reg!!x!!y) --Remove itens da lista de possibilidades que já estão presentes na regiao
    let possibilidades_filtradas2  = possibilidades_filtradas1 \\ (getAdjacentCells tab (x,y)) --remove itens da lista de possibilidades que já estão presentes em alguma adjacencia
    --if restricaoVerticalCima tab reg (x,y) /= [] then  possibilidades_filtradas2 `intersect` (restricaoVerticalCima tab reg (x,y)) else possibilidades_filtradas2
    --possibilidades_filtradas2
    let possibilidades_filtradas3 = filtraRestricoesVerticais tab reg (x,y) possibilidades_filtradas2 --`intersect`  (lowestLonelyofRegion tab reg (x,y) possibilidades_filtradas2)
    --possibilidades_filtradas3
    filtraSolitarioRegiao tab reg (x,y) possibilidades_filtradas3


restricaoVerticalCima :: Tabuleiro -> Regiao -> (Int, Int) -> [Int] -- FUNCIONA
restricaoVerticalCima tab reg (x, y)
    | x == length tab-1 = [] --evita index out of bounds. Itens da ultima linha não tem que se preocupar em ser maiores que o numero abaixo
    | reg!!(x+1)!!y /= id_regiao = [] --nao faz nada se a celula abaixo for de outra regiao
    | otherwise = filter (> valor_celula) [1..getRegiaoSize reg (x,y)+1] --devolve os valores maiores que o valor da celula acima para serem filtradas depois
  where
    id_regiao = reg!!x!!y
    valor_celula = tab!!(x+1)!!y

restricaoVerticalBaixo:: Tabuleiro -> Regiao -> (Int, Int) -> [Int]
restricaoVerticalBaixo tab reg (x,y)
     | x == 0 = [] --evita index out of bounds. Itens da primeira linha não tem que se preocupar com ser menor que a célula acima
    | reg!!(x-1)!!y /= id_regiao = [] -- se a celula acima não for da mesma regiao, retorna lista vazia
    | otherwise = filter (<valor_celula)[1..getRegiaoSize reg (x,y)+1]
   where
    id_regiao = reg!!x!!y
    valor_celula=tab!!(x-1)!!y

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

valoresPossiveisDaRegiao::Tabuleiro->Regiao->Int->[((Int,Int),[Int])]
valoresPossiveisDaRegiao tab reg reg_id= do
  [((i,j),l) | i <-[0.. length tab-1], j <- [0.. length tab-1],isBlank tab i j && (reg!!i!!j) == reg_id, l <- [restricaoPossibilidades tab reg (i,j)]] 

encontrarUnicaPossibilidade::Tabuleiro->[((Int,Int),[Int])]->Tabuleiro
encontrarUnicaPossibilidade tab [] = tab
encontrarUnicaPossibilidade tab possibilidades = do 
  let valor_unico = uniqueNumbers $ concatValues possibilidades
  let coordendadas_valor_unico = findCoordinate possibilidades $ head valor_unico
  if valor_unico == []
    then tab
    else atualizaTabuleiro tab (fst (coordendadas_valor_unico), snd (coordendadas_valor_unico), head valor_unico)

concatValues :: [((Int,Int), [Int])] -> [Int]
concatValues xs = concatMap snd xs


uniqueNumbers :: [Int] -> [Int]
uniqueNumbers xs = filter (\x -> length (filter (== x) xs) == 1) xs

findCoordinate :: [((Int,Int), [Int])] -> Int -> (Int, Int)
findCoordinate coords num =
  case filter (elem num . snd) coords of
    [(coord, _)] -> coord

iteraProcurandoPossibilidade::Tabuleiro->Regiao->Int->Tabuleiro
iteraProcurandoPossibilidade tab _ 0 = tab
iteraProcurandoPossibilidade tab reg n =
  iteraProcurandoPossibilidade (encontrarUnicaPossibilidade tab (valoresPossiveisDaRegiao tab reg n)) reg (n-1)

procuraTriviais::Tabuleiro->Regiao->[(Int,Int)]->Tabuleiro
procuraTriviais tab reg [] = tab
procuraTriviais tab reg ((linha,coluna):resto) = do
  let correcao1 = procuraSanduiche tab reg (linha,coluna)
  let correcao2 = procuraColunaTrivial tab reg (linha,coluna)
  procuraTriviais correcao1 reg resto
  

procuraSanduiche::Tabuleiro->Regiao->(Int,Int)->Tabuleiro
procuraSanduiche tab reg (x,y) = do
  let regiao_id = (reg!!x)!!y
  let regiao_id_abaixo = if x == length reg -1 then -99 else (reg!!(x+1)!!y)
  let regiao_id_acima = if x == 0 then -99 else (reg!!(x-1)!!y)
  let verticalUP = restricaoVerticalCima tab reg (x,y)
  let verticalDOWN = restricaoVerticalBaixo tab reg (x,y)
  if regiao_id_acima == regiao_id_abaixo && regiao_id_abaixo == regiao_id && tab!!(x-1)!!y - tab!!(x+1)!!y == 2
    then atualizaTabuleiro tab (x,y,(head $ verticalUP `intersect` verticalDOWN))
    else tab

procuraColunaTrivial::Tabuleiro->Regiao->(Int,Int)->Tabuleiro
procuraColunaTrivial tab reg (x,y) = do
  let regiao_id = (reg!!x)!!y
  let regiao_id_abaixo = if x == length reg -1 then -1 else (reg!!(x+1)!!y)
  if (length (filter (==0) (valoresRegiao tab reg regiao_id))) == 2 && regiao_id==regiao_id_abaixo && tab!!(x+1)!!y == 0 then
    atualizaTabuleiro tab (x,y,maximum ([1..(getRegiaoSize reg (x,y))] \\ valoresRegiao tab reg regiao_id))
    --atualizaTabuleiro tab (x,y, ([1..getRegiaoSize reg -1] \\ valoresRegiao tab reg regiao_id))
    else atualizaTabuleiro tab (x,y,9999999999)


getZeroCoords :: [[Int]] -> [(Int, Int)]
getZeroCoords matrix = [(row, col) | (row, rowVals) <- zip [0..] matrix,
                                     (col, val) <- zip [0..] rowVals,
                                     val == 0]


lowestLonelyOfRegion::Regiao -> (Int,Int) -> Bool
lowestLonelyOfRegion reg (x,y) = do
    let regiao_id = (reg!!x)!!y
    let regiao_id_abaixo = if x == length reg -1 then -1 else (reg!!(x+1)!!y)
    length (filter (==regiao_id) (reg!!x)) == 1 && regiao_id /= regiao_id_abaixo


highestLonelyOfRegion :: Regiao -> (Int, Int) -> Bool
highestLonelyOfRegion reg (x, y) = do
    let regiao_id = (reg!!x)!!y
    let regiao_id_acima = if x == 0 then -1 else (reg!!(x-1)!!y)
    length (filter (==regiao_id) (reg!!x)) == 1 && regiao_id /= regiao_id_acima
{-
lowestLonelyOfRegion :: [[Int]] -> (Int, Int) -> Bool
lowestLonelyOfRegion matrix (x, y) =
  let val = matrix !! x !! y
      row = matrix !! x
      occurrences = filter (== val) row
      isUnique = length occurrences == 1
      closestToBottom = all (\i -> i >= length matrix - x) $ elemIndices val row
  in isUnique && closestToBottom
-}
filtraSolitarioRegiao::Tabuleiro->Regiao->(Int,Int)->[Int]->[Int]
filtraSolitarioRegiao tab reg (x,y) prefiltro
    |lowestLonelyOfRegion reg (x,y) && length prefiltro /= 1 = prefiltro \\ [maximum prefiltro]
   --  |highestLonelyOfRegion reg (x,y) && length prefiltro /= 1 = prefiltro \\ [minimum prefiltro]
    |otherwise = prefiltro

atualizaTabuleiro :: Tabuleiro -> (Int, Int, Int) -> Tabuleiro
atualizaTabuleiro board (row, col, val) = 
  let (xs, ys) = splitAt row board
      row' = atualizaLinha (head ys) col val
  in xs ++ [row'] ++ tail ys

atualizaLinha :: [Int] -> Int -> Int -> [Int]
atualizaLinha row col val =
  let (xs, ys) = splitAt col row
  in xs ++ [val] ++ tail ys

isBlank :: Tabuleiro -> Int -> Int -> Bool
isBlank board row col = board !! row !! col == 0

--ANOTAÇÕES:: APARENTEMENTE O BACKTRACKING ESTÁ FUNCIONANDO, FALTA ARRUMAR O GETVALORESPOSSIVEIS SÓ
testaPossibilidades::Tabuleiro->Regiao->(Int,Int)->[Int]->Maybe Tabuleiro
testaPossibilidades tab _ _ [] = Just tab
testaPossibilidades tab reg (x,y) (possibilidade:resto) = do
  let newTab = atualizaTabuleiro tab (x,y,possibilidade)
  let newZeroCoords = getZeroCoords newTab
  let num_impossibilidades = length (filter (==[]) $ getAllPossibilidadesVazios newTab reg newZeroCoords [])
  if num_impossibilidades  > 0
    then testaPossibilidades tab reg (x,y) resto
    else 
      solucionador newZeroCoords newTab reg--testaPossibilidades tab reg (head newZeroCoords) (restricaoPossibilidades tab reg $ head newZeroCoords) 

getAllPossibilidadesVazios::Tabuleiro->Regiao->[(Int,Int)]->[[Int]]->[[Int]]
getAllPossibilidadesVazios _ _ [] _ = []
getAllPossibilidadesVazios tab reg (coord:resto) lista_r= do
   let possibilidades_atual = restricaoPossibilidades tab reg coord
   lista_r ++ [possibilidades_atual] ++ getAllPossibilidadesVazios tab reg resto []

preSolucionador :: [(Int,Int)] -> Tabuleiro -> Regiao ->Maybe Tabuleiro
preSolucionador [] tab _ = Just tab
preSolucionador ((linha,coluna):resto) tab reg = do
    let newTab = procuraTriviais (iteraProcurandoPossibilidade tab reg (reg!!(length reg-1)!!(length reg -1))) reg $ getZeroCoords tab
    if newTab!!linha!!coluna == 0
        then let valores_possiveis = restricaoPossibilidades newTab reg (linha,coluna) \\ [newTab!!linha!!coluna]
            in if length valores_possiveis == 1 
            then preSolucionador resto (atualizaTabuleiro newTab (linha,coluna,head valores_possiveis)) reg
            else preSolucionador resto newTab reg
        else preSolucionador resto newTab reg
    

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


getRegiaoSize :: Eq a => [[a]] -> (Int,Int) ->  Int
getRegiaoSize reg (x,y) = length $ filter (==(reg!!x!!y)) $ concat reg

main :: IO ()
main = do
  print "AAA"
  let tab = tabuleiro0
  let reg = regioes0
  --print tab


  let x = [(i,j)|i <- [0..length tab -1] , j <- [0..length tab - 1]]
  let r = fromJust $ solucionador x tab reg
  print r
 -- print $ restricaoPossibilidades r reg (6,0)
  --print $ getAllPossibilidadesVazios r reg (getZeroCoords r) []

  
