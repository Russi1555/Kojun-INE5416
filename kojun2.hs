import Data.List (nub,sortBy,elemIndices,transpose,intersect,findIndex,elemIndex)
import Data.List ((\\))
import Data.Maybe (fromJust)
import Data.ByteString (count)
import Control.Monad (when)

type Regiao = [[Int]]
type Tabuleiro = [[Int]]

{-
TABULEIRO1 E REGIOES1 = https://www.janko.at/Raetsel/Kojun/001.a.htm
-}

tabuleiro0::Tabuleiro
tabuleiro0 = [
    [1,0],
    [0,0]]

regioes0::Regiao
regioes0= [
    [1,1],
    [2,2]]

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

tabuleiro110::Tabuleiro
tabuleiro110 =
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

regioes110::Regiao
regioes110=
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

checkTabuleiroValido::Tabuleiro->Regiao->Bool
checkTabuleiroValido tab reg = all (checkRegiaoValida tab reg) [1..maxRegion] && checkZeros tab && not(checkAdjacencias tab)
    where
        maxRegion = maximum (map maximum reg)
        checkRegiaoValida tab reg r =
            checkRegiaoSemRepeticao tab reg r &&
            --checkAdjacencias tab reg &&
            checkOrdemVertical tab reg r

checkZeros::Tabuleiro->Bool
checkZeros tab = not(any (0 `elem`) tab)

valoresRegiao :: Tabuleiro -> Regiao -> Int -> [Int]
valoresRegiao tab reg r = [tab !! i !! j | (i,j) <- coordsRegiao reg r]
    where
        coordsRegiao reg r = [(i,j) | i <- [0..n-1], j <- [0..n-1], reg !! i !! j == r]
        n = length tab

checkRegiaoSemRepeticao::Tabuleiro->Regiao->Int->Bool
checkRegiaoSemRepeticao tab reg r = sem_reps $ valoresRegiao tab reg r--all (sem_reps . filter (/=0))[(valoresRegiao tab reg r)]
    where
        sem_reps x = length x == length (nub x)    

checkAdjacencias :: Eq a => [[a]] -> Bool
checkAdjacencias [] = False
checkAdjacencias [x] = False
checkAdjacencias (x:y:xs) = checkAdjacencias' x y || checkAdjacencias (y:xs)
  where
    checkAdjacencias' [] [] = False
    checkAdjacencias' (a:as) (b:bs) = a == b || checkAdjacencias' as bs
    checkAdjacencias' _ _ = False

checkOrdemVertical :: Tabuleiro -> Regiao -> Int -> Bool
checkOrdemVertical tab reg r = all (checkRegionVerticalOrder tab) [verticalCoords]
  where
    coords = [(i, j) | i <- [0..n-1], j <- [0..n-1], reg !! i !! j == r]
    verticalCoords = sortBy (\(i1, j1) (i2, j2) -> compare (tab !! i1 !! j1) (tab !! i2 !! j2)) coords
    checkRegionVerticalOrder tab coords = all (\((i1, j1), (i2, j2)) -> i1 < i2) verticalPairs
      where
        verticalPairs = [((i1, j1), (i2, j2)) | (i1, j1) <- coords, (i2, j2) <- coords, j1 == j2, i1 < i2]
    n = length tab

--DAQUI PRA CIMA TA FUNCIONANDO, SÓ FALTA ESTUDAR E ENTENDER 100%--
findEmptyCells :: Tabuleiro -> [(Int, Int)]
findEmptyCells board = [(r, c) | r <- [0..(length board)-1], c <- [0..(length (board !! 0))-1], board !! r !! c == 0]

getSize :: Regiao -> (Int,Int) -> Int
getSize regiao (x,y) = countConnected (x,y) [] 1
  where
    countConnected (i,j) visited count
      | i < 0 || j < 0 || i >= length regiao || j >= length (head regiao) || elem (i,j) visited = count - 1
      | regiao !! i !! j /= regiao !! x !! y = count - 1
      | otherwise = maximum [ countConnected (i-1,j) ((i,j):visited) (count+1)
                             , countConnected (i+1,j) ((i,j):visited) (count+1)
                             , countConnected (i,j-1) ((i,j):visited) (count+1)
                             , countConnected (i,j+1) ((i,j):visited) (count+1)
                             ]

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
    filtraSolitarioRegiao tab reg (x,y) possibilidades_filtradas3


restricaoVerticalCima :: Tabuleiro -> Regiao -> (Int, Int) -> [Int] -- FUNCIONA
restricaoVerticalCima tab reg (x, y)
    | x == length tab-1 = [] --evita index out of bounds. Itens da ultima linha não tem que se preocupar em ser maiores que o numero abaixo
    | reg!!(x+1)!!y /= id_regiao = [] --nao faz nada se a celula abaixo for de outra regiao
    | otherwise = filter (> valor_celula) [1..getSize reg (x,y)+1] --devolve os valores maiores que o valor da celula acima para serem filtradas depois
  where
    id_regiao = reg!!x!!y
    valor_celula = tab!!(x+1)!!y

restricaoVerticalBaixo:: Tabuleiro -> Regiao -> (Int, Int) -> [Int]
restricaoVerticalBaixo tab reg (x,y)
     | x == 0 = [] --evita index out of bounds. Itens da primeira linha não tem que se preocupar com ser menor que a célula acima
    | reg!!(x-1)!!y /= id_regiao = [] -- se a celula acima não for da mesma regiao, retorna lista vazia
    | otherwise = filter (<valor_celula)[1..getSize reg (x,y)+1]
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
                            (prefiltro `intersect`verticalUP ) `intersect` verticalDOWN
{-
lowestLonelyOfRegion::Regiao->(Int,Int)->Bool
lowestLonelyOfRegion reg (x,y)=
        let val = (reg !! x) !! y
            line = reg !! x
        (last (elemIndices val line) == y) && (length (filter (== val) line) == 1) 
        --    then [minimum prefiltro]
          --  else prefiltro
-}

highestLonelyOfRegion :: Regiao -> (Int, Int) -> Bool
highestLonelyOfRegion reg (x, y) = 
    let val = reg !! x !! y
        occurrences = filter (== val) (concat reg)
        isUnique = length occurrences == 1
        lastOccurrence = length (elemIndices val (concat reg)) == 1
  in isUnique && lastOccurrence

lowestLonelyOfRegion :: [[Int]] -> (Int, Int) -> Bool
lowestLonelyOfRegion matrix (x, y) = --tem problema aqui. Não ta respeitando alguma condição. AINDA TEM PROBLEMA NESSA BOSTA AQUI. RESOLVE O 10X10 NÃO O 6X6
  let val = matrix !! x !! y
      row = matrix !! x
      occurrences = filter (== val) (concat matrix)
      isUnique = length occurrences == 1
      isLast = elemIndex val (reverse (concat matrix)) == Just ((length matrix - x - 1) * length row + y)
      uniqueInRow = length (filter (== val) row) == 1
    in isLast && uniqueInRow


filtraSolitarioRegiao::Tabuleiro->Regiao->(Int,Int)->[Int]->[Int]
filtraSolitarioRegiao tab reg (x,y) prefiltro
    |highestLonelyOfRegion reg (x,y) = [maximum prefiltro]
    |lowestLonelyOfRegion reg (x,y) = [minimum prefiltro]
    |otherwise = prefiltro


resolveTabuleiro:: Tabuleiro -> Regiao -> Maybe Tabuleiro
resolveTabuleiro tab reg
  | checkTabuleiroValido tab reg= Just tab
  | otherwise = testarProximosValores (getValoresPossiveis tab reg) tab reg

getValoresPossiveis::Tabuleiro->Regiao->[(Int,Int,Int)]
getValoresPossiveis tab reg = 
  [(linha,coluna,n)| linha <-[0..length tab -1], coluna <-[0..length tab -1], isBlank tab linha coluna, n <-restricaoPossibilidades tab reg (linha,coluna)]

testarProximosValores::[(Int, Int, Int)]-> Tabuleiro ->Regiao -> Maybe Tabuleiro
testarProximosValores [] tab _ = Nothing
testarProximosValores ((linha,coluna, n):resto) tab reg = 
  case resolveTabuleiro tabAtualizado reg of
    Just tab -> Just tab
    Nothing-> testarProximosValores resto tab reg
  where tabAtualizado = atualizaTabuleiro tab (linha,coluna, n)

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

    
preSolucionador :: [(Int,Int)] -> Tabuleiro -> Regiao ->Maybe Tabuleiro
preSolucionador [] tab _ = Just tab
preSolucionador ((linha,coluna):resto) tab reg =
    if tab!!linha!!coluna == 0
        then let valores_possiveis = restricaoPossibilidades tab reg (linha,coluna) \\ [tab!!linha!!coluna]
            in if length valores_possiveis == 1 
            then preSolucionador resto (atualizaTabuleiro tab (linha,coluna,head valores_possiveis)) reg
            else preSolucionador resto tab reg
        else preSolucionador resto tab reg
    
solucionador :: [(Int, Int)] -> Tabuleiro -> Regiao -> Maybe Tabuleiro
solucionador coords tab reg = 
    let newTab = preSolucionador coords tab reg
    in case newTab of
        Just t -> if t == tab then Just t else solucionador coords t reg
        Nothing -> Nothing


getRegiaoSize :: Eq a => [[a]] -> (Int,Int) ->  Int
getRegiaoSize reg (x,y) = length $ filter (==(reg!!x!!y)) $ concat reg

main :: IO ()
main = do
  print "AAA"
  print tabuleiro110
  let x = [(i,j)|i <- [0..length tabuleiro110 -1] , j <- [0..length tabuleiro110 - 1]]
  --print x
  --let y = fromJust $ preSolucionador x tabuleiro1 regioes1
  --print y
  --let z = fromJust $ preSolucionador x y regioes1
  --print z
  --print $ fromJust $ preSolucionador x z regioes1
  --print $ restricaoVerticalCima z regioes1 (1,4)
  --print $ restricaoPossibilidades z regioes1 (3,3)
  --print $ getRegiaoSize  regioes1 (3,3)
  --print $ restricaoVerticalBaixo z regioes1 (3,3)
  --print $ lowestLonelyofRegion z regioes1 (3,3) [1,2,3,4,5,6]
  --print $ findEmptyCells tabuleiro1
  --print $ getSize regioes1 (0,0)
 -- print $ getAdjacentCells z (3,3)
  --print $ restricaoPossibilidades tabuleiro1 regioes1 (0,1)
  --print $ resolveTabuleiro z regioes1
  --let a = fromJust $ preSolucionador x z regioes1
  --print a
  --let b = fromJust $ preSolucionador x a regioes1
  --print b
  --let c = fromJust $ preSolucionador x b regioes1
  --print c
  --print $ restricaoPossibilidades a regioes1 (3,0)
  let r = fromJust $ solucionador x tabuleiro110 regioes110
  print $ restricaoPossibilidades tabuleiro110 regioes110 (6,5)
  print $ highestLonelyOfRegion regioes110 (6,5)
  print $ getSize regioes110 (7,4)
  print $ checkTabuleiroValido r regioes110
  print r
  let r2 = fromJust $ resolveTabuleiro r regioes110
  print r2
  print $ checkTabuleiroValido r2 regioes110

