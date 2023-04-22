import Data.List (nub,sortBy,elemIndices,transpose,intersect)
import Data.List ((\\))
import Data.Maybe (fromJust)

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
    let possibilidades_basicas = [1..getSize reg (x,y)] --Possibilidades basicas: Qualquer numero entre 1 e (tamanho da regiao)
    let possibilidades_filtradas1 =possibilidades_basicas \\ valoresRegiao tab reg (reg!!x!!y) --Remove itens da lista de possibilidades que já estão presentes na regiao
    let possibilidades_filtradas2  = possibilidades_filtradas1\\ (getAdjacentCells tab (x,y)) --remove itens da lista de possibilidades que já estão presentes em alguma adjacencia
    if restricaoVerticalCima tab reg (x,y) /= [] then intersect possibilidades_filtradas2 (restricaoVerticalCima tab reg (x,y)) else possibilidades_filtradas2
    
restricaoVerticalCima :: Tabuleiro -> Regiao -> (Int, Int) -> [Int] -- NAO FUNCIONA ESSA PORRA. REPENSA
restricaoVerticalCima tab reg (x, y)
    | x == length tab-1 = [] --evita index out of bounds
    | reg!!(x+1)!!y /= id_regiao = [] --nao faz nada se a celula abaixo for de outra regiao
    | otherwise = filter (> valor_celula) [1..getSize reg (x,y)+1] --devolve os valores maiores que o valor da celula acima para serem filtradas depois
  where
    id_regiao = reg!!x!!y
    valor_celula = tab!!(x+1)!!y
    



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

preSolucionador::Tabuleiro->Regiao->Tabuleiro
preSolucionador tab reg = do
    let valores_possiveis = restricaoPossibilidades tab reg (0,1)
    if (length valores_possiveis ) == 1 then atualizaTabuleiro tab (0,1,head valores_possiveis) else tab
    
preSolucionador1 :: [(Int,Int)] -> Tabuleiro -> Regiao -> Maybe Tabuleiro
preSolucionador1 [] tab _ = Just tab
preSolucionador1 ((linha,coluna):resto) tab reg =
    if tab!!linha!!coluna == 0
        then let valores_possiveis = restricaoPossibilidades tab reg (linha,coluna) \\ [tab!!linha!!coluna]
            in if length valores_possiveis == 1 
            then preSolucionador1 resto (atualizaTabuleiro tab (linha,coluna,head valores_possiveis)) reg
            else preSolucionador1 resto tab reg
        else preSolucionador1 resto tab reg
    

        
main :: IO ()
main = do
  print "AAA"
  print tabuleiro1
  let x = [(i,j)|i <- [0..length tabuleiro1 -1] , j <- [0..length tabuleiro1 - 1]]
  --print x
  let y = fromJust $ preSolucionador1 x tabuleiro1 regioes1
  print y
  let z = fromJust $ preSolucionador1 x y regioes1
  print z
  print $ fromJust $ preSolucionador1 x z regioes1
  print $ restricaoVerticalCima z regioes1 (1,4)
  print $ restricaoPossibilidades tabuleiro1 regioes1 (1,4)
  --print $ findEmptyCells tabuleiro1
  --print $ getSize regioes1 (0,0)
  print $ getAdjacentCells tabuleiro1 (0,1)
  --print $ restricaoPossibilidades tabuleiro1 regioes1 (0,1)
  --print $ resolveTabuleiro tabuleiro1 regioes1
  print $ checkTabuleiroValido [[1,2],[1,2]] regioes0
