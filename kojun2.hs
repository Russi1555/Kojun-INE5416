import Data.List (nub,sortBy,elemIndices,transpose)

type Regiao = [[Int]]
type Tabuleiro = [[Int]]

{-
TABULEIRO1 E REGIOES1 = https://www.janko.at/Raetsel/Kojun/001.a.htm
-}

tabuleiro0::Tabuleiro
tabuleiro0 = [
    [1,0],
    [0,0]]

regioes0::Regiao = [
    [1,1],
    [2,2]]

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
checkTabuleiroValido tab reg = all (checkRegiaoValida tab reg) [1..maxRegion] && checkZeros tab
    where
        maxRegion = maximum (map maximum reg)
        checkRegiaoValida tab reg r =
            checkRegiaoSemRepeticao tab reg r &&
            checkAdjacencias tab reg &&
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
        
checkAdjacencias :: Tabuleiro -> Regiao -> Bool
checkAdjacencias tab reg = all (all distinct) [regions]
  where
    distinct xs = length xs == length (nub xs)
    regions = map (\r -> regionValues tab r) [1..maxRegion]
    regionValues tab r = [tab !! i !! j | (i,j) <- regionCoords reg r]
    regionCoords reg r = [(i,j) | i <- [0..n-1], j <- [0..n-1], reg !! i !! j == r]
    maxRegion = maximum (map maximum reg)
    n = length tab

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


--DAQUI PRA CIMA TUDO FUNCIONA--
resolveTabuleiro:: Tabuleiro -> Regiao -> Maybe Tabuleiro
resolveTabuleiro tab reg
  | checkTabuleiroValido tab reg= Just tab
  | otherwise = testarProximosValores (getValoresPossiveis tab reg) tab reg

getValoresPossiveis::Tabuleiro->Regiao->[(Int,Int,Int)]
getValoresPossiveis tab reg = 
  [(linha,coluna,n)| linha <-[0..length tab -1], coluna <-[0..length tab -1], isBlank tab linha coluna, n <-[1..length tab]]

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

main :: IO ()
main = do
  print "AAA"
  print tabuleiro0
  let x = getValoresPossiveis tabuleiro0 regioes0
  print x
  --print $ findEmptyCells tabuleiro1
  print $ resolveTabuleiro tabuleiro0 regioes0

  print $ checkTabuleiroValido [[1,2],[1,0]] regioes0