import Data.List (nub,transpose,sortBy)

type Coordenada = (Int, Int)
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
checkTabuleiroValido tab reg = all (checkRegiaoValida tab reg) [1..maxRegion]
    where
        maxRegion = maximum (map maximum reg)
        checkRegiaoValida tab reg r =
            checkRegiaoSemRepeticao tab reg r &&
            checkAdjacencias tab reg &&
            checkOrdemVertical tab reg r

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

--DAQUI PRA CIMA TUDO FUNCIONA--
valid :: Tabuleiro -> Coordenada -> Int -> Bool
valid tab (x,y) n = and [rowOK, colOK, regionOK]
  where
    rowOK = n `notElem` tab !! x
    colOK = n `notElem` [tab !! i !! y | i <- [0..length tab - 1]]
    regionOK = let (x0, y0) = (x `div` regionSize, y `div` regionSize)
                   coords = [(i, j) | i <- [x0*regionSize..(x0+1)*regionSize-1], j <- [y0*regionSize..(y0+1)*regionSize-1]]
               in n `notElem` [tab !! i !! j | (i,j) <- coords]
    regionSize = round $ sqrt $ fromIntegral $ length tab      

nextEmpty :: Tabuleiro -> Maybe Coordenada
nextEmpty tab = case [(i,j) | i <- [0..length tab - 1], j <- [0..length tab - 1], tab !! i !! j == 0] of
                  [] -> Nothing
                  (c:_) -> Just c

solveKojun :: Tabuleiro -> Regiao -> Maybe Tabuleiro
solveKojun tab reg
  | not (checkTabuleiroValido tab reg) = Nothing
  | null emptyCells = Just tab
  | otherwise = head $ dropWhile isNothing [tryValues (i,j) tab reg | (i,j) <- emptyCells]
  where
    emptyCells = [(i,j) | i <- [0..n-1], j <- [0..n-1], tab !! i !! j == 0]
    tryValues (i,j) tab reg = case valoresPossiveis of
                                [] -> Nothing
                                xs -> head $ dropWhile isNothing $ map (\v -> solveKojun (updateTabuleiro (i,j) v tab) reg) xs
      where
        updateTabuleiro (i,j) v tab = take i tab ++ [take j (tab !! i) ++ [v] ++ drop (j+1) (tab !! i)] ++ drop (i+1) tab
        valoresPossiveis = [x | x <- [1..n], valida (i,j) x tab reg]
        valida (i,j) x tab reg = notElem x (valoresRegiao tab reg (reg !! i !! j)) && 
                                 notElem x (valoresLinha i tab) &&
                                 notElem x (valoresColuna j tab)
        valoresLinha i tab = filter (/=0) (tab !! i)
        valoresColuna j tab = filter (/=0) (map (!!j) tab)
        n = length tab

main :: IO ()
main = do
    print $ solve tabuleiro1 regioes1