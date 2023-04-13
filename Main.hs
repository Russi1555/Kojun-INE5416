import Data.List


readColuna::Int->[Int]->[Int] 
readColuna n a = [a !! n, a !! (n+9)]

{-
***********************
readColunarec
n = deve começar em 0, onde começa a ler a coluna
a = lista a ser lida
x = coluna devolvida
***********************
-}
readColunarec::Int->[Int]->[Int]->[Int]
readColunarec n _ _
  | n > 35 = [] --trocar 35 pelo numero total de celular do tabuleiro
readColunarec n a x= x++[a!!n]++readColunarec (n+6) a x --trocar o 6 pelo valor do tabuleiro 6x6, 10x10, etc

{-
***********************
readLinharec
n = numero da linha a ser lida
a = lista a ser lida
x = linha devolvida
***********************
-}
readLinharec::Int->[Int]->[Int]->[Int]
readLinharec n _ _
  | n > 5 = [] --trocar 5 pelo numero de celular por linha
readLinharec n a x = x++[a!!n]++readLinharec(n+1) a x

{-
***********************
readRegiao
n = numero da regiao a ser lida
m = iterador dentro da regiao sendo lida (tem que começar em 0)
a = lista de lista onde cada sublista é uma regiao
x = regiao devolvida
***********************
-}
readRegiao::Int->Int->[[Int]]->[Int]->[Int]
readRegiao n m a _
  |m >  length (a!!n)-1 = []

readRegiao n m a x = x++[a!!n!!m]++readRegiao n (m+1) a x

{-
PROBLEMA DE MERDA. VER DEPOIS. FAZER FUNCIONAR COM UM TABULEIRO 6X6 INPUTADO NA MÃO MESMO

setNumero::[a]->Float->Float->Int->[Int]
setNumero lista a b numero = do
  let tamanho_lista = fromIntegral (length lista) :: Float --transforma o tamanho da lista em float para
  let n = sqrt tamanho_lista --calcular a raiz quadrada que nos dara o N do valor NxN do tabuleiro
  let posicao = round (b + (a*n)) :: Int
  let lista = take posicao lista ++ [numero] ++ drop(posicao+1) lista
  lista
-}

{-
***********************
replaceAtIndex
lista = lista
indice = indice onde será substituido o valor
numero = numero que será inserido
***********************
-}
replaceAtIndex :: [Int] -> Int -> Int -> [Int]
replaceAtIndex lista indice numero = take indice lista ++ [numero] ++ drop (indice + 1) lista

{-
***********************
procuraRegiaoTrivial

Descrição: varre a lista de regiões procurando e corrigindo regiões com valores implícitos de células.
Caso 1: Região 1x1, só pode haver o valor '1' na célula daquela região
Caso 2: Região Coluna (1xN). No exemplo de uma região 1x3 a única solução possível é
  [3]
  [2]
  [1]


Objetivo: Preencher o tabuleiro o máximo o possível antes de iniciar o back-tracking para minimizar o número de iterações dele.
***********************
-}
procuraRegiaoTrivial :: [Int] -> [[Int]] -> Int -> [Int]
procuraRegiaoTrivial tabuleiro regiao x = do
  let tamanho = length (readRegiao 0 0 regiao [])
  let posicao_desejada = head(readRegiao 0 0 regiao [])

  ---ao encontrar uma região de tamanho 1, a única possibilidade é que a celular tenha valor 1 
  if tamanho < 2
    then replaceAtIndex tabuleiro posicao_desejada 1

  else tabuleiro
   
main = do
  let tabuleiro_teste = [1..100] --Começar testando com um tabuleiro 10x10
  let tabuleiro_zerado = [0 | _ <- [1..100]] -- posso fazer uma função que da o tabuleiro zerado de NxN tamanho a partir disso depois
  let regioes = [[0,1,10,11],[2,3,4]]
  
  --TABULEIRO SIMPLES E REGIÕES SIMPLES REPRESENTAM O TABULEIRO 6X6 DO LINK https://www.janko.at/Raetsel/Kojun/001.a.htm
  let tabuleiro_simples = [2,0,0,0,1,0,0,0,0,3,0,0,0,3,0,0,5,3,0,0,0,0,0,0,0,0,3,0,4,2,0,0,0,0,0,0] 
  --let tabuleiroTemp = replaceAtIndex tabuleiro_simples 0 7
  --let tabuleiro_simples = tabuleiroTemp
  print  tabuleiro_simples
  let regioes_simples = [[0,1],[2,3,4],[5,11],[6,7,8,9,10,16],[12,18,19,20],[13,14,15,21],[17,22,23],[24,25],[30,31],[26,32,33],[27,28,29,34,35]]

  let linha = readLinharec 0 tabuleiro_simples []
  print linha
  let coluna = readColunarec 4 tabuleiro_simples []
  print coluna
  let regiao1 = readRegiao 0 0 regioes_simples []
  print regiao1
  let tabuleiroTemp = procuraRegiaoTrivial tabuleiro_simples regioes_simples 0
  let tabuleiro_simples = tabuleiroTemp
  print  tabuleiro_simples