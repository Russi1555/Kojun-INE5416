import Data.List
import Data.Maybe

type Coordenada = (Int, Int)
type Regiao = [Coordenada]
type Tabuleiro = [[Int]]

tabuleiro1::Tabuleiro
tabuleiro1=[
     [2,0,0,0,1,0],
     [0,0,0,3,0,0],
     [0,3,0,0,5,3],
     [0,0,0,0,0,0],
     [0,0,3,0,4,2],
     [0,0,0,0,0,0]]

regioes1::[Regiao]
regioes1=[         
         [(0,0),(0,1)],
         [(0,2),(0,3),(0,4)],
         [(0,5),(1,5)],
         [(1,0),(1,1),(1,2),(1,3),(1,4),(2,4)],
         [(2,0),(3,0),(3,1),(3,2)],
         [(2,1),(2,2),(2,3),(3,3)],
         [(2,5),(3,4),(3,5)],
         [(4,0),(4,1)],
         [(5,0),(5,1)],
         [(4,2),(5,2),(5,3)],
         [(4,3),(4,4),(4,5),(5,4),(5,5)]]

getRegiao::[Regiao]->Coordenada->Regiao
getRegiao lista_regioes coordenada = head $ filter (elem coordenada) lista_regioes

checkAdjacencias::[[Int]]->[Bool]
checkAdjacencias lista = do
    let res = map (\x -> zip x (tail x)) lista--me devolve uma lista de listas de tuplas onde cada tupla é um par de valores adjacentes: exemplo [1,2,3] vira [(1,2),(2,3)]
    let res1 = concatMap (\lst -> map (\t -> t) lst) res --me devolve uma lista com todas as tuplas
    map (\(x,y)-> x /= 0 && y /= 0 && x == y) res1 --me devolve uma lista de booleanos


verificaAdjacencias::Tabuleiro -> IO()
verificaAdjacencias tab = print (adjacente_horizontal && adjacente_vertical)--all checkLinha linhas && all checkColuna colunas
    where   
        linhas = tab --O tabuleiro já esta arrumado em forma de linhas
        colunas = map (\i -> map (!!i) tab) [0..length linhas -1] --retorna lista das colunas     
        adjacente_horizontal = all not $ checkAdjacencias linhas --caso todos os booleanos sejam False, não há nenhuma adjacencia horizontal
        adjacente_vertical = all not $ checkAdjacencias colunas
    


main::IO()
main = do
    verificaAdjacencias tabuleiro1
    print ""
    