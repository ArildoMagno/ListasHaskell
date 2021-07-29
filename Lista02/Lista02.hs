--
--Aluno: Arildo Magno
--Matricula: 0040117    
--


--1) Crie uma função para retornar a cabeça de uma lista.
cabeca_lista:: [a] -> a
cabeca_lista lista = head lista



--2) Cria uma função para retornar a cauda de uma lista.
cauda_lista:: [a] -> [a]
cauda_lista lista = tail lista



--3)Crie uma função que receba uma lista de inteiros com tamanho maior 
--que três e retorne a soma dos três primeiros elementos.
soma_tres_primeiros::[Float]->Float
soma_tres_primeiros lista= sum(get_n 3 lista)


get_n:: Int -> [Float] -> [Float]
get_n 0 _ = []
get_n _ [] = []
--aqui ele pega o primeiro elemento da lista, adiciona em uma nova lista usando o sinal :
--e chama recursivo decrementando o contador 
--é onde fica o x que vai o elemento, no : é para continuar recursivo
get_n n (x:xs) = x : get_n (n-1) xs







--4)Crie uma função recursiva para verificar se um determinado elemento
--pertence a uma lista.
verifica_elemento_lista :: (Eq a) => a -> [a] -> Bool
verifica_elemento_lista a [] = False
verifica_elemento_lista a (x:xs) = if a == x then True
                     else verifica_elemento_lista
                     a xs




--5)Crie uma função recursiva chamada zipar, ela recebe duas listas e
--retorna uma lista com tuplas dos elementos da mesma posição Ex:
--[1,2] [a,b] => [(1,a),(2,b)].
zipar (h1:t1) (h2:t2) = [(h1,h2)] ++ zipar t1 t2
zipar [] [] = []



--6)Crie uma função recursiva que recebe uma lista de inteiros e
--retorna uma lista com o dobro de cada elemento da primeira lista.
dobro::[Float]->[Float]
dobro [] = []
--é onde fica o x que vai o elemento, no : é para continuar recursivo
dobro (x:xs) = x*x : dobro xs






--7) Defina a função and_list :: [Bool] -> Bool que retorna a conjunção
--da lista. Por exemplo, andList [e1; e2;...;en] = e1&&e2&&...&&en

and_list :: [Bool] -> Bool
and_list (h:hs) = h && and_list hs
and_list [] = True



--8)Crie uma função recursiva que insere um elemento na posição “x” de
--uma lista.

inserir_posicao_lista :: a -> Int -> [a] -> [a]
inserir_posicao_lista novo _ [] = [novo]
inserir_posicao_lista novo i (x:xs) | i <= 0 = novo:x:xs
                                    | otherwise = x : inserir_posicao_lista novo (i - 1) xs





--9) Crie uma função recursiva que insere um elemento na última posição
--de uma lista caso ele não exista.
insere_ultima_posicao::(Eq a) => a ->[a]->[a]
insere_ultima_posicao a lista | (verifica_elemento_lista a lista)==False = inserir_posicao_lista a (length(lista)) lista
                              | otherwise = []





--10)Crie uma função recursiva que recebe uma lista de inteiros e
--retorna o maior elemento.
maior_elemento_lista::[Int] -> Int
maior_elemento_lista lista= encontra_maior lista 0

encontra_maior :: [Int] -> Int -> Int
encontra_maior [] acc = acc
encontra_maior (h : rest) acc
  | h > acc = encontra_maior
 rest h
  | otherwise = encontra_maior
 rest acc



-- 11) Defina as seguintes listas por compreensão:

-- a) [0,3,6,9,12,15]
mult_tres :: [Integer]
mult_tres = [x | x <- [0 .. 15], x `mod` 3 == 0]

-- b) Os múltiplos de 2 e 3 entre 0 e 20.
multiplos_dois_tres :: [Integer]
multiplos_dois_tres = [x | x <- [0 .. 20], even x && x `mod` 3 == 0]

-- c) [[1],[2],[3],[4],[5]]
listas_encadeadas :: [[Integer]]
listas_encadeadas = [[x] | x <- [1 .. 5]]

-- d) [[1], [1,1], [1,1,1], [1,1,1,1], [1,1,1,1,1]]
listas_encadeadas_uns :: (Eq t, Num t, Num a) => t -> [a]
listas_encadeadas_uns 0 = []
listas_encadeadas_uns n = 1 : listas_encadeadas_uns (n - 1)

lista_N :: [[Integer]]
lista_N = [listas_encadeadas_uns x | x <- [1 .. 5]]


-- e)[(1,3),(1,2),(1,1),(2,3),(2,2),(2,1),(3,3),(3,2),(3,1)].
listas_encadeadas_tuplas :: [(Integer, Integer)]
listas_encadeadas_tuplas = [(x, y) | x <- [1, 2, 3], y <- [3, 2, 1]]






--12)Defina uma função que dada uma lista de inteiros, retorna o número
--de elementos de valor superior a um número n qualquer.
lista_elementos_maiores_que_x::Ord a=> a->[a]->[a]
lista_elementos_maiores_que_x numero lista = filter (> numero) lista



--13)Faça uma função que recebe duas listas e faça a interseção delas.
--Obs: a interseção não tem repetição de elementos.
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect (x:xs) l | elem x l = x : intersect xs l
                   | otherwise = intersect xs l




--14)



-- 15) Função para sequência: recebe dois números naturais n e m, e
-- retorna uma lista com n elementos, onde o primeiro é m, o segundo é m+1,
-- etc...
-- Ex.: sequencia 0 2 ==> []
-- sequencia 3 4 ==> [4,5,6]
sequencia :: (Num a, Enum a) => Int -> a -> [a]
sequencia n m = [x | x <- take n [m, m + 1 ..]]