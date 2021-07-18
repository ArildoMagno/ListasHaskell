
--FALTA FAZER: 13

--1)Crie uma função para verificar se uma letra é minúscula (Não use funções do Prelude).
letra_minuscula:: Char -> Bool
letra_minuscula letra | letra >= 'a' && letra <= 'z' = True
                      | otherwise = False

--2)Crie uma função para retornar o valor absoluto
valor_absoluto:: Int -> Int
valor_absoluto valor | valor > 0 = valor
                     | otherwise = -valor
                    
--3)Crie uma função que recebe um número e retorne seu antecessor se for maior que zero
numero_antecessor:: Int -> Int
numero_antecessor numero | numero > 0 = numero - 1
                         | otherwise = -1

                 
--4)Crie três versões de uma mesma função que ao receber três valores x y e z,
-- retorne 10 se x for 7, retorne 20 se y for 8, e retorna 30 se z for 9.
--Em uma versão utilize padrões de função, outra deve ter variáveis anônimas e a ultima,
--comando de guarda ou condicional
--v1:Guarda
--v2:Padroes de funcao
--v3:Variavel Anonima
numero4_v1:: Int -> Int -> Int -> Int
numero4_v1 x y z | x == 7 = 10
                 | y == 8 = 20
                 | z == 9 = 30
                 | otherwise = -1

numero4_v2:: Int -> Int -> Int -> Int
numero4_v2 7 _ _ = 10
numero4_v2 _ 8 _ = 20
numero4_v2 _ _ 9 = 30
numero4_v2 _ _ _ = -1


numero4_v3:: Int -> Int -> Int -> Int
numero4_v3 7 _ _ = 10
numero4_v3 _ 8 _ = 20
numero4_v3 _ _ 9 = 30
numero4_v3 _ _ _ = -1




--5) Crie uma função utilizando variáveis anônimas para definir a função lógica “and”
funcao_and:: Bool -> Bool -> Bool
funcao_and True True = True
funcao_and _ _ = False


--6) Crie uma função utilizando variáveis anônimas para definir a função lógica “or”
funcao_or:: Bool -> Bool -> Bool
funcao_or False False = False
funcao_or _ _ = True



--7)Crie uma funcao comDesconto::Float -> Float, que dado uma mercadoria, calcule o valor com desconto
calcula_desconto::Float->Float->Float
calcula_desconto valor desconto = valor - (desconto/100)

comDesconto::Float->Float
comDesconto valor | valor < 50 = calcula_desconto valor 0
                  | valor >= 50 && valor <= 100 = calcula_desconto valor 5
                  | valor >= 100 && valor <= 300 = calcula_desconto valor 10
                  | valor > 300 = calcula_desconto valor 15
                  | otherwise = -1

--8)Faça uma função recursiva para calcular a potência de dois.
potDois::Int->Float
potDois potencia = 2^potencia



--9)Dados três comprimentos de lados, verifique se podem formar um triangulo.
tipoComprimentoTriangulo::Int->Int->Int->String
tipoComprimentoTriangulo x y z | (x>(y+z)) || (y>(x+z)) || (z>(x+y)) = "Nao e um triangulo"
                               | (x==y) && (y==z) = "Equilatero"
                               | (x==y) || (y==z) || (x==z) = "Isosceles"
                               | (x/=y) || (y/=z) || (x/=z) = "Escaleno"
                               | otherwise = "-1"



--10)Dado um valor monetário em Reais, faça um programa que devolva uma tupla,
--que contenha 3 tuplas, contendo o valor em real, euro, e dolar ex: ((1,real),(1.8,dolar),(2.3,euro))
type Moeda = (String,Float)
type Conversao = (Moeda,Moeda,Moeda)
valorRealConvertido::Float->Conversao
valorRealConvertido valor1= f_calcula_moeda valor1

f_calcula_moeda::Float->Conversao
f_calcula_moeda valor2= (f_calcula_cada_moeda "real" valor2,(f_calcula_cada_moeda "euro" valor2),f_calcula_cada_moeda "dolar" valor2)

f_calcula_cada_moeda::String->Float->Moeda
f_calcula_cada_moeda tipo valor3 | tipo == "real" = (tipo,valor3)
                                 | tipo == "euro" = (tipo,(valor3*0.17))
                                 | tipo == "dolar" = (tipo,(valor3*0.20))
                                 | otherwise = ("sem tipoo",-1)





--11)Crie uma função para calcular a expressão 3*5, usando uma definição recursiva
recurisvo_tres_cinco::Int
recurisvo_tres_cinco = multiplicar_recursivo 3 5

multiplicar_recursivo :: Int -> Int -> Int
multiplicar_recursivo m n | n >= 1        = m + multiplicar_recursivo (n-1) m
                          | otherwise     = 0









--12)Dado um numero natural n>0, n é dito perfeito se a soma de seus divisores, incluindo o 
--numero 1 é igual ao prórpio n. Por exemplo, 6 é perfeito, seus dividores = 1+2+3=6.
--Faça uma funcao eperfeito(n) que informe se n é perfeito
--ver como vou fazer, porque preciso rever listas, mas a lógica é , se n mod contador ==0, adiciona
--e depois soma os elementos da lista

eperfeito::Int->Bool
eperfeito numero | eperfeito_calculo (calcula_divisores_numero_sem_ele numero) == numero = True
                 | otherwise = False

eperfeito_calculo::[Int]->Int
eperfeito_calculo lista = sum(lista)


calcula_divisores_numero::Int->[Int]
calcula_divisores_numero n= [x | x <- [1..n], n`mod`x == 0]

calcula_divisores_numero_sem_ele::Int->[Int]
calcula_divisores_numero_sem_ele n = remover_ultimo_elemento_lista (calcula_divisores_numero n)

remover_ultimo_elemento_lista::[Int]->[Int]
remover_ultimo_elemento_lista [] = []
remover_ultimo_elemento_lista (_:[]) = []
remover_ultimo_elemento_lista (x:xs) = x : remover_ultimo_elemento_lista xs





{--
13) Considere o algoritmo a seguir que gera uma seqüência de números
naturais não nulos, a partir de um número natural n > 0. 
Se n for par,divida-o por 2. 
Se n for ímpar, multiplique-o por 3 e some 1. 

Repita este processo com o novo valor de n, até que ele seja igual a 1, se possível.

Por exemplo, para n = 22, a seqüência 
é: 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2 e 1.
Para cada n, define-se o tamanho do ciclo de n como a quantidade de números da seqüência gerada, 
incluindo o número 1.
No exemplo acima, o tamanho do ciclo para n = 22 é 16.
 
Defina uma função tamciclo(n) que dê como resultado o tamanho do ciclo de n.
--}


tam_ciclo::Float->Int
tam_ciclo numeral= tam_lista (adicionar_lista_ciclo [] numeral)


tam_lista::[Float]->Int
tam_lista [] =0
tam_lista (_:xs) = 1+tam_lista xs


adicionar_lista_ciclo::[Float]->Float->[Float]
adicionar_lista_ciclo [] numeral = [] ++ [numeral]
adicionar_lista_ciclo (_:xs) numeral = xs ++ [calculo_ciclo_base numeral]





calculo_ciclo_base::Float->Float
calculo_ciclo_base numero | toInt(numero) `mod` 2 == 0 =  toFloat(toInt(numero))/2
                          | otherwise = toFloat((toInt(numero)*3)+1)



toInt :: Float -> Int
toInt x = round x
toFloat :: Int -> Float
toFloat x = toFloat x


--CORRIGIR BUG QUE TA SAINDO SÓ 1





--14)Dados dois números naturais, x e y, ambos maiores que zero, defina uma função mdc(x,y) que dê 
--como resultado o máximo divisor comum entre x e y
calcula_mmc :: Integral a => a -> a -> a
calcula_mmc a b | a == 0 = 0
        | b == 0 = 0
        | a == b = a
        | otherwise = div (a * b) (calcula_mdc a b)

calcula_mdc :: Integral a => a -> a -> a
calcula_mdc a b | mod a b == 0 = b
        | mod b a == 0 = a
        | a > b = calcula_mmc b (mod a b)
        | a < b = calcula_mmc a (mod b a)
        | otherwise = -1








