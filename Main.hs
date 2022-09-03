--RAFAEL BAUER SAMPAIO

--1. Escreva  uma  função  para  o  cálculo  dos  números  da  sequência  de  Fibonacci utilizando Haskell.
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci(x-1) + fibonacci(x-2)

--2. Um dos primeiros algoritmos documentados é o algoritmo para o cálculo do Maior Divisor Comum (MDC) de Euclides publicado por volta do ano 300 AC. Podemos simplificar este algoritmo dizendo que dados dois inteiros A e B, o MDC entre eles será dado pelo valor absoluto de A se B=0 e pelo MDC entre B e o resto da divisão de A por B se B>0. Escreva uma função para o cálculo do MDC entre dois números inteiros positivos, usando o algoritmo de Euclides conforme apresentado aqui, utilizando Haskell.
mdc :: Int -> Int -> Int
mdc a b
  |b==0 = a
  |b>0 = mdc b (mod a b)

--3. Escreva uma função recursiva que dado um número inteiro n, devolva a soma dos dígitos deste número. Exemplo: dado 1234 a função deverá devolver 10. Utilizando Haskell e recursividade.
somaDig :: Int -> Int
somaDig 0 = 0
somaDig x = (x `mod` 10) + somaDig (x `div` 10)

--4. Escreva uma função que devolva a soma de todos os números menores que 10000 que sejam múltiplos de 3 ou 5.
somaMenor10k :: Int
somaMenor10k = sum[x|x <- [1..9999], ((mod x 3 == 0) || (mod x 5 == 0))]

--5. Escreva uma função que, recebendo uma lista de inteiros, apresente a diferença entre a soma dos quadrados e o quadrado da soma destes inteiros, usando recursividade.
--Não cheguei a uma solução
{-listaInt :: [Int] -> Int
listaInt x = somaQuad - soma
  where
    let somaQuad (x:xs) = x^2 + listaInt(xs)
    let soma (z:zs) = z + listaInt(zs)
-}


--6. O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. Crie uma função que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos os números primos menores que um determinado inteiro dado.
--Não cheguei a uma solução
--eulerSieve :: Int -> [Int]

--7. Nem só de Fibonacci vivem os exemplos de recursão. Escreva uma função que devolva todos os números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123) menores que um inteiro dado.
--Não cheguei a uma solução
{-lucas :: Int -> [Int]
lucas 2 = 2
lucas 1 = 1
lucas (x:xs) = x + lucas(xs)-}

--8. Escreva uma função, chamada aoContrario em Haskel para reverter uma lista. Dado [1,2,3] devolva [3,2,1].
aoContrario :: [Int] -> [Int]
aoContrario [] = []
aoContrario (x:xs) = aoContrario xs ++ [x]

--9. Escreva uma função chamada somaRecursiva que recebe dois valores inteiros e devolve o produto destes valores sem usar o operador de multiplicação.
somaRecursiva :: Int -> Int -> Int
somaRecursiva x y
  |y==0=x
  |otherwise = somaRecursiva(x+1) (y-1)

--10. Escreva uma função chamada comprimento que receba uma lista de inteiros e devolva o comprimento desta lista. Observe que você não pode usar nenhuma função que já calcule o comprimento de uma lista.
comprimento :: [Int] -> Int
comprimento [] = 0
comprimento (x:xs) = 1 + comprimento xs


main = do
  print("Fibonacci, entrada: 10, saida: ", fibonacci 10)
  print("MDC, entrada: 15 e 27, saida: ", mdc 15 27)
  print("Soma digitos, entrada: 1234, saida: ", somaDig 1234)
  print("Soma menor que 10 mil, entrada:, saida: ", somaMenor10k)
  --print(listaInt [1,2,3,4,5])
  print("Ao contrario, entrada: [1,2,3], saida: ", aoContrario [1,2,3])
  print("Soma recursiva, entrada: 2 3, saida:", somaRecursiva 2 3)
  print("Comprimento, entrada [1,2,3,4,5,6,7,8], saida: ", comprimento [1,2,3,4,5,6,7,8])

  
