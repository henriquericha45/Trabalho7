-- Henrique Levandoski Richa
import Data.Char

{- 1. Utilizando a linguagem Haskell e o seu próprio tipo de dados, crie um programa capaz de
classificar triângulos a partir do comprimento dos seus lados sabendo que o comprimento
de cada um dos lados deve ser maior que zero e que:
a. triângulos equiláteros têm todos os lados do mesmo tamanho;
b. triangulos esosceles têm, no mínimo, dois lados do mesmo tamanho;
c. triangulos escalenos têm tem todos os lados de tamanho diferentes;
d. triangulos degenerados têm um lado igual a soma dos outros dois e área zero. -}
type Triangulo = String
classificaTriangulo :: Float -> Float -> Float -> Triangulo
classificaTriangulo n1 n2 n3 
    | n1 <= 0 || n2 <= 0 || n3 <= 0 = "NaoTriangulo"
    | n1 == n2 && n2 == n3 = "Equilatero"
    | n1 == n2 || n2 == n3 || n1 == n3 = "Isosceles"
    | n1 /= n2 && n2 /= n3 && n1 /= n3 = "Escaleno"
    | n1 == n2 + n3 || n2 == n1 + n3 || n3 == n1 + n2 = "Degenerado"

{- 2. Usando Haskell, crie uma função chamada fatias, com a assinatura dada por fatias:: :: Int ->
String -> [[Int]] que receba um string e um inteiro e devolva uma lista de listas contendo em
cada item uma lista de inteiros. Esta função receberá strings contendo digitos como, por
exemplo: "345234678" e devolverá listas parecidas com
[[3,4,5],[4,5,2],[5,2,3],[2,3,4],[3,4,6],[4,6,7],[6,7,8]] No caso do exemplo, o inteiro que fatias
recebeu foi 3. Observe que você poderá criar, quantas funções de apoio acredite que sejam
necessárias para criar as funcionalidades de fatias inclusive, se achar interessante, pode
usar as funções mapMaybe e digitToInt. -}
fatias :: Int -> String -> [[Int]]
fatias number text = map (map digitToInt) (fatiasAjuda number text)

fatiasAjuda :: Int -> String -> [String]
fatiasAjuda number text
    | length text < number = []
    | otherwise = take number text : fatiasAjuda number (tail text)

{- 3. Usando Haskell escreva uma função chamada romanos que receba um inteiro menor ou
igual a 3000 e devolva um string deste inteiro representado com algarismos romanos. -}
romanos :: Int -> String
romanos n = 
    if (n<1) || n>3000
      then error "Número inválido"
    else romanosAjuda n

romanosAjuda :: Int -> String
romanosAjuda n
    | n >= 1000 = "M" ++ romanosAjuda (n - 1000)
    | n >= 900 = "CM" ++ romanosAjuda (n - 900)
    | n >= 500 = "D" ++ romanosAjuda (n - 500)
    | n >= 400 = "CD" ++ romanosAjuda (n - 400)
    | n >= 100 = "C" ++ romanosAjuda (n - 100)
    | n >= 90 = "XC" ++ romanosAjuda (n - 90)
    | n >= 50 = "L" ++ romanosAjuda (n - 50)
    | n >= 40 = "XL" ++ romanosAjuda (n - 40)
    | n >= 10 = "X" ++ romanosAjuda (n - 10)
    | n >= 9 = "IX" ++ romanosAjuda (n - 9)
    | n >= 5 = "V" ++ romanosAjuda (n - 5)
    | n >= 4 = "IV" ++ romanosAjuda (n - 4)
    | n >= 1 = "I" ++ romanosAjuda (n - 1)
    | otherwise = ""


{- 4. Usando linguagem Haskell, escreva uma função que recebe uma lista de listas de inteiros
com até 5 digitos em cada lista e devolva apenas as listas que contenham palíndromes
primos. Por exemplo na lista de listas [[1,1,1], [1,2,2], [3,1,3], [3,1,5]] apenas o elemento
[3,1,3] é um palíndrome primo. Uma ferramenta importante para criar suas listas de teste
pode ser encontrada em: Prime Number Calculator (calculatorsoup.com). -}
isPalindrome :: Int -> Bool
isPalindrome n = show n == reverse (show n)

isPrime :: Int -> Bool
isPrime n = n>1 && all (\x -> mod n x /= 0) xs
    where xs = takeWhile (\y -> y^2 <= n) [2..]

palindromePrimes :: [[Int]] -> [[Int]]
palindromePrimes xs = filter (\x -> isPrime (read (concat (map show x))) && isPalindrome (read (concat (map show x)))) xs


{- 5. Usando a linguagem Haskell escreva uma função, chamada ultimoNome que receba o nome
completo de uma pessoa e devolva apenas o último sobrenome sem qualquer vogal. Caso
o ultimo sobrenome não contenhuma nenhuma vogal devolva o ultimo sobrenome que
ainda contenha vogal. Por exemplo se o nome for Ana Maria stzrx, a função deve devolver
Maria, se o nome for Silvia Silva a função deve devolver Slv. -}
ultimoNome :: String -> String
ultimoNome nome = [x | x <- (reverse (takeWhile (/= ' ') (reverse nome))), not (elem x "aeiouAEIOU")]

main = do
  putStr "Func. 1; entrada: 1 2 3; resultado: "
  print(classificaTriangulo 0 1 1)

  putStr "Func. 1; entrada: 2 2 2; resultado: "
  print(classificaTriangulo 2 2 2)

  putStr "Func. 1; entrada: 2 2 1; resultado: "
  print(classificaTriangulo 2 2 1)

  putStr "Func. 1; entrada: 1 2 3; resultado: "
  print(classificaTriangulo 1 2 3)

  putStr "Func. 1; entrada: 1,2,3; resultado: "
  print(classificaTriangulo 9 4 5)

  putStr "Func. 2; entrada: 3 \"345678\";resultado: "
  print(fatias 3 "345234678")

  putStr "Func. 3; entrada: 4;resultado: "
  print(romanos 2022)

  putStr "Func. 4; entrada: ;resultado: "
  print(palindromePrimes [[1,1,1], [1,2,2], [3,1,3], [3,1,5]])

  putStr "Func. 5; entrada: \"Silva Silva\";resultado: "
  print(ultimoNome "Silva Silva")

  putStr "Func. 5; entrada: \"Henrique Richa\";resultado: "
  print(ultimoNome "Henrique Richa")