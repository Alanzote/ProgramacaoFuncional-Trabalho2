-- Aluno: Alan Renato Bunese
-- Disciplina: Programação Funcional
-- Professor: Frank Alcantara
module Main (main) where

{- 1
 - Escreva uma função chamada soma1 que recebe um inteiro como argumento
 - e retorna um inteiro uma unidade maior que a entrada.
 -}
soma1 :: Int -> Int
soma1 x = x + 1

{- 2
 - Escreva uma função chamada sempre que, não importando o valor de entrada,
 - devolva sempre 0. Observe que neste caso a entrada pode ser de qualquer tipo.
 -}
sempre :: any -> Int
sempre x = 0

{- 3
 - Escreva uma função chamada treco que receba três valores em ponto flutuantes
 - com precisão dupla e retorne o resultado da soma dos dois primeiros multiplicado
 - pelo terceiro.
 -}
treco :: Double -> Double -> Double -> Double
treco x y z = (x + y) * z

{- 4
 - Escreva uma função chamada resto que devolta o resto da divisão entre dois
 - números inteiros.
 -}
resto :: Int -> Int -> Int
resto x y = x `mod` y

{- 5
 - Escreva uma função chamada precoMaior que devolva o maior valor entre quatro
 - valores monetários.
 -}
precoMaior :: Double -> Double -> Double -> Double -> Double
precoMaior a b c d = maximum [a, b, c, d]

{- 6
 - Escreva uma função chamada impar que devolta True, sempre que o resultado do
 - produto de dois números inteiros for ímpar.
 -}
impar :: (Int, Int) -> Bool
impar (x, y) = (x * y) `mod` 2 == 1

{- 7
 - Em Haskell existe o tipo par cuja assinatura tem a seguinte forma: par :: (Int, Int).
 - Escreva uma função em Haskell que devolva a soma dos componentes de um par de inteiros.
 -}
somaPar :: (Int, Int) -> Int
somaPar (x, y) = x + y

{- 8
 - Escreva uma função em Haskell que receba números reais (double) e desolva o resultado
 - da equação x**2 + y/2 + z
 -}
equacao :: Double -> Double -> Double -> Double
equacao x y z = x**2 + y/2 + z

{- 9
 - Escreva uma função em Haskell chamada diagnostico que receba o peso do aluno e impriva
 - um diagnóstico de obesidade, segundo a tabela que pode ser encontrada no link:
 - Sobrepeso, obesidade e obesidade mórbida: entenda a diferença entre os três termos (cuidadospelavida.com.br)
 - Observe que este diagnóstico é meramente estatístico e não tem nenhum valor real, está sendo usado nesta
 - questão apenas para definição das faixas. Todo e qualquer diagnóstico deve ser feito por um profissional
 - médico.
 -}
diagnostico :: Float -> String
diagnostico x
    | x < 17 = "Muito abaixo do peso"
    | x < 18.49 = "Abaixo do peso"
    | x < 24.99 = "Peso normal"
    | x < 20.99 = "Sobrepeso"
    | x < 34.99 = "Obesidade leve"
    | x < 39.00 = "Obesidade severa"
    | otherwise = "Obesidade mórbida"

{- 10
 - Escreva uma função em Haskell chamada bissexto que receba um ano e devolva True se o ano for bisexto sabendo
 - que anos bissextos obedecem a seguinte regra:
 - Todos os anos que sejam divisíveis por 4
 - Exceto os anos que são mútiplos de 100
 - Exceto os anos que são múltiplos de 400
 - 1997 não é bissexto, 1900 não é bissexto e 2000 é bissexto.
 -}
bissexto :: Int -> Bool
bissexto x
 | (x `mod` 100 /= 0 || x `mod` 400 == 0) && x `mod` 4 == 0 = True
 | otherwise = False

-- Main...
main :: IO ()
main = do
    -- Como querem uma questão por linha, assim será...
    print ("soma1: entrada: 3; resultado: " ++ show (soma1 3))
    print ("soma1: entrada: -2; resultado: " ++ show (soma1 (-2)))

    print ("sempre: entrada: 3; resultado: " ++ show (sempre 3))
    print ("sempre: entrada: -2; resultado: " ++ show (sempre (-2)))

    print ("treco: entrada: 3.0, 4.0, 5.0; resultado: " ++ show (treco 3.0 4.0 5.0))
    print ("treco: entrada: -2.0, -3.0, -4.0; resultado: " ++ show (treco (-2.0) (-3.0) (-4.0)))

    print ("resto: entrada: 3, 4; resultado: " ++ show (resto 3 4))
    print ("resto: entrada: -2, -3; resultado: " ++ show (resto (-2) (-3)))

    print ("precoMaior: entrada: 3.0, 4.0, 5.0, 6.0; resultado: " ++ show (precoMaior 3.0 4.0 5.0 6.0))
    print ("precoMaior: entrada: -3.2, -4.2, -5.2, -6.2; resultado: " ++ show (precoMaior (-3.2) (-4.2) (-5.2) (-6.2)))

    print ("impar: entrada: (3, 5); resultado: " ++ show (impar (3, 5)))
    print ("impar: entrada: (-2, -3); resultado: " ++ show (impar (-2, -3)))

    print ("equacao: entrada: 3.0, 4.0, 5.0; resultado: " ++ show (equacao 3.0 4.0 5.0))
    print ("equacao: entrada: -2.0, -3.0, -4.0; resultado: " ++ show (equacao (-2.0) (-3.0) (-4.0)))

    print ("somaPar: entrada: (3, 5); resultado: " ++ show (somaPar (3, 5)))
    print ("somaPar: entrada: (-2, -3); resultado: " ++ show (somaPar (-2, -3)))

    print ("diagnostico: entrada: 17.0; resultado: " ++ diagnostico 17.0)
    print ("diagnostico; entrada: 25; resultado: " ++ diagnostico 25)
    print ("diagnostico; entrada: 30; resultado: " ++ diagnostico 30)
    print ("diagnostico; entrada: 40; resultado: " ++ diagnostico 40)

    print ("bissexto: entrada: 1997; resultado: " ++ show (bissexto 1997))
    print ("bissexto: entrada: 1900; resultado: " ++ show (bissexto 1900))
    print ("bissexto: entrada: 2000; resultado: " ++ show (bissexto 2000))
