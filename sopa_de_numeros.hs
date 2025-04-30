import Test.HUnit
import Data.List (sort)

type Fila = [Integer]
type Tablero = [Fila]
type Posicion = (Integer, Integer)
type Camino = [Posicion]

{- Ejercicio 5 -}

maximoFila :: Fila -> Integer
maximoFila [] = 0
maximoFila (x:xs) | x > maximoFila xs = x
                 | otherwise = maximoFila xs

maximo :: Tablero -> Integer
maximo [] = 0
maximo (x:xs) | (maximoFila x) > maximo xs = maximoFila x
              | otherwise = maximo xs

{- Ejercicio 6 -}

frecuenciasFila :: [Integer] -> [(Integer, Integer)]
frecuenciasFila [] = []
frecuenciasFila (x:xs) = (x, contar x (x:xs)) : frecuenciasFila (eliminar x (x:xs))
    where
        contar :: Integer -> [Integer] -> Integer
        contar _ [] = 0
        contar n (y:ys)
            | n == y = 1 + contar n ys
            | otherwise = contar n ys
        eliminar :: Integer -> [Integer] -> [Integer]
        eliminar _ [] = []
        eliminar n (y:ys)
            | n == y = eliminar n ys
            | otherwise = y : eliminar n ys

unirFrecuencias :: [(Integer, Integer)] -> [(Integer, Integer)] -> [(Integer, Integer)]
unirFrecuencias [] ys = ys
unirFrecuencias xs [] = xs
unirFrecuencias xs ys = unirAux xs (combinar ys)
    where
        unirAux [] acc = acc
        unirAux ((ax, bx):xs) acc = unirAux xs (añadirOActualizar ax bx acc)

        añadirOActualizar :: Integer -> Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
        añadirOActualizar ax bx [] = [(ax, bx)]
        añadirOActualizar ax bx ((ay, by):ys)
            | ax == ay = (ay, bx + by) : ys
            | otherwise = (ay, by) : añadirOActualizar ax bx ys
        
        combinar :: [(Integer, Integer)] -> [(Integer, Integer)]
        combinar [] = []
        combinar ((ay, by):ys) = añadirOActualizar ay by (combinar ys)

frecuencias :: Tablero -> [(Integer, Integer)]
frecuencias [] = []
frecuencias (x:xs) = unirFrecuencias (frecuenciasFila x) (frecuencias xs)

masRepetidoAux :: [(Integer, Integer)] -> Integer
masRepetidoAux ((ax, bx):xs) = masRepetidoAux2 ax bx xs
    where
        masRepetidoAux2 :: Integer -> Integer -> [(Integer, Integer)] -> Integer
        masRepetidoAux2 mA mB [] = mA
        masRepetidoAux2 mA mB ((ay, by):ys)
            | by > mB = masRepetidoAux2 ay by ys
            | otherwise = masRepetidoAux2 mA mB ys

masRepetido :: Tablero -> Integer
masRepetido t = masRepetidoAux (frecuencias t)

{- Ejercicio 7 -}

obtenerValorFila :: Fila -> Integer -> Integer
obtenerValorFila [] n = 0
obtenerValorFila (x:xs) n | n==1 = x
                          | otherwise = obtenerValorFila xs (n-1)

obtenerValorColumna :: Tablero -> Integer -> Fila
obtenerValorColumna [] n = []
obtenerValorColumna (x:xs) n | n==1 = x
                             | otherwise = obtenerValorColumna xs (n-1)

obtenerValorTablero :: Tablero -> Integer -> Integer -> Integer
obtenerValorTablero tablero fila columna = obtenerValorFila (obtenerValorColumna tablero fila) columna

valoresDeCamino :: Tablero -> Camino -> [Integer]
valoresDeCamino _ [] = []
valoresDeCamino tablero ((x,y):xs) = (obtenerValorTablero tablero x y) : (valoresDeCamino tablero xs)

{- Ejercicio 8 -}

siguienteFibonacci :: Integer -> Integer
siguienteFibonacci 0 = 1
siguienteFibonacci 1 = 1
siguienteFibonacci n = hallarSiguiente 0 1 n
  where
    hallarSiguiente a b objetivo
      | a == objetivo = b
      | otherwise = hallarSiguiente b (a+b) objetivo

construirListaFibonacci :: Integer -> Integer -> Integer -> [Integer]
construirListaFibonacci _ _ 0 = []
construirListaFibonacci a b n = a : construirListaFibonacci b (a + b) (n-1)

listaFibonacci :: Integer -> Integer -> [Integer]
listaFibonacci i n = construirListaFibonacci i (siguienteFibonacci i) n

compararListas :: [Integer] -> [Integer] -> Bool
compararListas [] [] = True
compararListas (x:xs) (y:ys) = (x==y) && (compararListas xs ys)

longitudLista :: [Integer] -> Integer
longitudLista [] = 0
longitudLista (x:xs) = 1 + longitudLista xs

esCaminoFibo :: [Integer] -> Integer -> Bool
esCaminoFibo l i = compararListas l (listaFibonacci i (longitudLista l))


-------------------------
 -- -- -- TEST -- -- --

--------------------------
-- EJERCICIO 5: maximo
--------------------------

testMaximo = TestCase $ do
    let tablero = [[5, 1, 7], [3, 13, 2], [4, 9, 8]]
    assertEqual "maximo tablero" (maximo tablero) 13

--------------------------
-- EJERCICIO 6: masRepetido
--------------------------

testMasRepetido = TestCase $ do
    let tablero = [[1,2,3], [4,2,5], [6,2,1]]
    let resultado = masRepetido tablero
    assertBool "masRepetido debe ser 2 o 1" (resultado == 2 || resultado == 1)

--------------------------
-- EJERCICIO 7: valoresDeCamino
--------------------------

testValoresDeCamino = TestCase $ do
    let tablero = [[1,2,3],[4,5,6],[7,8,9]]
    let camino = [(1,1),(1,2),(2,2),(3,2)]
    assertEqual "valoresDeCamino" (valoresDeCamino tablero camino) [1,2,5,8]

--------------------------
-- EJERCICIO 8: esCaminoFibo
--------------------------

testEsCaminoFibo = TestCase $ do
    assertEqual "esCaminoFibo verdadero" (esCaminoFibo [1,1,2,3,5,8] 0) False
    assertEqual "esCaminoFibo falso" (esCaminoFibo [1,2,3,6] 0) False



main :: IO ()
main = runTestTTAndExit $ TestList
    [testMaximo
    , testMasRepetido
    , testValoresDeCamino
    , testEsCaminoFibo
    ]