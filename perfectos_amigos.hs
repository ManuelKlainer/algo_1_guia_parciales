import Test.HUnit
import Data.List (sort)

divisoresPropiosAux :: Integer -> Integer -> [Integer]
divisoresPropiosAux _ 1 = [1]
divisoresPropiosAux n i
  | mod n i == 0 = i : divisoresPropiosAux n (i-1)
  | otherwise    = divisoresPropiosAux n (i - 1)


divisoresPropios :: Integer -> [Integer]
divisoresPropios 1 = []
divisoresPropios n = divisoresPropiosAux n (n-1)

sumarLista :: [Integer] -> Integer
sumarLista [] = 0
sumarLista (x:xs) = x + sumarLista xs

sumarDivisores :: Integer -> Integer
sumarDivisores a = sumarLista (divisoresPropios a)

sonAmigos :: Integer -> Integer -> Bool
sonAmigos a b = (a == sumarDivisores b) && (b == sumarDivisores a)

esPefecto :: Integer -> Bool
esPefecto n = n == sumarDivisores n

siguientePerfecto :: Integer -> [Integer]
siguientePerfecto n | esPefecto n = n : siguientePerfecto (n+1)
                    | otherwise = siguientePerfecto (n+1)

losPrimerosNPerfectos :: Integer -> [Integer]
losPrimerosNPerfectos n = tomar n (siguientePerfecto 2)
  where
    tomar 0 _ = []
    tomar m (x:xs) = x : tomar (m-1) xs

listaDeAmigos :: [Integer] -> [(Integer, Integer)]
listaDeAmigos (x:y:xs)
  | sonAmigos x y = (x,y) : listaDeAmigos xs
  | otherwise = listaDeAmigos xs
listaDeAmigos _ = []
test_divisoresPropios :: Test
test_divisoresPropios = TestList [
    "divisores propios de 1 (ninguno)" ~: sort(divisoresPropios 1) ~?= [],
    "divisores propios de 6" ~: sort(divisoresPropios 6) ~?= [1,2,3],
    "divisores propios de 28" ~: sort(divisoresPropios 28) ~?= [1,2,4,7,14],
    "divisores propios de primo" ~: sort(divisoresPropios 13) ~?= [1]
    ]

test_sonAmigos :: Test
test_sonAmigos = TestList [
    "220 y 284 son amigos" ~: sonAmigos 220 284 ~?= True,
    "1184 y 1210 son amigos" ~: sonAmigos 1184 1210 ~?= True,
    "6 y 28 no son amigos" ~: sonAmigos 6 28 ~?= False,
    "13 y 17 no son amigos" ~: sonAmigos 13 17 ~?= False
    ]


test_losPrimerosNPerfectos :: Test
test_losPrimerosNPerfectos = TestList [
    "primer número perfecto" ~: losPrimerosNPerfectos 1 ~?= [6],
    "primeros 2 números perfectos" ~: losPrimerosNPerfectos 2 ~?= [6,28],
    "primeros 3 números perfectos" ~: losPrimerosNPerfectos 3 ~?= [6,28,496],
    "ningún número perfecto (n=0)" ~: losPrimerosNPerfectos 0 ~?= []
    ]

test_listaDeAmigos :: Test
test_listaDeAmigos = TestList [
    "una pareja de amigos" ~: listaDeAmigos [220,284] ~?= [(220,284)],
    "dos parejas de amigos" ~: listaDeAmigos [220,284,1184,1210] ~?= [(220,284),(1184,1210)],
    "lista con números sin amigos" ~: listaDeAmigos [6,10,13] ~?= [],
    "lista vacía" ~: listaDeAmigos [] ~?= []
    ]

main :: IO ()
main = do
  runTestTT $ TestList [
    test_divisoresPropios,
    test_sonAmigos,
    test_losPrimerosNPerfectos,
    test_listaDeAmigos
    ]
  return ()
