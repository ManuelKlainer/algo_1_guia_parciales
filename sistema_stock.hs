import Test.HUnit

type Mercaderia = [String]
type Stock = [(String, Integer)]
type Precios = [(String, Float)]

buscarEnStock :: Stock -> String -> Bool
buscarEnStock [] _ = False
buscarEnStock (x:xs) s | fst x == s = True
                       | otherwise = buscarEnStock xs s

valorEnStock :: Stock -> String -> Integer
valorEnStock [] _ = 0
valorEnStock (x:xs) s | fst x == s = snd x
                      | otherwise = valorEnStock xs s

actualizarStock :: (String, Integer) -> Stock -> Stock
actualizarStock (s,i) [] = [(s,i)]
actualizarStock (s,i) ((x,v):xs)
    | x == s = (x,v+i) : xs
    | otherwise = (x,v) : actualizarStock (s,i) xs

generarStockAux :: Mercaderia -> Stock -> Stock
generarStockAux [] s = s
generarStockAux (x:xs) s = generarStockAux xs (actualizarStock (x, 1) s)

generarStock :: Mercaderia -> Stock
generarStock [] = []
generarStock m = generarStockAux m []

stockDeProducto :: Stock -> String -> Integer
stockDeProducto [] _ = 0
stockDeProducto ((x,v):xs) s
    | x==s = v
    | otherwise = stockDeProducto xs s


obtenerPrecio :: String -> Precios -> Float
obtenerPrecio s [] = 0
obtenerPrecio s ((x,v):xs)
    | s == x = v
    | otherwise = obtenerPrecio s xs

transformarAPrecio :: Stock -> Precios -> [Float]
transformarAPrecio [] _ = []
transformarAPrecio ((x,v):xs) pr = (obtenerPrecio x pr * fromIntegral v) : transformarAPrecio xs pr


sumarPreciosAux :: [Float] -> Float -> Float
sumarPreciosAux [] s = s
sumarPreciosAux (x:xs) s = sumarPreciosAux xs (s+x)

sumarPrecios :: [Float] -> Float
sumarPrecios l = sumarPreciosAux l 0

dineroEnStock :: Stock -> Precios -> Float
dineroEnStock s p = sumarPrecios (transformarAPrecio s p)

aplicarOferta :: Stock -> Precios -> Precios
aplicarOferta _ [] = []
aplicarOferta s ((x,v):xs)
    | (stockDeProducto s x) > 10 = (x,v*0.80) : aplicarOferta s xs
    | otherwise = (x,v) : aplicarOferta s xs

testGenerarStock :: Test
testGenerarStock = TestList [
    TestCase (assertEqual "Lista vacía"
        [] (generarStock [])),

    TestCase (assertEqual "Lista con un solo producto"
        [("manzana", 1)] (generarStock ["manzana"])),

    TestCase (assertEqual "Lista con productos repetidos"
        [("manzana", 2), ("banana", 1)] (generarStock ["manzana", "banana", "manzana"])),

    TestCase (assertEqual "Lista con todos productos distintos"
        [("manzana",1), ("banana",1), ("pera",1)] (generarStock ["manzana", "banana", "pera"]))
  ]

testStockDeProducto :: Test
testStockDeProducto = TestList [
    TestCase (assertEqual "Producto existente en stock"
        5 (stockDeProducto [("manzana", 5), ("banana", 2)] "manzana")),

    TestCase (assertEqual "Producto existente pero con otro orden"
        2 (stockDeProducto [("banana", 2), ("manzana", 5)] "banana")),

    TestCase (assertEqual "Producto inexistente en stock"
        0 (stockDeProducto [("manzana", 5), ("banana", 2)] "pera")),

    TestCase (assertEqual "Stock vacío"
        0 (stockDeProducto [] "manzana"))
  ]

testDineroEnStock :: Test
testDineroEnStock = TestList [
    TestCase (assertEqual "Stock y precios vacíos"
        0 (dineroEnStock [] [])),

    TestCase (assertEqual "Un producto en stock"
        30 (dineroEnStock [("manzana", 3)] [("manzana", 10.0)])),

    TestCase (assertEqual "Múltiples productos en stock"
        55 (dineroEnStock [("manzana", 3), ("banana", 5)]
                          [("manzana", 10.0), ("banana", 5.0)])),

    TestCase (assertEqual "Productos con distintos precios"
        79 (dineroEnStock [("manzana", 2), ("banana", 3), ("pera", 4)]
                          [("manzana", 12.0), ("banana", 7.0), ("pera", 8.5)]))
  ]

testAplicarOferta :: Test
testAplicarOferta = TestList [
    TestCase (assertEqual "Stock y precios vacíos"
        [] (aplicarOferta [] [])),

    TestCase (assertEqual "Un producto con stock menor o igual a 10 (sin descuento)"
        [("manzana", 10.0)] (aplicarOferta [("manzana", 5)] [("manzana", 10.0)])),

    TestCase (assertEqual "Un producto con stock mayor a 10 (con descuento)"
        [("manzana", 8.0)] (aplicarOferta [("manzana", 15)] [("manzana", 10.0)])),

    TestCase (assertEqual "Varios productos mezclados"
        [("manzana", 8.0), ("banana", 5.0), ("pera", 12.0)]
        (aplicarOferta [("manzana", 15), ("banana", 5), ("pera", 8)]
                       [("manzana", 10.0), ("banana", 5.0), ("pera", 12.0)])),

    TestCase (assertEqual "Todos los productos con descuento"
        [("manzana", 8.0), ("banana", 4.0)]
        (aplicarOferta [("manzana", 11), ("banana", 12)]
                       [("manzana", 10.0), ("banana", 5.0)]))
  ]


main :: IO ()
main = do
  counts <- runTestTT $ TestList [testAplicarOferta]
  print counts