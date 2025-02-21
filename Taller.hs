-- Agregar un nuevo producto al inventario
addProduct :: [(String, Double, Int)] -> String -> Double -> Int -> [(String, Double, Int)]
addProduct inventory name price quantity = inventory ++ [(name, price, quantity)]

-- Actualizar la cantidad de un producto existente
updateQuantity :: [(String, Double, Int)] -> String -> Int -> [(String, Double, Int)]
updateQuantity [] _ _ = []
updateQuantity ((n, p, q):xs) name newQuantity
    | n == name = (n, p, newQuantity) : xs
    | otherwise = (n, p, q) : updateQuantity xs name newQuantity

-- Eliminar un producto del inventario
removeProduct :: [(String, Double, Int)] -> String -> [(String, Double, Int)]
removeProduct inventory name = filter (\(n, _, _) -> n /= name) inventory

-- Resumen del inventario: total de productos y valor total
inventorySummary :: [(String, Double, Int)] -> (Int, Double)
inventorySummary inventory = (totalQuantity, totalValue)
  where
    totalQuantity = sum [q | (_, _, q) <- inventory]
    totalValue = sum [p * fromIntegral q | (_, p, q) <- inventory]

-- Buscar un producto: devuelve (precio, cantidad) o (0,0) si no se encuentra
searchProduct :: [(String, Double, Int)] -> String -> (Double, Int)
searchProduct [] _ = (0, 0)
searchProduct ((n, p, q):xs) name =
    if n == name then (p, q)
    else searchProduct xs name

-- Aplicar un descuento a todos los productos del inventario.
-- 'discount' es un valor entre 0 y 1 (ejemplo: 0.1 para 10% de descuento)
applyDiscount :: [(String, Double, Int)] -> Double -> [(String, Double, Int)]
applyDiscount [] _ = []
applyDiscount ((n, p, q):xs) discount =
    (n, p * (1 - discount), q) : applyDiscount xs discount

-- Ejemplo de Prueba:
main :: IO ()
main = do
    let inventory = [] :: [(String, Double, Int)]
    let inventory1 = addProduct inventory "Manzanas" 0.5 100
    let inventory2 = addProduct inventory1 "Platanos" 0.3 150
    let inventory3 = updateQuantity inventory2 "Manzanas" 120
    
    -- Prueba: Buscar el producto "Manzanas"
    let (price, quantity) = searchProduct inventory3 "Manzanas"
    putStrLn $ "Buscar Producto 'Manzanas': Precio = " ++ show price ++ ", Cantidad = " ++ show quantity

    -- Prueba: Aplicar un descuento del 10% a todos los productos
    let inventoryDiscounted = applyDiscount inventory3 0.1
    putStrLn $ "Inventario con descuento aplicado: " ++ show inventoryDiscounted

    let inventory4 = removeProduct inventory3 "Platanos"
    let (totalQty, totalValue) = inventorySummary inventory4
    
    putStrLn $ "Inventario Final: " ++ show inventory4
    putStrLn $ "Total de productos en stock: " ++ show totalQty
    putStrLn $ "Valor total del inventario: " ++ show totalValue
