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

-- Consultar el inventario: total de productos en stock y valor total
inventorySummary :: [(String, Double, Int)] -> (Int, Double)
inventorySummary inventory = (totalQuantity, totalValue)
  where
    totalQuantity = sum [q | (_, _, q) <- inventory]
    totalValue    = sum [p * fromIntegral q | (_, p, q) <- inventory]

-- Función principal para probar las operaciones
main :: IO ()
main = do
  let inventory = [] :: [(String, Double, Int)]
  
  -- Agregar productos al inventario
  let inventory1 = addProduct inventory "Manzanas" 0.5 100
  let inventory2 = addProduct inventory1 "Plátanos" 0.3 150
  
  -- Actualizar la cantidad de "Manzanas"
  let inventory3 = updateQuantity inventory2 "Manzanas" 120
  
  -- Eliminar "Plátanos" del inventario
  let inventoryFinal = removeProduct inventory3 "Plátanos"
  
  -- Obtener resumen del inventario
  let (totalQty, totalVal) = inventorySummary inventoryFinal
  
  putStrLn $ "Inventario Final: " ++ show inventoryFinal
  putStrLn $ "Total de productos en stock: " ++ show totalQty
  putStrLn $ "Valor total del inventario: " ++ show totalVal

