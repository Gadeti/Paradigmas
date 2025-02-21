-- Archivo: Main.hs
module Main where

-- Tipo de dato para el inventario: (Nombre, Precio, Cantidad)
type Inventory = [(String, Double, Int)]

-- 1. Agregar un nuevo producto al inventario
addProduct :: Inventory -> String -> Double -> Int -> Inventory
addProduct inventory name price quantity = inventory ++ [(name, price, quantity)]

-- 2. Actualizar la cantidad de un producto existente
updateQuantity :: Inventory -> String -> Int -> Inventory
updateQuantity [] _ _ = []
updateQuantity ((n, p, q):xs) name newQuantity
    | n == name = (n, p, newQuantity) : xs
    | otherwise = (n, p, q) : updateQuantity xs name newQuantity

-- 3. Eliminar un producto del inventario
removeProduct :: Inventory -> String -> Inventory
removeProduct inventory name = filter (\(n, _, _) -> n /= name) inventory

-- 4. Resumen del inventario: total de productos en stock y valor total
inventorySummary :: Inventory -> (Int, Double)
inventorySummary inventory = (totalQuantity, totalValue)
  where
    totalQuantity = sum [q | (_, _, q) <- inventory]
    totalValue    = sum [p * fromIntegral q | (_, p, q) <- inventory]

-- Funciones de expansión:

-- 5. Buscar un producto por su nombre.
-- Devuelve Nothing si no se encuentra, o Just (precio, cantidad) si se encuentra.
searchProduct :: Inventory -> String -> Maybe (Double, Int)
searchProduct [] _ = Nothing
searchProduct ((n, p, q):xs) name
    | n == name = Just (p, q)
    | otherwise = searchProduct xs name

-- 6. Aplicar un descuento a todos los productos.
-- El parámetro 'discount' es un valor entre 0 y 1 (por ejemplo, 0.1 para un 10% de descuento).
applyDiscount :: Inventory -> Double -> Inventory
applyDiscount inventory discount = 
  map (\(n, p, q) -> (n, p * (1 - discount), q)) inventory

-- Ejemplo de uso en la función main:
main :: IO ()
main = do
  -- Inventario inicial vacío
  let inventory = [] :: Inventory
  
  -- Agregar productos
  let inventory1 = addProduct inventory "Manzanas" 0.5 100
  let inventory2 = addProduct inventory1 "Plátanos" 0.3 150
  
  -- Actualizar cantidad de "Manzanas"
  let inventory3 = updateQuantity inventory2 "Manzanas" 120
  
  -- Buscar producto "Plátanos"
  case searchProduct inventory3 "Plátanos" of
    Just (price, qty) -> putStrLn $ "Plátanos - Precio: " ++ show price ++ ", Cantidad: " ++ show qty
    Nothing         -> putStrLn "Plátanos no encontrado."
  
  -- Aplicar descuento del 10% a todos los productos
  let inventory4 = applyDiscount inventory3 0.1
  
  -- Eliminar "Plátanos" del inventario
  let inventoryFinal = removeProduct inventory4 "Plátanos"
  
  -- Obtener resumen del inventario
  let (totalQty, totalValue) = inventorySummary inventoryFinal
  
  -- Mostrar resultados
  putStrLn $ "\nInventario Final: " ++ show inventoryFinal
  putStrLn $ "Total de productos en stock: " ++ show totalQty
  putStrLn $ "Valor total del inventario: " ++ show totalValue
