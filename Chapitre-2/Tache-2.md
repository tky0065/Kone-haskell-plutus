## ✅ HC2T2 - Signatures de fonctions

### Code :

```haskell
-- Main.hs

-- 1. Fonction add qui prend deux Int et retourne leur somme
add :: Int -> Int -> Int
add x y = x + y

-- 2. Fonction isEven qui prend un Int et retourne un Bool
isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

-- 3. Fonction concatStrings qui prend deux String et retourne leur concaténation
concatStrings :: String -> String -> String
concatStrings str1 str2 = str1 ++ str2

main :: IO ()
main = do
  putStrLn "=== Test des signatures de fonctions ==="
  putStrLn ""
  
  -- Test de add
  let result1 = add 5 3
  putStrLn ("add 5 3 = " ++ show result1)
  
  -- Test de isEven
  let result2 = isEven 4
  let result3 = isEven 7
  putStrLn ("isEven 4 = " ++ show result2)
  putStrLn ("isEven 7 = " ++ show result3)
  
  -- Test de concatStrings
  let result4 = concatStrings "Hello" " World"
  putStrLn ("concatStrings \"Hello\" \" World\" = " ++ show result4)
```

### Explication :

* **add :: Int -> Int -> Int** : 
  - Prend deux paramètres de type `Int`
  - Retourne un `Int` (leur somme)
  - Utilise l'opérateur `+` pour additionner

* **isEven :: Int -> Bool** :
  - Prend un paramètre de type `Int`
  - Retourne un `Bool` (True si pair, False si impair)
  - Utilise `mod` pour calculer le reste de la division par 2

* **concatStrings :: String -> String -> String** :
  - Prend deux paramètres de type `String`
  - Retourne un `String` (concaténation des deux)
  - Utilise l'opérateur `++` pour concaténer

**Note** : En Haskell, les flèches `->` dans les signatures séparent les types des paramètres et le type de retour.
