## ✅ HC2T1 - Vérification des types dans GHCi

### Code :

```haskell
-- Main.hs
main :: IO ()
main = do
  putStrLn "=== Vérification des types dans GHCi ==="
  putStrLn ""
  putStrLn "1. 42 :: Int"
  putStrLn ("   Valeur: " ++ show (42 :: Int))
  putStrLn ""
  putStrLn "2. 3.14 :: Double"
  putStrLn ("   Valeur: " ++ show (3.14 :: Double))
  putStrLn ""
  putStrLn "3. \"Haskell\" :: String"
  putStrLn ("   Valeur: " ++ show ("Haskell" :: String))
  putStrLn ""
  putStrLn "4. 'Z' :: Char"
  putStrLn ("   Valeur: " ++ show ('Z' :: Char))
  putStrLn ""
  putStrLn "5. True && False :: Bool"
  putStrLn ("   Valeur: " ++ show (True && False :: Bool))
```

### Instructions GHCi :

```ghci
-- Dans GHCi, tapez ces commandes pour vérifier les types :
:t 42
-- 42 :: Num a => a

:t 3.14
-- 3.14 :: Fractional a => a

:t "Haskell"
-- "Haskell" :: String

:t 'Z'
-- 'Z' :: Char

:t (True && False)
-- (True && False) :: Bool
```

### Explication :

* **42** : Type `Int` ou `Integer` selon le contexte (polymorphe numérique)
* **3.14** : Type `Float` ou `Double` selon le contexte (polymorphe fractionnaire)
* **"Haskell"** : Type `String` (liste de caractères)
* **'Z'** : Type `Char` (caractère unique entre apostrophes)
* **True && False** : Type `Bool` (opération logique ET qui retourne `False`)

**Note** : Les types numériques en Haskell sont polymorphes par défaut, c'est pourquoi GHCi affiche des contraintes de classe comme `Num a => a`.
