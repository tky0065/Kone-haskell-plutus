## ✅ HC2T3 - Variables immuables

### Code :

```haskell
-- Main.hs

-- Définition des variables immuables
myAge :: Int
myAge = 25

piValue :: Double
piValue = 3.14159265359

greeting :: String
greeting = "Bonjour tout le monde!"

isHaskellFun :: Bool
isHaskellFun = True

-- Tentative de "modification" (ceci ne fonctionne pas en Haskell)
-- myAge = 26  -- ERREUR : Multiple declarations of 'myAge'

main :: IO ()
main = do
  putStrLn "=== Variables immuables en Haskell ==="
  putStrLn ""
  putStrLn ("Mon âge: " ++ show myAge)
  putStrLn ("Valeur de pi: " ++ show piValue)
  putStrLn ("Message: " ++ greeting)
  putStrLn ("Haskell est-il amusant? " ++ show isHaskellFun)
  putStrLn ""
  putStrLn "Tentative de modification..."
  putStrLn "-- myAge = 26  -- ERREUR: Cannot redefine variable!"
  putStrLn ""
  putStrLn "En Haskell, les variables sont IMMUABLES !"
```

### Explication :

* **Variables immuables** : Une fois qu'une valeur est assignée à une variable en Haskell, elle ne peut jamais être changée.

* **myAge :: Int** : Variable de type entier avec la valeur 25

* **piValue :: Double** : Variable de type nombre réel double précision

* **greeting :: String** : Variable de type chaîne de caractères

* **isHaskellFun :: Bool** : Variable booléenne

**Important** : Si vous essayez de redéfinir une variable (comme `myAge = 26`), Haskell génèrera une erreur de compilation "Multiple declarations". Cette immutabilité garantit qu'une fois qu'une valeur est définie, elle reste constante dans tout le programme.
