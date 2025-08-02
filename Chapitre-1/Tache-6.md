
## ✅ HC1T6 - Signature de Type (Addition)

### Code :

```haskell
-- Main.hs
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- Version avec application partielle
addFive :: Int -> Int
addFive = addNumbers 5

-- Version plus générale
addGeneric :: Num a => a -> a -> a
addGeneric x y = x + y

main :: IO ()
main = do
  let a = 3
  let b = 4
  putStrLn ("Addition de " ++ show a ++ " et " ++ show b ++ " : " ++ show (addNumbers a b))
  
  putStrLn "Démonstration de l'application partielle :"
  putStrLn ("addFive 10 = " ++ show (addFive 10))
  putStrLn ("addFive 7 = " ++ show (addFive 7))
  
  putStrLn "Version générique avec des Float :"
  let x = 3.5 :: Float
  let y = 2.7 :: Float
  putStrLn ("Addition de " ++ show x ++ " et " ++ show y ++ " : " ++ show (addGeneric x y))
```

### Explication :

* Signature `Int -> Int -> Int` : deux entiers en entrée, retourne un entier.
* **Currification** : `addNumbers 5` crée une nouvelle fonction qui ajoute 5.
* **Application partielle** : très utile pour créer des fonctions spécialisées.
* `Num a =>` permet d'utiliser avec n'importe quel type numérique (Int, Float, Double, etc.).
* **Sortie** : Démontre l'addition normale, partielle et générique.
