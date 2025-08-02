
## ✅ HC1T8 - Fonction d'Ordre Supérieur

### Code :

```haskell
-- Main.hs
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Généralisation : appliquer n fois
applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = f (applyN (n-1) f x)

-- Fonctions de test
double :: Int -> Int
double x = x * 2

addOne :: Int -> Int
addOne x = x + 1

negateNum :: Int -> Int
negateNum x = -x

main :: IO ()
main = do
  let x = 3
  
  putStrLn "Fonction applyTwice avec différentes fonctions :"
  putStrLn ("  applyTwice double " ++ show x ++ " = " ++ show (applyTwice double x))
  putStrLn ("  applyTwice addOne " ++ show x ++ " = " ++ show (applyTwice addOne x))
  putStrLn ("  applyTwice negateNum " ++ show x ++ " = " ++ show (applyTwice negateNum x))
  
  putStrLn "\nFonction applyN (appliquer n fois) :"
  putStrLn ("  applyN 0 double " ++ show x ++ " = " ++ show (applyN 0 double x))
  putStrLn ("  applyN 3 double " ++ show x ++ " = " ++ show (applyN 3 double x))
  putStrLn ("  applyN 5 addOne " ++ show x ++ " = " ++ show (applyN 5 addOne x))
  
  putStrLn "\nAvec des listes :"
  let list = [1,2,3]
  putStrLn ("  applyTwice reverse " ++ show list ++ " = " ++ show (applyTwice reverse list))
  putStrLn ("  applyTwice tail " ++ show list ++ " = " ++ show (applyTwice tail list))
```

### Explication :

* **Fonction d'ordre supérieur** : prend une fonction comme paramètre.
* `(a -> a)` : fonction qui prend et retourne le même type.
* **Type polymorphe** `a` permet d'utiliser avec n'importe quel type.
* `applyN` généralise le concept pour appliquer une fonction n fois.
* **Exemples variés** : nombres, listes, différentes opérations.
* **Sortie** : Démontre la flexibilité des fonctions d'ordre supérieur.
