
## ✅ HC1T5 - Liste Infinie (Paresse)

### Code :

```haskell
-- Main.hs
infiniteNumbers :: [Int]
infiniteNumbers = [1..]

takeN :: Int -> [Int]
takeN n = take n infiniteNumbers

-- Autres exemples de listes infinies
infiniteOdds :: [Int]
infiniteOdds = [1,3..]

infiniteFibs :: [Int]
infiniteFibs = 0 : 1 : zipWith (+) infiniteFibs (tail infiniteFibs)

main :: IO ()
main = do
  putStrLn "Premiers 10 nombres naturels :"
  putStrLn ("  " ++ show (takeN 10))
  
  putStrLn "Premiers 8 nombres impairs :"
  putStrLn ("  " ++ show (take 8 infiniteOdds))
  
  putStrLn "Premiers 12 nombres de Fibonacci :"
  putStrLn ("  " ++ show (take 12 infiniteFibs))
  
  putStrLn "Le 100ème nombre naturel :"
  putStrLn ("  " ++ show (infiniteNumbers !! 99)) -- index 99 = 100ème élément
```

### Explication :

* `[1..]` crée une **liste infinie** grâce à la **paresse** (lazy evaluation).
* `take n` prend **seulement les n premiers éléments** sans calculer toute la liste.
* La **paresse** permet de définir des structures infinies sans problème de mémoire.
* `infiniteFibs` montre une définition récursive élégante de Fibonacci.
* **Sortie** : Différents exemples d'utilisation de listes infinies.
