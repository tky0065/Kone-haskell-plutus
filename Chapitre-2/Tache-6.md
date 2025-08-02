## ✅ HC2T6 - Comprendre Int vs Integer

### Code :

```haskell
-- Main.hs

-- Définition des variables avec types spécifiques
smallNumber :: Int
smallNumber = 2^62

bigNumber :: Integer
bigNumber = 2^127

-- Fonction pour tester les limites
testIntLimits :: IO ()
testIntLimits = do
  putStrLn "=== Comparaison Int vs Integer ==="
  putStrLn ""
  putStrLn ("smallNumber (Int) = " ++ show smallNumber)
  putStrLn ("bigNumber (Integer) = " ++ show bigNumber)
  putStrLn ""
  putStrLn "Test des limites :"
  putStrLn ("2^62 :: Int = " ++ show (2^62 :: Int))
  putStrLn ("2^127 :: Integer = " ++ show (2^127 :: Integer))
  putStrLn ""
  putStrLn "Tentative avec 2^64 :: Int :"
  putStrLn "-- 2^64 :: Int  -- Peut causer un dépassement !"
  putStrLn ("2^64 (sans annotation) = " ++ show (2^64))

main :: IO ()
main = testIntLimits
```

### Instructions GHCi :

```ghci
-- Dans GHCi, testez ces expressions :

-- Ceci fonctionne (dans la limite)
2^62 :: Int

-- Ceci peut causer un dépassement
2^64 :: Int

-- Ceci fonctionne toujours (précision arbitraire)
2^127 :: Integer
2^1000 :: Integer
```

### Explication :

* **Int** :
  - Type d'entier à **taille fixe** (généralement 64 bits sur CPU 64 bits)
  - **Plus rapide** en calcul car taille fixe
  - **Risque de dépassement** pour de très grandes valeurs
  - Plage typique : -2^63 à 2^63-1

* **Integer** :
  - Type d'entier à **précision arbitraire**
  - **Jamais de dépassement** - peut gérer des nombres très grands
  - **Plus lent** car allocation dynamique de mémoire
  - Limité seulement par la mémoire disponible

* **Recommandation** :
  - Utilisez `Int` pour des calculs rapides avec des nombres "normaux"
  - Utilisez `Integer` quand vous avez besoin de très grandes valeurs
