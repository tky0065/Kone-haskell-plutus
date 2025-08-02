## ✅ HC2T5 - Définir et utiliser des fonctions

### Code :

```haskell
-- Main.hs

-- 1. Fonction circleArea qui prend un rayon (Float) et retourne l'aire du cercle
circleArea :: Float -> Float
circleArea r = pi * r * r

-- 2. Fonction maxOfThree qui prend trois Int et retourne la valeur maximale
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree x y z = max x (max y z)

-- Version alternative de maxOfThree avec des conditions
maxOfThreeAlt :: Int -> Int -> Int -> Int
maxOfThreeAlt x y z
  | x >= y && x >= z = x
  | y >= z = y
  | otherwise = z

main :: IO ()
main = do
  putStrLn "=== Définir et utiliser des fonctions ==="
  putStrLn ""
  
  -- Tests de circleArea
  putStrLn "Tests de circleArea :"
  let radius1 = 3.0
  let radius2 = 5.5
  let radius3 = 1.0
  putStrLn ("Aire d'un cercle de rayon " ++ show radius1 ++ " = " ++ show (circleArea radius1))
  putStrLn ("Aire d'un cercle de rayon " ++ show radius2 ++ " = " ++ show (circleArea radius2))
  putStrLn ("Aire d'un cercle de rayon " ++ show radius3 ++ " = " ++ show (circleArea radius3))
  putStrLn ""
  
  -- Tests de maxOfThree
  putStrLn "Tests de maxOfThree :"
  putStrLn ("maxOfThree 5 8 3 = " ++ show (maxOfThree 5 8 3))
  putStrLn ("maxOfThree 12 7 15 = " ++ show (maxOfThree 12 7 15))
  putStrLn ("maxOfThree 4 4 4 = " ++ show (maxOfThree 4 4 4))
  putStrLn ("maxOfThree (-1) (-5) (-2) = " ++ show (maxOfThree (-1) (-5) (-2)))
```

### Explication :

* **circleArea :: Float -> Float** :
  - Calcule l'aire d'un cercle avec la formule A = π × r²
  - Utilise la constante prédéfinie `pi` en Haskell
  - Type `Float` pour les nombres à virgule flottante

* **maxOfThree :: Int -> Int -> Int -> Int** :
  - Prend trois entiers en paramètres
  - Retourne le maximum des trois
  - Utilise la fonction `max` intégrée de Haskell de façon imbriquée

* **Version alternative avec gardes** :
  - `maxOfThreeAlt` utilise des gardes (`|`) pour des conditions
  - Plus lisible pour des logiques conditionnelles complexes
  - `otherwise` est équivalent à `True` (cas par défaut)

**Sortie attendue** :
- Aire pour rayon 3.0 ≈ 28.27
- Aire pour rayon 5.5 ≈ 95.03
- Maximum de (5,8,3) = 8
