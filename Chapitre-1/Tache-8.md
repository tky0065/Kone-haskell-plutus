## ✅ HC1T8 - Fonction d'Ordre Supérieur

### 🎯 Objectif
Maîtriser les **fonctions d'ordre supérieur** en Haskell - des fonctions qui prennent d'autres fonctions comme paramètres. Comprendre `applyTwice` et sa généralisation `applyN` pour appliquer une fonction plusieurs fois de suite.

### 📝 Concepts Clés
- **Fonction d'ordre supérieur** : Fonction qui accepte une autre fonction comme paramètre
- **applyTwice** : Applique une fonction deux fois de suite : `f(f(x))`
- **applyN** : Généralisation pour appliquer une fonction `n` fois de suite
- **Récursion** : `applyN` utilise la récursion pour compter les applications

### 💻 Code Complet

```haskell
-- Main.hs

-- | Applique une fonction deux fois de suite
-- applyTwice f x = f (f x)
-- Exemple : applyTwice double 5 = double (double 5) = 20
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- | Généralisation : applique une fonction n fois
-- Cas de base : applyN 0 f x = x (zéro application = valeur originale)
-- Cas récursif : applyN n f x = f (applyN (n-1) f x)
applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x                    -- Cas de base : 0 applications
applyN n f x = f (applyN (n-1) f x) -- Récursion : f appliquée puis (n-1) fois

-- | Fonction de test : double un nombre
-- Utilisée pour démontrer applyTwice et applyN
double :: Int -> Int
double x = x * 2

-- | Fonction de test : ajoute 1 à un nombre
-- Autre exemple pour les démonstrations
addOne :: Int -> Int
addOne x = x + 1

-- | Fonction de test : met au carré
-- Démontre une croissance très rapide avec applyN
square :: Int -> Int
square x = x * x

-- | Fonction de test : soustrait 1
-- Inverse de addOne pour montrer différents effets
subtractOne :: Int -> Int
subtractOne x = x - 1

-- | Fonction de test : divise par 2 (avec arrondi vers le bas)
-- Inverse approximatif de double
halve :: Int -> Int
halve x = x `div` 2

-- | Exemples de fonctions avec leurs descriptions
testFunctions :: [(String, Int -> Int)]
testFunctions = [
    ("double (×2)", double),
    ("addOne (+1)", addOne),
    ("square (x²)", square),
    ("subtractOne (-1)", subtractOne),
    ("halve (÷2)", halve)
  ]

-- | Valeurs de test pour les démonstrations
testValues :: [Int]
testValues = [1, 2, 3, 5, 10]

-- | Fonction principale pour démontrer les fonctions d'ordre supérieur
main :: IO ()
main = do
  putStrLn "=== FONCTIONS D'ORDRE SUPÉRIEUR EN HASKELL ==="
  putStrLn ""
  
  -- Explication du concept
  putStrLn "=== Concept de Fonction d'Ordre Supérieur ==="
  putStrLn "Une fonction d'ordre supérieur prend une autre fonction comme paramètre"
  putStrLn ""
  putStrLn "applyTwice :: (a -> a) -> a -> a"
  putStrLn "• Premier paramètre : (a -> a) = une fonction"
  putStrLn "• Deuxième paramètre : a = une valeur"
  putStrLn "• Résultat : a = la fonction appliquée deux fois"
  putStrLn ""
  
  -- Démonstration d'applyTwice
  putStrLn "=== Démonstration d'applyTwice ==="
  putStrLn "applyTwice f x = f (f x)"
  putStrLn ""
  
  mapM_ (\(funcName, func) -> do
    putStrLn $ "Avec la fonction " ++ funcName ++ " :"
    mapM_ (\val -> do
      let result = applyTwice func val
      putStrLn $ "• applyTwice " ++ funcName ++ " " ++ show val ++ " = " ++ show result
      putStrLn $ "  → " ++ funcName ++ " (" ++ funcName ++ " " ++ show val ++ ") = " ++ show result
      ) (take 3 testValues)
    putStrLn ""
    ) (take 3 testFunctions)
  
  -- Démonstration détaillée avec double
  putStrLn "=== Calcul Détaillé avec double ==="
  let exampleVal = 3
  putStrLn $ "applyTwice double " ++ show exampleVal ++ " :"
  putStrLn $ "1. Premier appel : double " ++ show exampleVal ++ " = " ++ show (double exampleVal)
  putStrLn $ "2. Deuxième appel : double " ++ show (double exampleVal) ++ " = " ++ show (double (double exampleVal))
  putStrLn $ "Résultat final : " ++ show (applyTwice double exampleVal)
  putStrLn ""
  
  -- Démonstration d'applyN
  putStrLn "=== Généralisation avec applyN ==="
  putStrLn "applyN n f x applique f exactement n fois à x"
  putStrLn ""
  
  putStrLn "Exemple avec double et différentes valeurs de n :"
  let baseVal = 2
  mapM_ (\n -> do
    let result = applyN n double baseVal
    putStrLn $ "• applyN " ++ show n ++ " double " ++ show baseVal ++ " = " ++ show result
    putStrLn $ "  → " ++ show baseVal ++ " " ++ replicate n '×' ++ " 2^" ++ show n ++ " = " ++ show result
    ) [0, 1, 2, 3, 4, 5]
  putStrLn ""
  
  -- Démonstration avec addOne
  putStrLn "Exemple avec addOne et différentes valeurs de n :"
  let baseVal2 = 10
  mapM_ (\n -> do
    let result = applyN n addOne baseVal2
    putStrLn $ "• applyN " ++ show n ++ " addOne " ++ show baseVal2 ++ " = " ++ show result
    putStrLn $ "  → " ++ show baseVal2 ++ " + " ++ show n ++ " = " ++ show result
    ) [0, 1, 2, 3, 5, 10]
  putStrLn ""
  
  -- Démonstration avec square (croissance exponentielle)
  putStrLn "=== Croissance Rapide avec square ==="
  putStrLn "Attention : square croît très rapidement !"
  let baseVal3 = 2
  mapM_ (\n -> do
    let result = applyN n square baseVal3
    putStrLn $ "• applyN " ++ show n ++ " square " ++ show baseVal3 ++ " = " ++ show result
    case n of
      0 -> putStrLn "  → " ++ show baseVal3 ++ " (aucune application)"
      1 -> putStrLn "  → " ++ show baseVal3 ++ "² = " ++ show result
      2 -> putStrLn "  → (" ++ show baseVal3 ++ "²)² = " ++ show baseVal3 ++ "⁴ = " ++ show result
      3 -> putStrLn "  → ((" ++ show baseVal3 ++ "²)²)² = " ++ show baseVal3 ++ "⁸ = " ++ show result
      _ -> putStrLn "  → " ++ show baseVal3 ++ "^(2^" ++ show n ++ ") = " ++ show result
    ) [0, 1, 2, 3, 4]
  putStrLn ""
  
  -- Comparaison applyTwice vs applyN 2
  putStrLn "=== Comparaison applyTwice vs applyN 2 ==="
  putStrLn "Ces deux expressions sont équivalentes :"
  let testVal = 7
  let result1 = applyTwice double testVal
  let result2 = applyN 2 double testVal
  putStrLn $ "• applyTwice double " ++ show testVal ++ " = " ++ show result1
  putStrLn $ "• applyN 2 double " ++ show testVal ++ " = " ++ show result2
  putStrLn $ "• Égalité : " ++ show (result1 == result2)
  putStrLn ""
  
  -- Démonstration de la récursion dans applyN
  putStrLn "=== Récursion dans applyN ==="
  putStrLn "Comment applyN 3 double 5 est calculé :"
  putStrLn "1. applyN 3 double 5"
  putStrLn "2. = double (applyN 2 double 5)"
  putStrLn "3. = double (double (applyN 1 double 5))"
  putStrLn "4. = double (double (double (applyN 0 double 5)))"
  putStrLn "5. = double (double (double 5))"
  putStrLn "6. = double (double 10)"
  putStrLn "7. = double 20"
  putStrLn "8. = 40"
  putStrLn $ "Vérification : " ++ show (applyN 3 double 5)
  putStrLn ""
  
  -- Applications pratiques
  putStrLn "=== Applications Pratiques ==="
  putStrLn ""
  putStrLn "1. Simulation d'évolution dans le temps :"
  putStrLn $ "   Population qui double chaque année : " ++ show (applyN 5 double 100) ++ " (après 5 ans)"
  putStrLn ""
  putStrLn "2. Calculs itératifs :"
  putStrLn $ "   Ajouter 1 dix fois : " ++ show (applyN 10 addOne 0) ++ " = 10"
  putStrLn ""
  putStrLn "3. Puissances de 2 :"
  putStrLn $ "   2^6 via doublement : " ++ show (applyN 6 double 1) ++ " = 64"
  putStrLn ""
  putStrLn "4. Division successive :"
  putStrLn $ "   1024 divisé par 2 cinq fois : " ++ show (applyN 5 halve 1024) ++ " = 32"
```

### 🔍 Instructions GHCi

```ghci
-- Dans GHCi, tapez ces commandes pour explorer les fonctions d'ordre supérieur :

-- Vérification des types
:t applyTwice
-- applyTwice :: (a -> a) -> a -> a

:t applyN
-- applyN :: Int -> (a -> a) -> a -> a

-- Tests avec applyTwice
applyTwice double 5
-- 20

applyTwice addOne 10
-- 12

applyTwice square 3
-- 81

-- Tests avec applyN
applyN 0 double 10
-- 10

applyN 3 double 2
-- 16

applyN 5 addOne 0
-- 5

-- Création de fonctions spécialisées
let triple = applyN 3
let quadruple = applyN 4
triple addOne 10
-- 13
quadruple double 1
-- 16

-- Application avec des fonctions lambda
applyTwice (\x -> x + 3) 5
-- 11

applyN 4 (\x -> x * 2) 1
-- 16

-- Vérification d'équivalence
applyTwice double 7 == applyN 2 double 7
-- True
```

### 📊 Sortie Attendue
```
=== FONCTIONS D'ORDRE SUPÉRIEUR EN HASKELL ===

=== Concept de Fonction d'Ordre Supérieur ===
Une fonction d'ordre supérieur prend une autre fonction comme paramètre

applyTwice :: (a -> a) -> a -> a
• Premier paramètre : (a -> a) = une fonction
• Deuxième paramètre : a = une valeur
• Résultat : a = la fonction appliquée deux fois

=== Démonstration d'applyTwice ===
applyTwice f x = f (f x)

Avec la fonction double (×2) :
• applyTwice double (×2) 1 = 4
  → double (×2) (double (×2) 1) = 4
• applyTwice double (×2) 2 = 8
  → double (×2) (double (×2) 2) = 8
• applyTwice double (×2) 3 = 12
  → double (×2) (double (×2) 3) = 12

=== Calcul Détaillé avec double ===
applyTwice double 3 :
1. Premier appel : double 3 = 6
2. Deuxième appel : double 6 = 12
Résultat final : 12

=== Généralisation avec applyN ===
applyN n f x applique f exactement n fois à x

Exemple avec double et différentes valeurs de n :
• applyN 0 double 2 = 2
  → 2  × 2^0 = 2
• applyN 1 double 2 = 4
  → 2 × × 2^1 = 4
• applyN 2 double 2 = 8
  → 2 ×× × 2^2 = 8
• applyN 3 double 2 = 16
  → 2 ××× × 2^3 = 16

=== Récursion dans applyN ===
Comment applyN 3 double 5 est calculé :
1. applyN 3 double 5
2. = double (applyN 2 double 5)
3. = double (double (applyN 1 double 5))
4. = double (double (double (applyN 0 double 5)))
5. = double (double (double 5))    -- Cas de base
6. = double (double 10)
7. = double 20
8. = 40
Vérification : 40
```

### 🚀 Points Importants à Retenir
1. **Fonction d'ordre supérieur** : Prend une fonction comme paramètre, très puissant
2. **applyTwice** : Cas spécial simple pour appliquer deux fois
3. **applyN** : Généralisation flexible pour n applications
4. **Récursion élégante** : applyN utilise la récursion pour compter les applications
5. **Applications pratiques** : Simulations, itérations, calculs répétitifs

### 🧠 Explication Détaillée - Récursion dans applyN

La fonction `applyN` utilise la récursion de manière élégante :

```haskell
-- Définition de applyN
applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x                    -- Cas de base
applyN n f x = f (applyN (n-1) f x) -- Cas récursif

-- Exemple : applyN 3 double 5
-- Step 1: applyN 3 double 5
-- Step 2: double (applyN 2 double 5)
-- Step 3: double (double (applyN 1 double 5))
-- Step 4: double (double (double (applyN 0 double 5)))
-- Step 5: double (double (double 5))    -- Cas de base
-- Step 6: double (double 10)
-- Step 7: double 20
-- Step 8: 40
```
