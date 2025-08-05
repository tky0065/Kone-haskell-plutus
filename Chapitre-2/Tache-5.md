## ✅ HC2T5 - Définir et utiliser des fonctions

### 🎯 Objectif
Apprendre à **définir et utiliser des fonctions** en Haskell avec des signatures de type explicites. Maîtriser les **gardes (guards)** avec `|` et comprendre les différentes façons d'implémenter la même logique.

### 📝 Concepts Clés
- **Définition de fonction** : `nom :: Type -> Type -> Type` puis `nom param1 param2 = expression`
- **Gardes (guards)** : Utilisation de `|` pour des conditions multiples
- **Pattern matching** : Déconstruction des arguments selon leur forme
- **Fonctions pures** : Même entrée produit toujours la même sortie

### 💻 Code Complet

```haskell
-- Main.hs

-- | Fonction circleArea qui prend un rayon (Float) et retourne l'aire du cercle
-- Formule mathématique : A = π × r²
-- Signature : Float -> Float (un Float en entrée, un Float en sortie)
circleArea :: Float -> Float
circleArea r = pi * r * r

-- | Fonction maxOfThree qui prend trois Int et retourne la valeur maximale
-- Utilise la fonction max de façon imbriquée : max x (max y z)
-- Signature : Int -> Int -> Int -> Int (trois Int en entrée, un Int en sortie)
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree x y z = max x (max y z)

-- | Version alternative de maxOfThree avec des gardes (conditions)
-- Les gardes utilisent | pour tester différentes conditions
-- 'otherwise' est équivalent à True (cas par défaut)
maxOfThreeAlt :: Int -> Int -> Int -> Int
maxOfThreeAlt x y z
  | x >= y && x >= z = x    -- Si x est le plus grand
  | y >= z = y              -- Sinon, si y >= z, alors y est le plus grand
  | otherwise = z           -- Sinon, z est le plus grand

-- | Fonction qui calcule le périmètre d'un cercle
-- Formule : P = 2 × π × r
-- Démontre une autre fonction mathématique simple
circlePerimeter :: Float -> Float
circlePerimeter r = 2 * pi * r

-- | Fonction qui calcule l'aire d'un rectangle
-- Formule : A = largeur × hauteur
-- Signature : Float -> Float -> Float (deux paramètres)
rectangleArea :: Float -> Float -> Float
rectangleArea width height = width * height

-- | Fonction qui détermine si un nombre est pair
-- Utilise l'opérateur modulo (mod) pour tester la divisibilité par 2
isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

-- | Fonction qui détermine si un nombre est impair
-- Utilise la négation de isEven pour éviter la duplication
isOdd :: Int -> Bool
isOdd n = not (isEven n)

-- | Fonction avec gardes multiples - évalue une note
-- Démontre l'utilisation de plusieurs conditions avec |
gradeEvaluation :: Int -> String
gradeEvaluation score
  | score >= 90 = "Excellent (A)"
  | score >= 80 = "Très bien (B)" 
  | score >= 70 = "Bien (C)"
  | score >= 60 = "Passable (D)"
  | otherwise = "Échec (F)"

-- | Fonction qui calcule la valeur absolue d'un nombre
-- Version personnalisée de la fonction abs
-- Utilise des gardes pour tester le signe
absoluteValue :: Int -> Int
absoluteValue n
  | n >= 0 = n      -- Si positif ou nul, retourne tel quel
  | otherwise = -n  -- Si négatif, retourne l'opposé

-- | Fonction qui calcule le factoriel d'un nombre
-- Utilise la récursion : n! = n × (n-1)!
-- Cas de base : 0! = 1
factorial :: Int -> Int
factorial n
  | n <= 0 = 1                    -- Cas de base
  | otherwise = n * factorial (n-1)  -- Récursion

-- | Fonction qui calcule la puissance d'un nombre
-- power base exponent = base^exponent
-- Utilise la récursion pour l'exponentiation
power :: Int -> Int -> Int
power base exponent
  | exponent == 0 = 1                           -- Cas de base : n^0 = 1
  | exponent > 0 = base * power base (exponent - 1)  -- Récursion positive
  | otherwise = error "Exposant négatif non supporté"  -- Gestion d'erreur

-- | Fonction qui trouve le minimum de trois nombres
-- Équivalent de maxOfThree mais pour le minimum
minOfThree :: Int -> Int -> Int -> Int
minOfThree x y z = min x (min y z)

-- | Version alternative avec gardes pour minOfThree
minOfThreeAlt :: Int -> Int -> Int -> Int
minOfThreeAlt x y z
  | x <= y && x <= z = x    -- Si x est le plus petit
  | y <= z = y              -- Sinon, si y <= z, alors y est le plus petit
  | otherwise = z           -- Sinon, z est le plus petit

-- | Fonction qui classifie un triangle selon ses côtés
-- Prend trois côtés et détermine le type de triangle
triangleType :: Float -> Float -> Float -> String
triangleType a b c
  | a <= 0 || b <= 0 || c <= 0 = "Côtés invalides"
  | a + b <= c || a + c <= b || b + c <= a = "Triangle impossible"
  | a == b && b == c = "Triangle équilatéral"
  | a == b || b == c || a == c = "Triangle isocèle"
  | otherwise = "Triangle scalène"

-- | Exemples de données pour les tests
testRadii :: [Float]
testRadii = [1.0, 2.5, 5.0, 10.0]

testNumbers :: [Int]
testNumbers = [5, 12, 8, 3, 15, 7, 20]

testScores :: [Int]
testScores = [95, 85, 75, 65, 55, 45]

-- | Fonction principale pour démontrer toutes les fonctions définies
main :: IO ()
main = do
  putStrLn "=== DÉFINIR ET UTILISER DES FONCTIONS EN HASKELL ==="
  putStrLn ""
  
  -- =========================================================================
  -- SECTION 1: FONCTIONS MATHÉMATIQUES SIMPLES
  -- =========================================================================
  putStrLn "=== FONCTIONS MATHÉMATIQUES SIMPLES ==="
  putStrLn ""
  
  putStrLn "🔵 Calculs d'aire et périmètre de cercles :"
  mapM_ (\r -> do
    let area = circleArea r
    let perimeter = circlePerimeter r
    putStrLn $ "• Rayon " ++ show r ++ " : Aire = " ++ show (round (area * 100) / 100) ++ 
               ", Périmètre = " ++ show (round (perimeter * 100) / 100)
    ) testRadii
  putStrLn ""
  
  putStrLn "🔲 Calcul d'aire de rectangles :"
  let rectangles = [(3.0, 4.0), (5.5, 2.0), (10.0, 7.5)]
  mapM_ (\(w, h) -> do
    let area = rectangleArea w h
    putStrLn $ "• Rectangle " ++ show w ++ "×" ++ show h ++ " : Aire = " ++ show area
    ) rectangles
  putStrLn ""
  
  -- =========================================================================
  -- SECTION 2: FONCTIONS AVEC GARDES
  -- =========================================================================
  putStrLn "=== FONCTIONS AVEC GARDES (CONDITIONS) ==="
  putStrLn ""
  
  putStrLn "🔢 Tests pair/impair :"
  mapM_ (\n -> do
    putStrLn $ "• " ++ show n ++ " est " ++ 
               (if isEven n then "pair" else "impair")
    ) (take 6 testNumbers)
  putStrLn ""
  
  putStrLn "📊 Évaluation de notes :"
  mapM_ (\score -> do
    putStrLn $ "• Score " ++ show score ++ " : " ++ gradeEvaluation score
    ) testScores
  putStrLn ""
  
  putStrLn "📏 Valeur absolue :"
  let negativeNumbers = [-5, -12, 0, 8, -3]
  mapM_ (\n -> do
    putStrLn $ "• |" ++ show n ++ "| = " ++ show (absoluteValue n)
    ) negativeNumbers
  putStrLn ""
  
  -- =========================================================================
  -- SECTION 3: COMPARAISON DES APPROCHES (max avec fonction vs gardes)
  -- =========================================================================
  putStrLn "=== COMPARAISON DES APPROCHES - Maximum de trois nombres ==="
  putStrLn ""
  
  let triples = [(5, 12, 8), (20, 15, 25), (7, 7, 3), (10, 10, 10)]
  putStrLn "Comparaison maxOfThree vs maxOfThreeAlt :"
  mapM_ (\(x, y, z) -> do
    let result1 = maxOfThree x y z
    let result2 = maxOfThreeAlt x y z
    putStrLn $ "• max(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ") = " ++ 
               show result1 ++ " (fonction) = " ++ show result2 ++ " (gardes)"
    ) triples
  putStrLn ""
  
  -- =========================================================================
  -- SECTION 4: FONCTIONS RÉCURSIVES
  -- =========================================================================
  putStrLn "=== FONCTIONS RÉCURSIVES ==="
  putStrLn ""
  
  putStrLn "🔢 Calcul de factorielles :"
  let factorialInputs = [0, 1, 3, 5, 6]
  mapM_ (\n -> do
    putStrLn $ "• " ++ show n ++ "! = " ++ show (factorial n)
    ) factorialInputs
  putStrLn ""
  
  putStrLn "⚡ Calcul de puissances :"
  let powerInputs = [(2, 3), (3, 4), (5, 2), (10, 0)]
  mapM_ (\(base, exp) -> do
    putStrLn $ "• " ++ show base ++ "^" ++ show exp ++ " = " ++ show (power base exp)
    ) powerInputs
  putStrLn ""
  
  -- =========================================================================
  -- SECTION 5: FONCTIONS AVEC LOGIQUE COMPLEXE
  -- =========================================================================
  putStrLn "=== FONCTIONS AVEC LOGIQUE COMPLEXE ==="
  putStrLn ""
  
  putStrLn "🔺 Classification de triangles :"
  let triangles = [(3, 4, 5), (5, 5, 5), (3, 3, 5), (2, 3, 4), (1, 1, 3)]
  mapM_ (\(a, b, c) -> do
    putStrLn $ "• Triangle (" ++ show a ++ "," ++ show b ++ "," ++ show c ++ ") : " ++ 
               triangleType a b c
    ) triangles
  putStrLn ""
  
  -- =========================================================================
  -- SECTION 6: ANALYSE DES SIGNATURES DE FONCTIONS
  -- =========================================================================
  putStrLn "=== ANALYSE DES SIGNATURES DE FONCTIONS ==="
  putStrLn ""
  putStrLn "📋 Récapitulatif des signatures :"
  putStrLn "• circleArea :: Float -> Float"
  putStrLn "  → Une entrée (rayon), une sortie (aire)"
  putStrLn ""
  putStrLn "• rectangleArea :: Float -> Float -> Float"
  putStrLn "  → Deux entrées (largeur, hauteur), une sortie (aire)"
  putStrLn ""
  putStrLn "• maxOfThree :: Int -> Int -> Int -> Int"
  putStrLn "  → Trois entrées (nombres), une sortie (maximum)"
  putStrLn ""
  putStrLn "• gradeEvaluation :: Int -> String"
  putStrLn "  → Une entrée (score), une sortie (évaluation textuelle)"
  putStrLn ""
  putStrLn "• factorial :: Int -> Int"
  putStrLn "  → Une entrée (nombre), une sortie (factorielle)"
  putStrLn ""
  
  -- =========================================================================
  -- SECTION 7: AVANTAGES DES FONCTIONS EN HASKELL
  -- =========================================================================
  putStrLn "=== AVANTAGES DES FONCTIONS EN HASKELL ==="
  putStrLn ""
  putStrLn "1. 🔒 PURETÉ :"
  putStrLn "   • Même entrée → même sortie, toujours"
  putStrLn "   • Pas d'effets de bord cachés"
  putStrLn ""
  putStrLn "2. 📝 SIGNATURES EXPRESSIVES :"
  putStrLn "   • Le type documente le comportement"
  putStrLn "   • Détection d'erreurs à la compilation"
  putStrLn ""
  putStrLn "3. 🎯 GARDES LISIBLES :"
  putStrLn "   • Conditions multiples claires"
  putStrLn "   • Plus expressif que if/else imbriqués"
  putStrLn ""
  putStrLn "4. 🔄 RÉCURSION NATURELLE :"
  putStrLn "   • Définitions mathématiques directes"
  putStrLn "   • Élégance et concision"
  putStrLn ""
  
  putStrLn "✅ Les fonctions en Haskell sont pures, expressives et puissantes !"
```

### 🔍 Instructions GHCi

```ghci
-- Dans GHCi, testez vos fonctions définies :

-- Fonctions mathématiques
circleArea 5.0
-- 78.53982

rectangleArea 4.0 6.0
-- 24.0

-- Tests avec différentes approches
maxOfThree 5 12 8
-- 12

maxOfThreeAlt 5 12 8  
-- 12

-- Vérification que les deux donnent le même résultat
maxOfThree 10 7 15 == maxOfThreeAlt 10 7 15
-- True

-- Fonctions avec gardes
gradeEvaluation 85
-- "Très bien (B)"

gradeEvaluation 55
-- "Échec (F)"

-- Tests pair/impair
isEven 4
-- True

isOdd 7
-- True

-- Fonctions récursives
factorial 5
-- 120

power 2 8
-- 256

-- Classification de triangles
triangleType 3 4 5
-- "Triangle scalène"

triangleType 5 5 5
-- "Triangle équilatéral"

-- Vérification des types
:t circleArea
-- circleArea :: Float -> Float

:t maxOfThree
-- maxOfThree :: Int -> Int -> Int -> Int

:t gradeEvaluation
-- gradeEvaluation :: Int -> String
```

### 📊 Sortie Attendue
```
=== DÉFINIR ET UTILISER DES FONCTIONS EN HASKELL ===

=== FONCTIONS MATHÉMATIQUES SIMPLES ===

🔵 Calculs d'aire et périmètre de cercles :
• Rayon 1.0 : Aire = 3.14, Périmètre = 6.28
• Rayon 2.5 : Aire = 19.63, Périmètre = 15.71
• Rayon 5.0 : Aire = 78.54, Périmètre = 31.42
• Rayon 10.0 : Aire = 314.16, Périmètre = 62.83

🔲 Calcul d'aire de rectangles :
• Rectangle 3.0×4.0 : Aire = 12.0
• Rectangle 5.5×2.0 : Aire = 11.0
• Rectangle 10.0×7.5 : Aire = 75.0

=== FONCTIONS AVEC GARDES (CONDITIONS) ===

🔢 Tests pair/impair :
• 5 est impair
• 12 est pair
• 8 est pair
• 3 est impair
• 15 est impair
• 7 est impair

📊 Évaluation de notes :
• Score 95 : Excellent (A)
• Score 85 : Très bien (B)
• Score 75 : Bien (C)
• Score 65 : Passable (D)
• Score 55 : Échec (F)
• Score 45 : Échec (F)

📏 Valeur absolue :
• |-5| = 5
• |-12| = 12
• |0| = 0
• |8| = 8
• |-3| = 3

=== COMPARAISON DES APPROCHES - Maximum de trois nombres ===

Comparaison maxOfThree vs maxOfThreeAlt :
• max(5,12,8) = 12 (fonction) = 12 (gardes)
• max(20,15,25) = 25 (fonction) = 25 (gardes)
• max(7,7,3) = 7 (fonction) = 7 (gardes)
• max(10,10,10) = 10 (fonction) = 10 (gardes)

=== FONCTIONS RÉCURSIVES ===

🔢 Calcul de factorielles :
• 0! = 1
• 1! = 1
• 3! = 6
• 5! = 120
• 6! = 720

⚡ Calcul de puissances :
• 2^3 = 8
• 3^4 = 81
• 5^2 = 25
• 10^0 = 1

=== FONCTIONS AVEC LOGIQUE COMPLEXE ===

🔺 Classification de triangles :
• Triangle (3.0,4.0,5.0) : Triangle scalène
• Triangle (5.0,5.0,5.0) : Triangle équilatéral
• Triangle (3.0,3.0,5.0) : Triangle isocèle
• Triangle (2.0,3.0,4.0) : Triangle scalène
• Triangle (1.0,1.0,3.0) : Triangle impossible

✅ Les fonctions en Haskell sont pures, expressives et puissantes !
```

### 🚀 Points Importants à Retenir
1. **Signature explicite** : Toujours déclarer `:: Type` avant la définition
2. **Gardes avec |** : Plus lisibles que des if/else imbriqués pour les conditions multiples
3. **otherwise** : Équivaut à `True`, utilisé comme cas par défaut
4. **Récursion naturelle** : Haskell excelle dans les définitions récursives
5. **Pureté des fonctions** : Même entrée produit toujours la même sortie

### 🧠 Explication Détaillée - Gardes vs If/Else

Les gardes offrent une syntaxe plus claire que les if/else imbriqués :

```haskell
-- AVEC GARDES (recommandé)
gradeEvaluation score
  | score >= 90 = "Excellent"
  | score >= 80 = "Très bien"
  | score >= 70 = "Bien"
  | score >= 60 = "Passable"
  | otherwise = "Échec"

-- AVEC IF/ELSE (plus verbeux)
gradeEvaluationIfElse score = 
  if score >= 90 
    then "Excellent"
    else if score >= 80
      then "Très bien"
      else if score >= 70
        then "Bien"
        else if score >= 60
          then "Passable"
          else "Échec"

-- Les gardes sont plus lisibles et expressives !
```
