## ✅ HC2T2 - Signatures de fonctions

### 🎯 Objectif
Comprendre et maîtriser les **signatures de fonctions** en Haskell. Apprendre à déclarer explicitement les types d'entrée et de sortie des fonctions pour améliorer la lisibilité du code et détecter les erreurs de type.

### 📝 Concepts Clés
- **Signature de fonction** : Déclaration explicite des types avec `::` 
- **Types d'entrée et sortie** : Spécifier ce que la fonction accepte et retourne
- **Lisibilité du code** : Les signatures documentent le comportement des fonctions
- **Détection d'erreurs** : Le compilateur vérifie la cohérence entre signature et implémentation

### 💻 Code Complet

```haskell
-- Main.hs

-- | Fonction add qui prend deux Int et retourne leur somme
-- Signature : Int -> Int -> Int signifie "deux Int en entrée, un Int en sortie"
-- Cette fonction démontre l'arithmétique de base avec types explicites
add :: Int -> Int -> Int
add x y = x + y

-- | Fonction isEven qui prend un Int et retourne un Bool
-- Signature : Int -> Bool signifie "un Int en entrée, un Bool en sortie"  
-- Utilise l'opérateur modulo pour tester la parité
isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

-- | Fonction concatStrings qui prend deux String et retourne leur concaténation
-- Signature : String -> String -> String 
-- Utilise l'opérateur ++ pour joindre deux chaînes
concatStrings :: String -> String -> String
concatStrings str1 str2 = str1 ++ str2

-- | Fonction multiply qui prend deux nombres et retourne leur produit
-- Démontre une signature similaire à add mais avec multiplication
multiply :: Int -> Int -> Int
multiply x y = x * y

-- | Fonction isPositive qui teste si un nombre est positif
-- Signature : Int -> Bool comme isEven mais teste une condition différente
isPositive :: Int -> Bool
isPositive n = n > 0

-- | Fonction getLength qui retourne la longueur d'une String
-- Signature : String -> Int montre conversion String vers Int
getLength :: String -> Int
getLength str = length str

-- | Fonction isLongString qui teste si une chaîne a plus de 5 caractères
-- Combine getLength avec une comparaison, retourne Bool
isLongString :: String -> Bool
isLongString str = getLength str > 5

-- | Fonction addThree qui prend trois Int et retourne leur somme
-- Signature : Int -> Int -> Int -> Int montre une fonction à trois paramètres
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- | Fonction formatMessage qui combine deux String avec un format
-- Démontre manipulation de String avec signature explicite
formatMessage :: String -> String -> String
formatMessage name message = "Message pour " ++ name ++ ": " ++ message

-- | Fonction isInRange qui teste si un nombre est dans un intervalle
-- Signature : Int -> Int -> Int -> Bool montre trois entrées, sortie Bool
isInRange :: Int -> Int -> Int -> Bool
isInRange min max value = value >= min && value <= max

-- | Exemples de données pour les tests
testNumbers :: [Int]
testNumbers = [-5, -1, 0, 1, 2, 8, 15, 42]

testStrings :: [String]
testStrings = ["Hi", "Test", "Haskell", "Programming", "a", "Hello World"]

-- | Fonction principale pour démontrer les signatures de fonctions
main :: IO ()
main = do
  putStrLn "=== SIGNATURES DE FONCTIONS EN HASKELL ==="
  putStrLn ""
  
  -- Démonstration des fonctions arithmétiques
  putStrLn "=== Fonctions Arithmétiques ==="
  putStrLn "Ces fonctions ont des signatures Int -> Int -> Int"
  putStrLn ""
  
  let a = 12
  let b = 8
  putStrLn $ "Tests avec a = " ++ show a ++ " et b = " ++ show b ++ " :"
  putStrLn $ "• add " ++ show a ++ " " ++ show b ++ " = " ++ show (add a b)
  putStrLn $ "  → Signature : add :: Int -> Int -> Int"
  putStrLn $ "• multiply " ++ show a ++ " " ++ show b ++ " = " ++ show (multiply a b)
  putStrLn $ "  → Signature : multiply :: Int -> Int -> Int"
  putStrLn ""
  
  -- Démonstration des fonctions de test (prédicats)
  putStrLn "=== Fonctions de Test (Prédicats) ==="
  putStrLn "Ces fonctions ont des signatures Int -> Bool"
  putStrLn ""
  
  mapM_ (\num -> do
    putStrLn $ "Tests avec le nombre " ++ show num ++ " :"
    putStrLn $ "• isEven " ++ show num ++ " = " ++ show (isEven num)
    putStrLn $ "• isPositive " ++ show num ++ " = " ++ show (isPositive num)
    putStrLn ""
    ) (take 4 testNumbers)
  
  -- Démonstration des fonctions sur String
  putStrLn "=== Fonctions sur String ==="
  putStrLn ""
  
  mapM_ (\str -> do
    putStrLn $ "Tests avec \"" ++ str ++ "\" :"
    putStrLn $ "• getLength \"" ++ str ++ "\" = " ++ show (getLength str)
    putStrLn $ "  → Signature : getLength :: String -> Int"
    putStrLn $ "• isLongString \"" ++ str ++ "\" = " ++ show (isLongString str)
    putStrLn $ "  → Signature : isLongString :: String -> Bool"
    putStrLn ""
    ) (take 3 testStrings)
  
  -- Démonstration de concaténation de String
  putStrLn "=== Concaténation de String ==="
  putStrLn "Signature : String -> String -> String"
  putStrLn ""
  
  let str1 = "Hello"
  let str2 = "World"
  putStrLn $ "• concatStrings \"" ++ str1 ++ "\" \"" ++ str2 ++ "\" = \"" ++ concatStrings str1 str2 ++ "\""
  putStrLn ""
  
  let name = "Alice"
  let msg = "Bienvenue en Haskell!"
  putStrLn $ "• formatMessage \"" ++ name ++ "\" \"" ++ msg ++ "\""
  putStrLn $ "  = \"" ++ formatMessage name msg ++ "\""
  putStrLn ""
  
  -- Démonstration de fonction à trois paramètres
  putStrLn "=== Fonction à Trois Paramètres ==="
  putStrLn "Signature : Int -> Int -> Int -> Int"
  putStrLn ""
  
  let x = 5
  let y = 10
  let z = 15
  putStrLn $ "• addThree " ++ show x ++ " " ++ show y ++ " " ++ show z ++ " = " ++ show (addThree x y z)
  putStrLn $ "  → " ++ show x ++ " + " ++ show y ++ " + " ++ show z ++ " = " ++ show (addThree x y z)
  putStrLn ""
  
  -- Démonstration de fonction avec intervalle
  putStrLn "=== Test d'Intervalle ==="
  putStrLn "Signature : Int -> Int -> Int -> Bool"
  putStrLn ""
  
  let minVal = 1
  let maxVal = 10
  let testValues = [0, 5, 10, 15]
  
  putStrLn $ "Test si les valeurs sont dans l'intervalle [" ++ show minVal ++ ", " ++ show maxVal ++ "] :"
  mapM_ (\val -> do
    let result = isInRange minVal maxVal val
    putStrLn $ "• isInRange " ++ show minVal ++ " " ++ show maxVal ++ " " ++ show val ++ " = " ++ show result
    ) testValues
  putStrLn ""
  
  -- Récapitulatif des types de signatures
  putStrLn "=== Récapitulatif des Types de Signatures ==="
  putStrLn ""
  putStrLn "1. Fonction à un paramètre :"
  putStrLn "   • getLength :: String -> Int"
  putStrLn "   • isEven :: Int -> Bool"
  putStrLn ""
  putStrLn "2. Fonction à deux paramètres :"
  putStrLn "   • add :: Int -> Int -> Int"
  putStrLn "   • concatStrings :: String -> String -> String"
  putStrLn ""
  putStrLn "3. Fonction à trois paramètres :"
  putStrLn "   • addThree :: Int -> Int -> Int -> Int"
  putStrLn "   • isInRange :: Int -> Int -> Int -> Bool"
  putStrLn ""
  putStrLn "Pattern général : Type1 -> Type2 -> ... -> TypeRésultat"
```

### 🔍 Instructions GHCi

```ghci
-- Dans GHCi, tapez ces commandes pour explorer les signatures de fonctions :

-- Vérification des signatures de nos fonctions
:t add
-- add :: Int -> Int -> Int

:t isEven
-- isEven :: Int -> Bool

:t concatStrings
-- concatStrings :: String -> String -> String

:t getLength
-- getLength :: String -> Int

-- Tests des fonctions
add 5 3
-- 8

isEven 4
-- True

isEven 7
-- False

concatStrings "Hello" "World"
-- "HelloWorld"

getLength "Haskell"
-- 7

-- Test de signatures avec différents types
:t length
-- length :: [a] -> Int

:t (++)
-- (++) :: [a] -> [a] -> [a]

:t (>)
-- (>) :: Ord a => a -> a -> Bool

-- Vérification que nos fonctions respectent leurs signatures
:t (add 5 3)
-- (add 5 3) :: Int

:t (isEven 4)
-- (isEven 4) :: Bool

:t (concatStrings "Hello" "World")
-- (concatStrings "Hello" "World") :: String
```

### 📊 Sortie Attendue
```
=== SIGNATURES DE FONCTIONS EN HASKELL ===

=== Fonctions Arithmétiques ===
Ces fonctions ont des signatures Int -> Int -> Int

Tests avec a = 12 et b = 8 :
• add 12 8 = 20
  → Signature : add :: Int -> Int -> Int
• multiply 12 8 = 96
  → Signature : multiply :: Int -> Int -> Int

=== Fonctions de Test (Prédicats) ===
Ces fonctions ont des signatures Int -> Bool

Tests avec le nombre -5 :
• isEven -5 = False
• isPositive -5 = False

Tests avec le nombre -1 :
• isEven -1 = False
• isPositive -1 = False

Tests avec le nombre 0 :
• isEven 0 = True
• isPositive 0 = False

Tests avec le nombre 1 :
• isEven 1 = False
• isPositive 1 = True

=== Fonctions sur String ===

Tests avec "Hi" :
• getLength "Hi" = 2
  → Signature : getLength :: String -> Int
• isLongString "Hi" = False
  → Signature : isLongString :: String -> Bool

Tests avec "Test" :
• getLength "Test" = 4
  → Signature : getLength :: String -> Int
• isLongString "Test" = False
  → Signature : isLongString :: String -> Bool

=== Concaténation de String ===
Signature : String -> String -> String

• concatStrings "Hello" "World" = "HelloWorld"

• formatMessage "Alice" "Bienvenue en Haskell!"
  = "Message pour Alice: Bienvenue en Haskell!"

=== Test d'Intervalle ===
Signature : Int -> Int -> Int -> Bool

Test si les valeurs sont dans l'intervalle [1, 10] :
• isInRange 1 10 0 = False
• isInRange 1 10 5 = True
• isInRange 1 10 10 = True
• isInRange 1 10 15 = False
```

### 🚀 Points Importants à Retenir
1. **Signatures explicites** : Toujours déclarer `:: Type` améliore la lisibilité
2. **Pattern Type1 -> Type2 -> TypeRésultat** : Structure standard des signatures
3. **Documentation vivante** : Les signatures servent de documentation du code
4. **Vérification automatique** : Le compilateur vérifie la cohérence type/implémentation
5. **Fonction vs prédicat** : `-> Bool` indique une fonction de test/condition

### 🧠 Explication Détaillée - Lecture des Signatures

Les signatures de fonctions se lisent de gauche à droite avec des associations à droite :

```haskell
-- Signature simple à un paramètre
getLength :: String -> Int
-- "Prend une String, retourne un Int"

-- Signature à deux paramètres
add :: Int -> Int -> Int
-- Équivalent à : Int -> (Int -> Int)
-- "Prend un Int, retourne une fonction (Int -> Int)"
-- Cette fonction retournée prend un Int et retourne un Int

-- Signature à trois paramètres
addThree :: Int -> Int -> Int -> Int
-- Équivalent à : Int -> (Int -> (Int -> Int))
-- "Prend un Int, retourne une fonction qui prend un Int, 
--  qui retourne une fonction qui prend un Int et retourne un Int"

-- C'est pourquoi l'application partielle fonctionne :
let addFive = add 5  -- Type : Int -> Int
addFive 3           -- Résultat : 8
```
