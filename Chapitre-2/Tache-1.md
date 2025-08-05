## ✅ HC2T1 - Vérification des types dans GHCi

### 🎯 Objectif
Maîtriser la **vérification des types** dans GHCi avec la commande `:t` et comprendre le **polymorphisme des types** en Haskell. Découvrir comment Haskell infère les types et utilise les **contraintes de classe** comme `Num a =>` et `Fractional a =>`.

### 📝 Concepts Clés
- **Commande :t** : Permet de vérifier le type d'une expression dans GHCi
- **Polymorphisme numérique** : Les nombres peuvent avoir différents types selon le contexte
- **Contraintes de classe** : `Num a =>`, `Fractional a =>` spécifient les capacités requises
- **Types concrets** : `Int`, `Double`, `String`, `Char`, `Bool` sont des types spécifiques

### 💻 Code Complet

```haskell
-- Main.hs

-- | Démonstration des types de base en Haskell
-- Cette fonction affiche différentes valeurs avec leurs types explicites
demonstrateBasicTypes :: IO ()
demonstrateBasicTypes = do
  putStrLn "=== Types de Base en Haskell ==="
  putStrLn ""
  
  -- Type Int : entier de taille fixe
  putStrLn "1. 42 :: Int"
  putStrLn ("   Valeur: " ++ show (42 :: Int))
  putStrLn "   → Entier de taille fixe (généralement 64 bits)"
  putStrLn ""
  
  -- Type Double : nombre à virgule flottante double précision
  putStrLn "2. 3.14 :: Double"
  putStrLn ("   Valeur: " ++ show (3.14 :: Double))
  putStrLn "   → Nombre flottant double précision (64 bits)"
  putStrLn ""
  
  -- Type String : liste de caractères
  putStrLn "3. \"Haskell\" :: String"
  putStrLn ("   Valeur: " ++ show ("Haskell" :: String))
  putStrLn "   → Chaîne de caractères (équivalent à [Char])"
  putStrLn ""
  
  -- Type Char : caractère unique
  putStrLn "4. 'Z' :: Char"
  putStrLn ("   Valeur: " ++ show ('Z' :: Char))
  putStrLn "   → Caractère unique entre apostrophes"
  putStrLn ""
  
  -- Type Bool : valeur booléenne
  putStrLn "5. True && False :: Bool"
  putStrLn ("   Valeur: " ++ show (True && False :: Bool))
  putStrLn "   → Valeur logique (True ou False)"

-- | Démonstration du polymorphisme numérique
-- Montre comment le même nombre peut avoir différents types
demonstrateNumericPolymorphism :: IO ()
demonstrateNumericPolymorphism = do
  putStrLn ""
  putStrLn "=== Polymorphisme Numérique ==="
  putStrLn ""
  
  -- Le même nombre avec différents types
  let number = 42
  putStrLn $ "Le nombre 42 peut être :"
  putStrLn $ "• Comme Int      : " ++ show (number :: Int)
  putStrLn $ "• Comme Integer  : " ++ show (number :: Integer) 
  putStrLn $ "• Comme Float    : " ++ show (number :: Float)
  putStrLn $ "• Comme Double   : " ++ show (number :: Double)
  putStrLn ""
  
  -- Nombres fractionnaires
  let fractional = 3.14
  putStrLn $ "Le nombre 3.14 peut être :"
  putStrLn $ "• Comme Float    : " ++ show (fractional :: Float)
  putStrLn $ "• Comme Double   : " ++ show (fractional :: Double)
  putStrLn "  (Pas possible comme Int car il a une partie décimale)"

-- | Démonstration des opérations et de l'inférence de type
-- Montre comment Haskell déduit les types selon le contexte
demonstrateTypeInference :: IO ()
demonstrateTypeInference = do
  putStrLn ""
  putStrLn "=== Inférence de Type et Opérations ==="
  putStrLn ""
  
  -- Opérations arithmétiques
  putStrLn "Opérations arithmétiques :"
  putStrLn $ "• 5 + 3 = " ++ show (5 + 3)
  putStrLn "  Type inféré : Num a => a (polymorphe)"
  putStrLn ""
  
  putStrLn $ "• 5.5 + 2.3 = " ++ show (5.5 + 2.3)
  putStrLn "  Type inféré : Fractional a => a"
  putStrLn ""
  
  putStrLn $ "• 10 `div` 3 = " ++ show (10 `div` 3)
  putStrLn "  Type spécifique : Int -> Int -> Int"
  putStrLn ""
  
  -- Opérations logiques
  putStrLn "Opérations logiques :"
  putStrLn $ "• True || False = " ++ show (True || False)
  putStrLn $ "• not True = " ++ show (not True)
  putStrLn $ "• 5 > 3 = " ++ show (5 > 3)
  putStrLn "  Toutes retournent Bool"

-- | Démonstration des listes et de leurs types
-- Montre comment les listes héritent du type de leurs éléments
demonstrateListTypes :: IO ()
demonstrateListTypes = do
  putStrLn ""
  putStrLn "=== Types de Listes ==="
  putStrLn ""
  
  -- Listes de différents types
  let intList = [1, 2, 3, 4, 5] :: [Int]
  let charList = ['H', 'i', '!'] :: [Char]
  let stringList = ["Hello", "World"] :: [String]  
  let boolList = [True, False, True] :: [Bool]
  
  putStrLn $ "• Liste d'Int    : " ++ show intList ++ " :: [Int]"
  putStrLn $ "• Liste de Char  : " ++ show charList ++ " :: [Char]"
  putStrLn $ "• Liste de String: " ++ show stringList ++ " :: [String]"
  putStrLn $ "• Liste de Bool  : " ++ show boolList ++ " :: [Bool]"
  putStrLn ""
  putStrLn "Note : String est équivalent à [Char]"
  putStrLn $ "\"Hi!\" == " ++ show charList ++ " → " ++ show ("Hi!" == charList)

-- | Exemples de contraintes de classe en action
-- Démontre comment les contraintes limitent les types utilisables
demonstrateClassConstraints :: IO ()
demonstrateClassConstraints = do
  putStrLn ""
  putStrLn "=== Contraintes de Classe ==="
  putStrLn ""
  
  putStrLn "1. Contrainte Num a => a :"
  putStrLn "   • Permet +, -, *, abs, signum"
  putStrLn "   • Types acceptés : Int, Integer, Float, Double"
  putStrLn ""
  
  putStrLn "2. Contrainte Fractional a => a :"
  putStrLn "   • Permet /, fromRational"
  putStrLn "   • Types acceptés : Float, Double (pas Int!)"
  putStrLn ""
  
  putStrLn "3. Contrainte Ord a => a :"
  putStrLn "   • Permet <, <=, >, >=, compare"
  putStrLn "   • Types acceptés : Int, Char, String, Bool, etc."
  putStrLn ""
  
  putStrLn "4. Contrainte Show a => a :"
  putStrLn "   • Permet show (conversion en String)"
  putStrLn "   • La plupart des types de base l'implémentent"

-- | Fonction principale qui orchestre toutes les démonstrations
main :: IO ()
main = do
  putStrLn "=== VÉRIFICATION DES TYPES DANS GHCi ==="
  putStrLn ""
  
  -- Démonstrations successives
  demonstrateBasicTypes
  demonstrateNumericPolymorphism
  demonstrateTypeInference
  demonstrateListTypes
  demonstrateClassConstraints
  
  putStrLn ""
  putStrLn "=== Instructions pour GHCi ==="
  putStrLn "Lancez GHCi et utilisez :t pour vérifier les types :"
  putStrLn "• :t 42"
  putStrLn "• :t 3.14"
  putStrLn "• :t \"Hello\""
  putStrLn "• :t 'A'"
  putStrLn "• :t True"
  putStrLn "• :t [1,2,3]"
  putStrLn "• :t (5 > 3)"
```

### 🔍 Instructions GHCi

```ghci
-- Dans GHCi, tapez ces commandes pour vérifier les types :

-- Types de base
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

-- Types avec annotations explicites
:t (42 :: Int)
-- (42 :: Int) :: Int

:t (3.14 :: Double)
-- (3.14 :: Double) :: Double

-- Types de fonctions
:t (+)
-- (+) :: Num a => a -> a -> a

:t (>)
-- (>) :: Ord a => a -> a -> Bool

:t show
-- show :: Show a => a -> String

-- Types de listes
:t [1,2,3]
-- [1,2,3] :: Num a => [a]

:t "Hello"
-- "Hello" :: String

:t ['a','b','c']
-- ['a','b','c'] :: [Char]

-- Vérification d'expressions complexes
:t (length [1,2,3])
-- (length [1,2,3]) :: Int

:t (head "Hello")
-- (head "Hello") :: Char

:t (take 3 [1..10])
-- (take 3 [1..10]) :: (Num a, Enum a) => [a]
```

### 📊 Sortie Attendue
```
=== VÉRIFICATION DES TYPES DANS GHCi ===

=== Types de Base en Haskell ===

1. 42 :: Int
   Valeur: 42
   → Entier de taille fixe (généralement 64 bits)

2. 3.14 :: Double
   Valeur: 3.14
   → Nombre flottant double précision (64 bits)

3. "Haskell" :: String
   Valeur: "Haskell"
   → Chaîne de caractères (équivalent à [Char])

4. 'Z' :: Char
   Valeur: 'Z'
   → Caractère unique entre apostrophes

5. True && False :: Bool
   Valeur: False
   → Valeur logique (True ou False)

=== Polymorphisme Numérique ===

Le nombre 42 peut être :
• Comme Int      : 42
• Comme Integer  : 42
• Comme Float    : 42.0
• Comme Double   : 42.0

Le nombre 3.14 peut être :
• Comme Float    : 3.14
• Comme Double   : 3.14
  (Pas possible comme Int car il a une partie décimale)

=== Inférence de Type et Opérations ===

Opérations arithmétiques :
• 5 + 3 = 8
  Type inféré : Num a => a (polymorphe)

• 5.5 + 2.3 = 7.8
  Type inféré : Fractional a => a

• 10 `div` 3 = 3
  Type spécifique : Int -> Int -> Int

Opérations logiques :
• True || False = True
• not True = False
• 5 > 3 = True
  Toutes retournent Bool

=== Types de Listes ===

• Liste d'Int    : [1,2,3,4,5] :: [Int]
• Liste de Char  : "Hi!" :: [Char]
• Liste de String: ["Hello","World"] :: [String]
• Liste de Bool  : [True,False,True] :: [Bool]

Note : String est équivalent à [Char]
"Hi!" == "Hi!" → True
```

### 🚀 Points Importants à Retenir
1. **:t dans GHCi** : Commande essentielle pour vérifier les types d'expressions
2. **Polymorphisme** : Les nombres sont polymorphes par défaut (`Num a => a`)
3. **Contraintes de classe** : Spécifient les opérations disponibles pour un type
4. **Inférence de type** : Haskell déduit automatiquement les types la plupart du temps
5. **Annotations explicites** : `:: Type` force un type spécifique quand nécessaire

### 🧠 Explication Détaillée - Contraintes de Classe

Les contraintes de classe comme `Num a =>` sont comme des "contrats" que doit respecter le type :

```haskell
-- Contrainte Num a => signifie :
-- "Le type 'a' doit implémenter la classe Num"
-- Donc 'a' peut être Int, Integer, Float, Double, etc.

42 :: Num a => a
-- Peut devenir : 42 :: Int, 42 :: Double, etc.

-- Contrainte Fractional a => est plus restrictive :
3.14 :: Fractional a => a  
-- Peut seulement devenir : 3.14 :: Float, 3.14 :: Double
-- PAS 3.14 :: Int (Int n'implémente pas Fractional)

-- Plusieurs contraintes peuvent être combinées :
sqrt :: Floating a => a -> a
-- 'a' doit implémenter Floating (qui étend Fractional)
```
