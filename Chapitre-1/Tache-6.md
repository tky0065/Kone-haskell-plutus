## ✅ HC1T6 - Signature de Type (Addition)

### 🎯 Objectif
Comprendre les **signatures de type** en Haskell à travers l'exemple de l'addition. Maîtriser la **currification**, l'**application partielle** et les **contraintes de classe de type** avec `Num a =>` pour créer des fonctions flexibles et réutilisables.

### 📝 Concepts Clés
- **Signature de type** : Déclaration explicite des types d'entrée et de sortie d'une fonction
- **Currification** : Transformation automatique des fonctions multi-paramètres en fonctions à un paramètre
- **Application partielle** : Fournir moins d'arguments que nécessaire pour créer une nouvelle fonction
- **Contrainte de classe** : `Num a =>` permet d'utiliser n'importe quel type numérique

### 💻 Code Complet

```haskell
-- Main.hs

-- | Fonction d'addition avec signature de type explicite
-- Int -> Int -> Int signifie : prend un Int, puis un Int, retourne un Int
-- En réalité, c'est Int -> (Int -> Int) grâce à la currification
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- | Version avec application partielle
-- addFive est créée en appliquant partiellement addNumbers avec la valeur 5
-- Type résultant : Int -> Int (il manque un argument)
addFive :: Int -> Int
addFive = addNumbers 5

-- | Version plus générale avec contrainte de classe
-- Num a => a -> a -> a permet d'utiliser avec Int, Float, Double, Integer, etc.
-- La contrainte Num garantit que le type 'a' supporte les opérations arithmétiques
addGeneric :: Num a => a -> a -> a
addGeneric x y = x + y

-- | Fonction de soustraction pour comparaison
-- Même principe que l'addition mais avec l'opérateur -
subtractNumbers :: Int -> Int -> Int
subtractNumbers x y = x - y

-- | Application partielle pour soustraire 3
subtractThree :: Int -> Int
subtractThree = flip subtractNumbers 3  -- flip inverse l'ordre des arguments

-- | Fonction de multiplication générique
-- Démontre une autre opération avec contrainte Num
multiplyGeneric :: Num a => a -> a -> a
multiplyGeneric x y = x * y

-- | Création d'une fonction "doubler" par application partielle
doubleNumber :: Num a => a -> a
doubleNumber = multiplyGeneric 2

-- | Exemples de calculs pour les tests
testCalculations :: [(String, Int)]
testCalculations = [
    ("addNumbers 3 4", addNumbers 3 4),
    ("addFive 10", addFive 10),
    ("addFive 7", addFive 7),
    ("subtractNumbers 10 3", subtractNumbers 10 3),
    ("subtractThree 10", subtractThree 10)
  ]

-- | Exemples avec différents types numériques
numericExamples :: [String]
numericExamples = [
    "Int: " ++ show (addGeneric (3 :: Int) (4 :: Int)),
    "Float: " ++ show (addGeneric (3.5 :: Float) (2.7 :: Float)),
    "Double: " ++ show (addGeneric (3.14159 :: Double) (2.71828 :: Double)),
    "Integer: " ++ show (addGeneric (123456789 :: Integer) (987654321 :: Integer))
  ]

-- | Fonction principale pour démontrer les signatures de type
main :: IO ()
main = do
  putStrLn "=== SIGNATURES DE TYPE ET ADDITION EN HASKELL ==="
  putStrLn ""
  
  -- Démonstration de l'addition de base
  let a = 3
  let b = 4
  putStrLn $ "=== Addition de base avec signature Int -> Int -> Int ==="
  putStrLn $ "addNumbers " ++ show a ++ " " ++ show b ++ " = " ++ show (addNumbers a b)
  putStrLn $ "• Signature : addNumbers :: Int -> Int -> Int"
  putStrLn $ "• Signification : prend deux Int, retourne un Int"
  putStrLn ""
  
  -- Démonstration de l'application partielle
  putStrLn "=== Application Partielle ==="
  putStrLn "L'application partielle permet de créer de nouvelles fonctions"
  putStrLn ""
  putStrLn "addFive = addNumbers 5  -- Application partielle avec 5"
  putStrLn "• addFive a maintenant le type : Int -> Int"
  putStrLn "• Il lui manque un argument pour être complète"
  putStrLn ""
  
  mapM_ (\val -> do
    putStrLn $ "addFive " ++ show val ++ " = " ++ show (addFive val)
    putStrLn $ "  → équivalent à addNumbers 5 " ++ show val
    ) [10, 7, 15, (-3)]
  putStrLn ""
  
  -- Démonstration des contraintes de classe
  putStrLn "=== Contrainte de Classe Num a => ==="
  putStrLn "addGeneric :: Num a => a -> a -> a"
  putStrLn "• 'a' peut être n'importe quel type qui implémente la classe Num"
  putStrLn "• Fonctionne avec Int, Float, Double, Integer, etc."
  putStrLn ""
  
  mapM_ putStrLn numericExamples
  putStrLn ""
  
  -- Démonstration de la soustraction et flip
  putStrLn "=== Autres Opérations Arithmétiques ==="
  putStrLn ""
  putStrLn "subtractNumbers :: Int -> Int -> Int"
  putStrLn $ "subtractNumbers 10 3 = " ++ show (subtractNumbers 10 3)
  putStrLn "  → 10 - 3 = 7"
  putStrLn ""
  putStrLn "subtractThree = flip subtractNumbers 3"
  putStrLn "• flip inverse l'ordre des arguments"
  putStrLn "• subtractThree x = subtractNumbers x 3 = x - 3"
  putStrLn $ "subtractThree 10 = " ++ show (subtractThree 10)
  putStrLn ""
  
  -- Démonstration de la multiplication
  putStrLn "=== Multiplication et Application Partielle ==="
  putStrLn ""
  putStrLn "multiplyGeneric :: Num a => a -> a -> a"
  putStrLn "doubleNumber = multiplyGeneric 2"
  putStrLn ""
  
  let testValues = [5, 7, 10, (-4)]
  mapM_ (\val -> do
    putStrLn $ "doubleNumber " ++ show val ++ " = " ++ show (doubleNumber val)
    ) testValues
  putStrLn ""
  
  -- Récapitulatif des calculs
  putStrLn "=== Récapitulatif des Calculs ==="
  mapM_ (\(description, result) -> do
    putStrLn $ "• " ++ description ++ " = " ++ show result
    ) testCalculations
  putStrLn ""
  
  -- Démonstration de la currification
  putStrLn "=== Explication de la Currification ==="
  putStrLn "En Haskell, toutes les fonctions sont automatiquement 'curryfiées'"
  putStrLn ""
  putStrLn "addNumbers :: Int -> Int -> Int"
  putStrLn "est en réalité : addNumbers :: Int -> (Int -> Int)"
  putStrLn ""
  putStrLn "Cela signifie :"
  putStrLn "• addNumbers prend un Int"
  putStrLn "• et retourne une fonction (Int -> Int)"
  putStrLn "• Cette fonction retournée prend le deuxième Int"
  putStrLn "• et produit le résultat final Int"
  putStrLn ""
  putStrLn "C'est pourquoi l'application partielle fonctionne naturellement !"
```

### 🔍 Instructions GHCi

```ghci
-- Dans GHCi, tapez ces commandes pour explorer les signatures de type :

-- Vérification des types de nos fonctions
:t addNumbers
-- addNumbers :: Int -> Int -> Int

:t addFive
-- addFive :: Int -> Int

:t addGeneric
-- addGeneric :: Num a => a -> a -> a

-- Tests d'addition
addNumbers 3 4
-- 7

addFive 10
-- 15

-- Application partielle en action
let add10 = addNumbers 10
:t add10
-- add10 :: Int -> Int

add10 5
-- 15

-- Tests avec différents types numériques
addGeneric (3.5 :: Float) (2.7 :: Float)
-- 6.2

addGeneric (100 :: Integer) (200 :: Integer)
-- 300

-- Création de fonctions spécialisées
let multiply3 = multiplyGeneric 3
multiply3 7
-- 21

-- Vérification que flip fonctionne
let subtract5 = flip subtractNumbers 5
subtract5 12
-- 7
```

### 📊 Sortie Attendue
```
=== SIGNATURES DE TYPE ET ADDITION EN HASKELL ===

=== Addition de base avec signature Int -> Int -> Int ===
addNumbers 3 4 = 7
• Signature : addNumbers :: Int -> Int -> Int
• Signification : prend deux Int, retourne un Int

=== Application Partielle ===
L'application partielle permet de créer de nouvelles fonctions

addFive = addNumbers 5  -- Application partielle avec 5
• addFive a maintenant le type : Int -> Int
• Il lui manque un argument pour être complète

addFive 10 = 15
  → équivalent à addNumbers 5 10
addFive 7 = 12
  → équivalent à addNumbers 5 7
addFive 15 = 20
  → équivalent à addNumbers 5 15
addFive (-3) = 2
  → équivalent à addNumbers 5 (-3)

=== Contrainte de Classe Num a => ===
addGeneric :: Num a => a -> a -> a
• 'a' peut être n'importe quel type qui implémente la classe Num
• Fonctionne avec Int, Float, Double, Integer, etc.

Int: 7
Float: 6.2
Double: 5.85987
Integer: 1111111110

=== Autres Opérations Arithmétiques ===

subtractNumbers :: Int -> Int -> Int
subtractNumbers 10 3 = 7
  → 10 - 3 = 7

subtractThree = flip subtractNumbers 3
• flip inverse l'ordre des arguments
• subtractThree x = subtractNumbers x 3 = x - 3
subtractThree 10 = 7

=== Multiplication et Application Partielle ===

multiplyGeneric :: Num a => a -> a -> a
doubleNumber = multiplyGeneric 2

doubleNumber 5 = 10
doubleNumber 7 = 14
doubleNumber 10 = 20
doubleNumber (-4) = -8

=== Récapitulatif des Calculs ===
• addNumbers 3 4 = 7
• addFive 10 = 15
• addFive 7 = 12
• subtractNumbers 10 3 = 7
• subtractThree 10 = 7

=== Explication de la Currification ===
En Haskell, toutes les fonctions sont automatiquement 'curryfiées'

addNumbers :: Int -> Int -> Int
est en réalité : addNumbers :: Int -> (Int -> Int)

Cela signifie :
• addNumbers prend un Int
• et retourne une fonction (Int -> Int)
• Cette fonction retournée prend le deuxième Int
• et produit le résultat final Int

C'est pourquoi l'application partielle fonctionne naturellement !
```

### 🚀 Points Importants à Retenir
1. **Signature explicite** : Déclarer `:: Int -> Int -> Int` rend le code plus clair et détecte les erreurs
2. **Currification automatique** : Toutes les fonctions multi-paramètres sont curryfiées en Haskell
3. **Application partielle** : Fournir moins d'arguments crée une nouvelle fonction spécialisée
4. **Contraintes de classe** : `Num a =>` permet la généricité sur les types numériques
5. **Flexibilité** : Une fonction peut marcher avec Int, Float, Double, Integer selon le contexte

### 🧠 Explication Détaillée - Currification

La currification transforme une fonction à plusieurs arguments en une chaîne de fonctions à un argument :

```haskell
-- Ce que nous écrivons
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- Ce que Haskell comprend (avec parenthèses explicites)
addNumbers :: Int -> (Int -> Int)
addNumbers = \x -> (\y -> x + y)

-- Donc :
addNumbers 5     -- retourne une fonction (\y -> 5 + y)
addNumbers 5 3   -- applique cette fonction à 3, donne 8
```
