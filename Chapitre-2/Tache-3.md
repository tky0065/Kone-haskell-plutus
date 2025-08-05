## ✅ HC2T3 - Variables immuables

### 🎯 Objectif
Comprendre l'**immutabilité** en Haskell - un concept fondamental où les valeurs ne peuvent jamais être modifiées après leur création. Découvrir pourquoi cette approche améliore la sécurité, la prévisibilité et la facilité de raisonnement du code.

### 📝 Concepts Clés
- **Immutabilité** : Les valeurs ne changent jamais une fois définies
- **Variables vs Constantes** : En Haskell, tout est constant par défaut
- **Sécurité** : Pas de modification accidentelle, pas d'effets de bord surprenants
- **Programmation pure** : Les fonctions produisent toujours le même résultat

### 💻 Code Complet

```haskell
-- Main.hs

-- | Définition des variables immuables
-- Une fois définies, ces valeurs ne peuvent jamais changer

-- Age d'une personne - valeur entière immuable
myAge :: Int
myAge = 25

-- Valeur de π avec précision double - constante mathématique
piValue :: Double
piValue = 3.14159265359

-- Message de salutation - chaîne immuable
greeting :: String
greeting = "Bonjour tout le monde!"

-- État booléen - Haskell est-il amusant ? (spoiler: oui!)
isHaskellFun :: Bool
isHaskellFun = True

-- | Exemples de calculs basés sur des valeurs immuables
-- Ces fonctions utilisent les valeurs définies ci-dessus

-- Calcule l'aire d'un cercle de rayon donné
-- Utilise la constante piValue pour garantir la précision
circleArea :: Double -> Double
circleArea radius = piValue * radius * radius

-- Crée un message personnalisé
-- Combine la salutation avec un nom
personalGreeting :: String -> String
personalGreeting name = greeting ++ " " ++ name ++ "!"

-- Détermine si quelqu'un est majeur
-- Compare avec l'âge de référence
isAdult :: Int -> Bool
isAdult age = age >= 18

-- Calcule l'année de naissance approximative
-- Utilise myAge comme référence (année 2024)
birthYear :: Int -> Int
birthYear currentAge = 2024 - currentAge

-- | Démonstration de l'immutabilité avec des listes
-- Les listes sont aussi immuables en Haskell

-- Liste de nombres favoris - ne peut pas être modifiée
favoriteNumbers :: [Int]
favoriteNumbers = [7, 13, 21, 42]

-- Liste de langages de programmation
programmingLanguages :: [String]
programmingLanguages = ["Haskell", "Rust", "Scala", "Clojure"]

-- | Fonctions qui "modifient" des listes (créent de nouvelles listes)
-- Démonstrent comment travailler avec l'immutabilité

-- Ajoute un nombre à la liste (crée une nouvelle liste)
addToFavorites :: Int -> [Int]
addToFavorites newNumber = newNumber : favoriteNumbers

-- Double tous les nombres favoris (nouvelle liste)
doubleFavorites :: [Int]
doubleFavorites = map (*2) favoriteNumbers

-- Filtre les nombres pairs de la liste favorite
evenFavorites :: [Int]
evenFavorites = filter even favoriteNumbers

-- | Démonstration de l'immutabilité avec des tuples
-- Les tuples sont aussi immuables

-- Coordonnées d'un point - ne peuvent pas changer
homeCoordinates :: (Double, Double)
homeCoordinates = (48.8566, 2.3522)  -- Paris

-- Informations personnelles - tuple immuable
personalInfo :: (String, Int, Bool)
personalInfo = ("Alice", myAge, isHaskellFun)

-- | Fonctions pour travailler avec des données immuables
-- Montrent comment créer de nouvelles valeurs sans modifier les anciennes

-- Déplace un point (crée un nouveau point)
movePoint :: (Double, Double) -> Double -> Double -> (Double, Double)
movePoint (x, y) deltaX deltaY = (x + deltaX, y + deltaY)

-- Met à jour l'âge dans les infos personnelles (nouveau tuple)
updateAge :: (String, Int, Bool) -> Int -> (String, Int, Bool)
updateAge (name, _, funStatus) newAge = (name, newAge, funStatus)

-- | Exemples de "variables" locales immuables
-- Dans une fonction, les bindings let sont aussi immuables

calculateComplexValue :: Int -> Int
calculateComplexValue input = 
  let step1 = input * 2          -- step1 ne peut pas changer
      step2 = step1 + 10         -- step2 ne peut pas changer
      step3 = step2 `div` 3      -- step3 ne peut pas changer
  in step3

-- | Fonction principale pour démontrer l'immutabilité
main :: IO ()
main = do
  putStrLn "=== VARIABLES IMMUABLES EN HASKELL ==="
  putStrLn ""
  
  -- Démonstration des valeurs immuables de base
  putStrLn "=== Valeurs Immuables de Base ==="
  putStrLn ""
  putStrLn $ "• myAge = " ++ show myAge
  putStrLn "  → Cette valeur ne peut jamais être modifiée"
  putStrLn $ "• piValue = " ++ show piValue
  putStrLn "  → Constante mathématique précise et immuable"
  putStrLn $ "• greeting = \"" ++ greeting ++ "\""
  putStrLn "  → Chaîne de caractères fixe"
  putStrLn $ "• isHaskellFun = " ++ show isHaskellFun
  putStrLn "  → État booléen permanent (et vrai!)"
  putStrLn ""
  
  -- Utilisation des valeurs immuables dans des calculs
  putStrLn "=== Utilisation dans des Calculs ==="
  putStrLn ""
  let radius = 5.0
  putStrLn $ "• Aire d'un cercle de rayon " ++ show radius ++ " : " ++ show (circleArea radius)
  putStrLn "  → Utilise piValue (immuable) pour le calcul"
  putStrLn ""
  
  let userName = "Sophie"
  putStrLn $ "• " ++ personalGreeting userName
  putStrLn "  → Combine greeting (immuable) avec un paramètre"
  putStrLn ""
  
  putStrLn $ "• Est-ce que " ++ show myAge ++ " ans est majeur ? " ++ show (isAdult myAge)
  putStrLn $ "• Année de naissance approximative : " ++ show (birthYear myAge)
  putStrLn ""
  
  -- Démonstration avec des listes immuables
  putStrLn "=== Listes Immuables ==="
  putStrLn ""
  putStrLn $ "• Nombres favoris originaux : " ++ show favoriteNumbers
  putStrLn "  → Cette liste ne peut pas être modifiée"
  putStrLn ""
  
  let newFavorites = addToFavorites 99
  putStrLn $ "• Après ajout de 99 : " ++ show newFavorites
  putStrLn $ "• Liste originale toujours : " ++ show favoriteNumbers
  putStrLn "  → addToFavorites crée une NOUVELLE liste"
  putStrLn ""
  
  putStrLn $ "• Favoris doublés : " ++ show doubleFavorites
  putStrLn $ "• Favoris pairs seulement : " ++ show evenFavorites
  putStrLn $ "• Original inchangé : " ++ show favoriteNumbers
  putStrLn "  → Chaque opération crée une nouvelle liste"
  putStrLn ""
  
  -- Démonstration avec des tuples
  putStrLn "=== Tuples Immuables ==="
  putStrLn ""
  putStrLn $ "• Coordonnées de départ : " ++ show homeCoordinates
  let newCoords = movePoint homeCoordinates 1.0 0.5
  putStrLn $ "• Après déplacement : " ++ show newCoords
  putStrLn $ "• Coordonnées originales : " ++ show homeCoordinates
  putStrLn "  → movePoint crée un NOUVEAU tuple"
  putStrLn ""
  
  putStrLn $ "• Infos personnelles : " ++ show personalInfo
  let updatedInfo = updateAge personalInfo 30
  putStrLn $ "• Avec âge mis à jour : " ++ show updatedInfo
  putStrLn $ "• Infos originales : " ++ show personalInfo
  putStrLn "  → updateAge crée un NOUVEAU tuple"
  putStrLn ""
  
  -- Démonstration des bindings let immuables
  putStrLn "=== Bindings Locaux Immuables ==="
  putStrLn ""
  let testInput = 15
  let result = calculateComplexValue testInput
  putStrLn $ "• Calcul complexe avec " ++ show testInput ++ " : " ++ show result
  putStrLn "  → Toutes les étapes intermédiaires sont immuables"
  putStrLn ""
  
  -- Comparaison avec des langages mutables
  putStrLn "=== Comparaison avec des Langages Mutables ==="
  putStrLn ""
  putStrLn "En Java/Python/C++ (langages mutables) :"
  putStrLn "  int x = 5;"
  putStrLn "  x = 10;        // ✅ Modification possible"
  putStrLn "  list.add(42);  // ✅ Mutation de la liste"
  putStrLn ""
  putStrLn "En Haskell (langage immuable) :"
  putStrLn "  let x = 5"
  putStrLn "  let x = 10     -- ❌ Impossible! Nouvelle variable"
  putStrLn "  newList = 42 : oldList  -- ✅ Nouvelle liste"
  putStrLn ""
  
  -- Avantages de l'immutabilité
  putStrLn "=== Avantages de l'Immutabilité ==="
  putStrLn ""
  putStrLn "1. 🔒 SÉCURITÉ :"
  putStrLn "   • Pas de modification accidentelle"
  putStrLn "   • Pas d'effets de bord surprenants"
  putStrLn ""
  putStrLn "2. 🧠 RAISONNEMENT FACILE :"
  putStrLn "   • Une valeur = une valeur, toujours"
  putStrLn "   • Pas besoin de traquer les changements"
  putStrLn ""
  putStrLn "3. 🔄 CONCURRENCE SÛRE :"
  putStrLn "   • Pas de race conditions possibles"
  putStrLn "   • Partage sûr entre threads"
  putStrLn ""
  putStrLn "4. 🐛 MOINS DE BUGS :"
  putStrLn "   • Pas de mutations inattendues"
  putStrLn "   • Comportement prévisible"
  putStrLn ""
  
  putStrLn "✅ L'immutabilité rend le code plus sûr et prévisible !"
```

### 🔍 Instructions GHCi

```ghci
-- Dans GHCi, testez l'immutabilité :

-- Définition de valeurs
let x = 42
let name = "Haskell"
let numbers = [1,2,3,4,5]

-- Vérification des types
:t x
-- x :: Num a => a

:t name
-- name :: String

:t numbers
-- numbers :: Num a => [a]

-- Tentative de "modification" (crée de nouvelles valeurs)
let x2 = x + 10
let newName = name ++ " rocks!"
let newNumbers = 0 : numbers

-- Vérification que les originaux n'ont pas changé
x
-- 42

name
-- "Haskell"

numbers
-- [1,2,3,4,5]

-- Les nouvelles valeurs sont différentes
x2
-- 52

newName
-- "Haskell rocks!"

newNumbers
-- [0,1,2,3,4,5]

-- Test avec des listes
let doubled = map (*2) numbers
doubled
-- [2,4,6,8,10]

numbers  -- Original inchangé
-- [1,2,3,4,5]
```

### 📊 Sortie Attendue
```
=== VARIABLES IMMUABLES EN HASKELL ===

=== Valeurs Immuables de Base ===

• myAge = 25
  → Cette valeur ne peut jamais être modifiée
• piValue = 3.141592653589793
  → Constante mathématique précise et immuable
• greeting = "Bonjour tout le monde!"
  → Chaîne de caractères fixe
• isHaskellFun = True
  → État booléen permanent (et vrai!)

=== Utilisation dans des Calculs ===

• Aire d'un cercle de rayon 5.0 : 78.53981633974483
  → Utilise piValue (immuable) pour le calcul

• Bonjour tout le monde! Sophie!
  → Combine greeting (immuable) avec un paramètre

• Est-ce que 25 ans est majeur ? True
• Année de naissance approximative : 1999

=== Listes Immuables ===

• Nombres favoris originaux : [7,13,21,42]
  → Cette liste ne peut pas être modifiée

• Après ajout de 99 : [99,7,13,21,42]
• Liste originale toujours : [7,13,21,42]
  → addToFavorites crée une NOUVELLE liste

• Favoris doublés : [14,26,42,84]
• Favoris pairs seulement : [42]
• Original inchangé : [7,13,21,42]
  → Chaque opération crée une nouvelle liste

=== Avantages de l'Immutabilité ===

1. 🔒 SÉCURITÉ :
   • Pas de modification accidentelle
   • Pas d'effets de bord surprenants

2. 🧠 RAISONNEMENT FACILE :
   • Une valeur = une valeur, toujours
   • Pas besoin de traquer les changements

3. 🔄 CONCURRENCE SÛRE :
   • Pas de race conditions possibles
   • Partage sûr entre threads

4. 🐛 MOINS DE BUGS :
   • Pas de mutations inattendues
   • Comportement prévisible

✅ L'immutabilité rend le code plus sûr et prévisible !
```

### 🚀 Points Importants à Retenir
1. **Immutabilité par défaut** : Toutes les valeurs sont immuables en Haskell
2. **Nouvelles valeurs, pas de modification** : Les opérations créent de nouvelles données
3. **Sécurité accrue** : Pas de modifications accidentelles ou d'effets de bord
4. **Raisonnement simplifié** : Une valeur garde toujours la même signification
5. **Concurrence sûre** : Pas de race conditions avec des données immuables

### 🧠 Explication Détaillée - Immutabilité vs Mutabilité

Comparaison entre les approches immuable (Haskell) et mutable (langages traditionnels) :

```haskell
-- HASKELL (Immuable)
let numbers = [1,2,3]
let moreNumbers = 4 : numbers
-- numbers reste [1,2,3]
-- moreNumbers devient [4,1,2,3]
-- Deux listes distinctes coexistent

-- JAVA/C++ (Mutable) 
// List<Integer> numbers = Arrays.asList(1,2,3);
// numbers.add(4);  // Modifie la liste originale
// numbers est maintenant [1,2,3,4]
// L'état original est perdu !

-- Avantage Haskell : traçabilité et sécurité
-- L'état précédent est toujours disponible
-- Pas de surprises, pas d'effets de bord
```
