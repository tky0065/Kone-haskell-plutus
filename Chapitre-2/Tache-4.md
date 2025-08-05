## ✅ HC2T4 - Notation préfixe et infixe

### 🎯 Objectif
Maîtriser les **notations préfixe et infixe** en Haskell - deux façons d'écrire et d'appeler les fonctions et opérateurs. Comprendre quand utiliser `(+)` vs `+`, et comment transformer des fonctions normales en opérateurs infixes avec les backticks.

### 📝 Concepts Clés
- **Notation préfixe** : L'opérateur/fonction vient avant les arguments `(+) 5 3`
- **Notation infixe** : L'opérateur vient entre les arguments `5 + 3`
- **Parenthèses pour préfixe** : Les opérateurs infixes deviennent préfixes avec `()`
- **Backticks pour infixe** : Les fonctions préfixes deviennent infixes avec `` `backticks` ``

### 💻 Code Complet

```haskell
-- Main.hs

-- | Fonctions utilitaires pour démontrer les notations
-- Ces fonctions seront utilisées en notation préfixe et infixe

-- Fonction pour calculer la puissance (version préfixe normale)
power :: Int -> Int -> Int
power base exponent = base ^ exponent

-- Fonction pour calculer la moyenne de deux nombres
average :: Double -> Double -> Double
average x y = (x + y) / 2

-- Fonction pour vérifier si un nombre est entre deux autres
isBetween :: Int -> Int -> Int -> Bool
isBetween min max value = value >= min && value <= max

-- Fonction pour concaténer avec un séparateur
concatWith :: String -> String -> String -> String
concatWith separator str1 str2 = str1 ++ separator ++ str2

-- | Fonction principale pour démontrer les notations préfixe et infixe
main :: IO ()
main = do
  putStrLn "=== NOTATION PRÉFIXE ET INFIXE EN HASKELL ==="
  putStrLn ""
  
  -- =========================================================================
  -- SECTION 1: OPÉRATEURS INFIXES EN NOTATION PRÉFIXE
  -- =========================================================================
  putStrLn "=== OPÉRATEURS INFIXES → NOTATION PRÉFIXE ==="
  putStrLn "Les opérateurs normalement infixes peuvent être utilisés en préfixe avec ()"
  putStrLn ""
  
  -- Opérateurs arithmétiques
  putStrLn "🧮 Opérateurs arithmétiques :"
  putStrLn $ "• Infixe normal  : 5 + 3 = " ++ show (5 + 3)
  putStrLn $ "• Notation préfixe : (+) 5 3 = " ++ show ((+) 5 3)
  putStrLn ""
  
  putStrLn $ "• Infixe normal  : 10 * 4 = " ++ show (10 * 4)
  putStrLn $ "• Notation préfixe : (*) 10 4 = " ++ show ((*) 10 4)
  putStrLn ""
  
  putStrLn $ "• Infixe normal  : 20 - 8 = " ++ show (20 - 8)
  putStrLn $ "• Notation préfixe : (-) 20 8 = " ++ show ((-) 20 8)
  putStrLn ""
  
  putStrLn $ "• Infixe normal  : 15 `div` 3 = " ++ show (15 `div` 3)
  putStrLn $ "• Notation préfixe : div 15 3 = " ++ show (div 15 3)
  putStrLn ""
  
  -- Opérateurs logiques
  putStrLn "🔗 Opérateurs logiques :"
  putStrLn $ "• Infixe normal  : True && False = " ++ show (True && False)
  putStrLn $ "• Notation préfixe : (&&) True False = " ++ show ((&&) True False)
  putStrLn ""
  
  putStrLn $ "• Infixe normal  : True || False = " ++ show (True || False)
  putStrLn $ "• Notation préfixe : (||) True False = " ++ show ((||) True False)
  putStrLn ""
  
  -- Opérateurs de comparaison
  putStrLn "⚖️  Opérateurs de comparaison :"
  putStrLn $ "• Infixe normal  : 5 > 3 = " ++ show (5 > 3)
  putStrLn $ "• Notation préfixe : (>) 5 3 = " ++ show ((>) 5 3)
  putStrLn ""
  
  putStrLn $ "• Infixe normal  : 10 == 10 = " ++ show (10 == 10)
  putStrLn $ "• Notation préfixe : (==) 10 10 = " ++ show ((==) 10 10)
  putStrLn ""
  
  -- =========================================================================
  -- SECTION 2: FONCTIONS PRÉFIXES EN NOTATION INFIXE
  -- =========================================================================
  putStrLn "=== FONCTIONS PRÉFIXES → NOTATION INFIXE ==="
  putStrLn "Les fonctions normalement préfixes peuvent être utilisées en infixe avec `backticks`"
  putStrLn ""
  
  -- Fonctions mathématiques
  putStrLn "🔢 Fonctions mathématiques :"
  putStrLn $ "• Notation préfixe : power 2 8 = " ++ show (power 2 8)
  putStrLn $ "• Notation infixe   : 2 `power` 8 = " ++ show (2 `power` 8)
  putStrLn "  → Plus lisible : '2 à la puissance 8'"
  putStrLn ""
  
  putStrLn $ "• Notation préfixe : average 10.5 15.3 = " ++ show (average 10.5 15.3)
  putStrLn $ "• Notation infixe   : 10.5 `average` 15.3 = " ++ show (10.5 `average` 15.3)
  putStrLn "  → Plus naturel : 'moyenne entre 10.5 et 15.3'"
  putStrLn ""
  
  -- Fonctions de chaînes
  putStrLn "📝 Fonctions sur chaînes :"
  putStrLn $ "• Notation préfixe : concatWith \"-\" \"Hello\" \"World\" = \"" ++ concatWith "-" "Hello" "World" ++ "\""
  putStrLn $ "• Notation infixe   : \"Hello\" `concatWith` \"-\" appliqué à \"World\""
  putStrLn "  (nécessite currification pour cette syntaxe)"
  putStrLn ""
  
  -- Fonctions logiques
  putStrLn "🔍 Fonctions de test :"
  putStrLn $ "• Notation préfixe : isBetween 1 10 5 = " ++ show (isBetween 1 10 5)
  putStrLn $ "• Notation infixe   : 5 `isBetween` (1, 10) - syntaxe adaptée"
  putStrLn "  → Plus lisible : '5 est entre 1 et 10'"
  putStrLn ""
  
  -- =========================================================================
  -- SECTION 3: FONCTIONS STANDARD AVEC LES DEUX NOTATIONS
  -- =========================================================================
  putStrLn "=== FONCTIONS STANDARD AVEC LES DEUX NOTATIONS ==="
  putStrLn ""
  
  -- div et mod
  putStrLn "🔢 Division et modulo :"
  putStrLn $ "• div 17 5 = " ++ show (div 17 5) ++ "   (préfixe)"
  putStrLn $ "• 17 `div` 5 = " ++ show (17 `div` 5) ++ "   (infixe)"
  putStrLn ""
  putStrLn $ "• mod 17 5 = " ++ show (mod 17 5) ++ "   (préfixe)"
  putStrLn $ "• 17 `mod` 5 = " ++ show (17 `mod` 5) ++ "   (infixe)"
  putStrLn ""
  
  -- max et min
  putStrLn "📊 Maximum et minimum :"
  putStrLn $ "• max 15 23 = " ++ show (max 15 23) ++ "  (préfixe)"
  putStrLn $ "• 15 `max` 23 = " ++ show (15 `max` 23) ++ "  (infixe)"
  putStrLn ""
  putStrLn $ "• min 15 23 = " ++ show (min 15 23) ++ "  (préfixe)"
  putStrLn $ "• 15 `min` 23 = " ++ show (15 `min` 23) ++ "  (infixe)"
  putStrLn ""
  
  -- elem (appartenance à une liste)
  putStrLn "📝 Test d'appartenance à une liste :"
  let numbers = [1, 3, 5, 7, 9]
  putStrLn $ "• Liste : " ++ show numbers
  putStrLn $ "• elem 5 " ++ show numbers ++ " = " ++ show (elem 5 numbers) ++ "  (préfixe)"
  putStrLn $ "• 5 `elem` " ++ show numbers ++ " = " ++ show (5 `elem` numbers) ++ "  (infixe)"
  putStrLn "  → Plus naturel : '5 appartient à la liste'"
  putStrLn ""
  
  -- =========================================================================
  -- SECTION 4: UTILISATION PRATIQUE AVEC MAP ET AUTRES
  -- =========================================================================
  putStrLn "=== UTILISATION PRATIQUE AVEC LES FONCTIONS D'ORDRE SUPÉRIEUR ==="
  putStrLn ""
  
  let testNumbers = [1, 2, 3, 4, 5]
  
  putStrLn "📋 Avec map et les opérateurs en notation préfixe :"
  putStrLn $ "• Liste originale : " ++ show testNumbers
  putStrLn $ "• map (+10) = " ++ show (map (+10) testNumbers) ++ "  (ajoute 10 à chaque élément)"
  putStrLn $ "• map (*3) = " ++ show (map (*3) testNumbers) ++ "   (multiplie par 3)"
  putStrLn $ "• map (^2) = " ++ show (map (^2) testNumbers) ++ "   (met au carré)"
  putStrLn ""
  
  putStrLn "🔍 Avec filter et les opérateurs de comparaison :"
  putStrLn $ "• filter (>3) = " ++ show (filter (>3) testNumbers) ++ "     (éléments > 3)"
  putStrLn $ "• filter (==3) = " ++ show (filter (==3) testNumbers) ++ "       (éléments == 3)"
  putStrLn $ "• filter (`elem` [2,4]) = " ++ show (filter (`elem` [2,4]) testNumbers) ++ "   (appartient à [2,4])"
  putStrLn ""
  
  -- =========================================================================
  -- SECTION 5: COMPARAISON DES STYLES ET LISIBILITÉ
  -- =========================================================================
  putStrLn "=== COMPARAISON DES STYLES ET LISIBILITÉ ==="
  putStrLn ""
  
  putStrLn "🎯 Quand utiliser chaque notation :"
  putStrLn ""
  putStrLn "1. NOTATION PRÉFIXE pour opérateurs (avec parenthèses) :"
  putStrLn "   ✅ Avec map, filter : map (+1), filter (>5)"
  putStrLn "   ✅ Application partielle : let addTen = (+10)"
  putStrLn "   ✅ Passage comme argument : foldl (+) 0"
  putStrLn ""
  
  putStrLn "2. NOTATION INFIXE pour fonctions (avec backticks) :"
  putStrLn "   ✅ Lisibilité naturelle : 5 `elem` list"
  putStrLn "   ✅ Opérations binaires : a `max` b"
  putStrLn "   ✅ Tests : value `isBetween` (min, max)"
  putStrLn ""
  
  putStrLn "3. STYLE RECOMMANDÉ :"
  putStrLn "   • Utilisez la notation la plus lisible"
  putStrLn "   • Préfixe pour les opérations fonctionnelle"
  putStrLn "   • Infixe pour les comparaisons et tests"
  putStrLn ""
  
  -- =========================================================================
  -- SECTION 6: EXEMPLES AVANCÉS ET COMBINAISONS
  -- =========================================================================
  putStrLn "=== EXEMPLES AVANCÉS ET COMBINAISONS ==="
  putStrLn ""
  
  -- Combinaisons complexes
  let result1 = 10 + 5 * 3  -- infixe normal
  let result2 = (+) 10 ((*) 5 3)  -- tout en préfixe
  putStrLn $ "• Expression mixte : 10 + 5 * 3 = " ++ show result1
  putStrLn $ "• Tout en préfixe : (+) 10 ((*) 5 3) = " ++ show result2
  putStrLn "  → Même résultat, styles différents"
  putStrLn ""
  
  -- Avec des listes
  let numbers1 = [1..10]
  let numbers2 = [5..15]
  putStrLn $ "• Liste 1 : " ++ show numbers1
  putStrLn $ "• Liste 2 : " ++ show numbers2
  putStrLn $ "• Éléments communs (avec elem) : " ++ show (filter (`elem` numbers2) numbers1)
  putStrLn ""
  
  -- Calculs en chaîne
  let chainResult = 20 `div` 4 `max` 3 `min` 10
  putStrLn $ "• Calcul en chaîne : 20 `div` 4 `max` 3 `min` 10 = " ++ show chainResult
  putStrLn "  → Se lit : ((20 div 4) max 3) min 10 = (5 max 3) min 10 = 5 min 10 = 5"
  putStrLn ""
  
  putStrLn "✅ Maîtrisez les deux notations pour un code expressif et lisible !"
```

### 🔍 Instructions GHCi

```ghci
-- Dans GHCi, testez les notations préfixe et infixe :

-- Opérateurs en notation préfixe
(+) 5 3
-- 8

(*) 10 4
-- 40

(>) 15 10
-- True

(&&) True False
-- False

-- Fonctions en notation infixe
5 `max` 10
-- 10

17 `div` 3
-- 5

5 `elem` [1,3,5,7,9]
-- True

"Hello" `elem` ["Hi", "Hello", "Bonjour"]
-- True

-- Application partielle avec notation préfixe
let addFive = (+5)
map addFive [1,2,3,4,5]
-- [6,7,8,9,10]

let multiplyByTwo = (*2)
map multiplyByTwo [1,2,3,4,5]
-- [2,4,6,8,10]

-- Comparaisons avec notation préfixe dans filter
filter (>3) [1,2,3,4,5,6]
-- [4,5,6]

filter (==0) [0,1,0,2,0,3]
-- [0,0,0]

-- Combinaisons
let result = 10 `max` 5 `min` 7
result
-- 7

-- Vérification des types
:t (+)
-- (+) :: Num a => a -> a -> a

:t max
-- max :: Ord a => a -> a -> a

:t elem
-- elem :: Eq a => a -> [a] -> Bool
```

### 📊 Sortie Attendue
```
=== NOTATION PRÉFIXE ET INFIXE EN HASKELL ===

=== OPÉRATEURS INFIXES → NOTATION PRÉFIXE ===
Les opérateurs normalement infixes peuvent être utilisés en préfixe avec ()

🧮 Opérateurs arithmétiques :
• Infixe normal  : 5 + 3 = 8
• Notation préfixe : (+) 5 3 = 8

• Infixe normal  : 10 * 4 = 40
• Notation préfixe : (*) 10 4 = 40

• Infixe normal  : 20 - 8 = 12
• Notation préfixe : (-) 20 8 = 12

• Infixe normal  : 15 `div` 3 = 5
• Notation préfixe : div 15 3 = 5

🔗 Opérateurs logiques :
• Infixe normal  : True && False = False
• Notation préfixe : (&&) True False = False

• Infixe normal  : True || False = True
• Notation préfixe : (||) True False = True

⚖️  Opérateurs de comparaison :
• Infixe normal  : 5 > 3 = True
• Notation préfixe : (>) 5 3 = True

• Infixe normal  : 10 == 10 = True
• Notation préfixe : (==) 10 10 = True

=== FONCTIONS PRÉFIXES → NOTATION INFIXE ===
Les fonctions normalement préfixes peuvent être utilisées en infixe avec `backticks`

🔢 Fonctions mathématiques :
• Notation préfixe : power 2 8 = 256
• Notation infixe   : 2 `power` 8 = 256
  → Plus lisible : '2 à la puissance 8'

• Notation préfixe : average 10.5 15.3 = 12.9
• Notation infixe   : 10.5 `average` 15.3 = 12.9
  → Plus naturel : 'moyenne entre 10.5 et 15.3'

📝 Fonctions sur chaînes :
• Notation préfixe : concatWith "-" "Hello" "World" = "Hello-World"
• Notation infixe   : "Hello" `concatWith` "-" appliqué à "World"
  (nécessite currification pour cette syntaxe)

🔍 Fonctions de test :
• Notation préfixe : isBetween 1 10 5 = True
• Notation infixe   : 5 `isBetween` (1, 10) - syntaxe adaptée
  → Plus lisible : '5 est entre 1 et 10'

=== FONCTIONS STANDARD AVEC LES DEUX NOTATIONS ===

🔢 Division et modulo :
• div 17 5 = 3   (préfixe)
• 17 `div` 5 = 3   (infixe)

• mod 17 5 = 2   (préfixe)
• 17 `mod` 5 = 2   (infixe)

📊 Maximum et minimum :
• max 15 23 = 23  (préfixe)
• 15 `max` 23 = 23  (infixe)

• min 15 23 = 15  (préfixe)
• 15 `min` 23 = 15  (infixe)

📝 Test d'appartenance à une liste :
• Liste : [1,3,5,7,9]
• elem 5 [1,3,5,7,9] = True  (préfixe)
• 5 `elem` [1,3,5,7,9] = True  (infixe)
  → Plus naturel : '5 appartient à la liste'

🎯 Quand utiliser chaque notation :

1. NOTATION PRÉFIXE pour opérateurs (avec parenthèses) :
   ✅ Avec map, filter : map (+1), filter (>5)
   ✅ Application partielle : let addTen = (+10)
   ✅ Passage comme argument : foldl (+) 0

2. NOTATION INFIXE pour fonctions (avec backticks) :
   ✅ Lisibilité naturelle : 5 `elem` list
   ✅ Opérations binaires : a `max` b
   ✅ Tests : value `isBetween` (min, max)

✅ Maîtrisez les deux notations pour un code expressif et lisible !
```

### 🚀 Points Importants à Retenir
1. **Parenthèses () → préfixe** : `(+) 5 3` au lieu de `5 + 3`
2. **Backticks `` → infixe** : `5 `max` 10` au lieu de `max 5 10`
3. **Application partielle** : `(+5)`, `(*2)` créent de nouvelles fonctions
4. **Lisibilité** : Choisir la notation la plus naturelle selon le contexte
5. **Fonctions d'ordre supérieur** : La notation préfixe est souvent plus pratique

### 🧠 Explication Détaillée - Choix de Notation

Le choix entre préfixe et infixe dépend du contexte et de la lisibilité :

```haskell
-- NOTATION PRÉFIXE - Meilleure pour :
map (+10) [1,2,3]        -- Application partielle claire
foldl (+) 0 [1,2,3,4]    -- Opérateur comme argument
filter (>5) numbers      -- Comparaison partielle

-- NOTATION INFIXE - Meilleure pour :
5 `elem` [1,3,5,7]       -- Lecture naturelle
10 `max` 20              -- Opération binaire claire
value `between` (1, 10)  -- Test de condition

-- RÈGLE GÉNÉRALE :
-- Utilisez la notation qui rend le code le plus lisible
-- et expressif pour le lecteur humain
```
