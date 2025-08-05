## ✅ HC2T7 - Expressions booléennes

### 🎯 Objectif
Maîtriser les **expressions booléennes** en Haskell avec les opérateurs logiques `&&`, `||`, `not` et les **opérateurs de comparaison**. Comprendre comment combiner des conditions pour créer des expressions logiques complexes et leur évaluation paresseuse.

### 📝 Concepts Clés
- **Type Bool** : Valeurs `True` et `False` uniquement
- **Opérateurs logiques** : `&&` (ET), `||` (OU), `not` (NON)
- **Opérateurs de comparaison** : `==`, `/=`, `<`, `<=`, `>`, `>=`
- **Évaluation paresseuse** : `&&` et `||` s'arrêtent dès que le résultat est connu

### 💻 Code Complet

```haskell
-- Main.hs

-- | Expressions booléennes qui évaluent à True ou False
-- Ces définitions montrent différentes façons de créer des valeurs booléennes

-- Expression True avec l'opérateur ET (&&)
trueWithAnd :: Bool
trueWithAnd = True && True

-- Expression False avec l'opérateur OU (||)
falseWithOr :: Bool
falseWithOr = False || False

-- Expression True avec l'opérateur NOT
trueWithNot :: Bool
trueWithNot = not False

-- Expression False avec comparaison
falseComparison :: Bool
falseComparison = 5 > 10

-- | Expressions booléennes plus complexes

-- Combinaison de comparaisons avec ET
complexAnd :: Bool
complexAnd = (5 > 3) && (10 == 10) && (7 <= 15)

-- Combinaison de comparaisons avec OU
complexOr :: Bool
complexOr = (5 > 10) || (3 == 3) || (20 < 15)

-- Expression mixte avec parenthèses
mixedExpression :: Bool
mixedExpression = (5 > 3 && 10 < 20) || (not (7 == 8))

-- | Fonctions qui retournent des booléens avec des paramètres

-- Vérifie si un nombre est dans un intervalle
inRange :: Int -> Int -> Int -> Bool
inRange min max value = value >= min && value <= max

-- Vérifie si un nombre est pair ET positif
evenAndPositive :: Int -> Bool
evenAndPositive n = (n `mod` 2 == 0) && (n > 0)

-- Vérifie si un nombre est soit très petit soit très grand
extremeValue :: Int -> Bool
extremeValue n = (n < 10) || (n > 1000)

-- Vérifie si une chaîne est vide OU très courte
shortOrEmpty :: String -> Bool
shortOrEmpty str = null str || length str <= 2

-- | Démonstration de l'évaluation paresseuse

-- Cette fonction ne sera jamais appelée (pour démontrer la paresse)
expensiveComputation :: Bool
expensiveComputation = error "Cette fonction ne devrait jamais être appelée!"

-- Utilise l'évaluation paresseuse avec &&
lazyAndExample :: Bool
lazyAndExample = False && expensiveComputation  -- expensiveComputation n'est pas évaluée

-- Utilise l'évaluation paresseuse avec ||
lazyOrExample :: Bool
lazyOrExample = True || expensiveComputation   -- expensiveComputation n'est pas évaluée

-- | Table de vérité pour les opérateurs logiques

-- Génère toutes les combinaisons pour &&
andTruthTable :: [(Bool, Bool, Bool)]
andTruthTable = [(a, b, a && b) | a <- [False, True], b <- [False, True]]

-- Génère toutes les combinaisons pour ||
orTruthTable :: [(Bool, Bool, Bool)]
orTruthTable = [(a, b, a || b) | a <- [False, True], b <- [False, True]]

-- Génère la table pour not
notTruthTable :: [(Bool, Bool)]
notTruthTable = [(a, not a) | a <- [False, True]]

-- | Exemples de comparaisons avec différents types

-- Comparaisons numériques
numericComparisons :: [(String, Bool)]
numericComparisons = [
    ("5 == 5", 5 == 5),
    ("3 /= 7", 3 /= 7),
    ("10 > 8", 10 > 8),
    ("15 <= 15", 15 <= 15),
    ("20 < 19", 20 < 19),
    ("25 >= 30", 25 >= 30)
  ]

-- Comparaisons de chaînes
stringComparisons :: [(String, Bool)]
stringComparisons = [
    ("\"abc\" == \"abc\"", "abc" == "abc"),
    ("\"hello\" /= \"world\"", "hello" /= "world"),
    ("\"apple\" < \"banana\"", "apple" < "banana"),
    ("\"zebra\" > \"alpha\"", "zebra" > "alpha")
  ]

-- Comparaisons de caractères
charComparisons :: [(String, Bool)]
charComparisons = [
    ("'A' == 'A'", 'A' == 'A'),
    ("'a' /= 'A'", 'a' /= 'A'),
    ("'B' > 'A'", 'B' > 'A'),
    ("'z' < 'a'", 'z' < 'a')
  ]

-- | Fonction principale pour démontrer les expressions booléennes
main :: IO ()
main = do
  putStrLn "=== EXPRESSIONS BOOLÉENNES EN HASKELL ==="
  putStrLn ""
  
  -- =========================================================================
  -- SECTION 1: OPÉRATEURS LOGIQUES DE BASE
  -- =========================================================================
  putStrLn "=== OPÉRATEURS LOGIQUES DE BASE ==="
  putStrLn ""
  
  putStrLn "🔵 Expressions avec ET (&&) :"
  putStrLn $ "• True && True = " ++ show trueWithAnd
  putStrLn $ "• (5 > 3) && (10 == 10) = " ++ show ((5 > 3) && (10 == 10))
  putStrLn $ "• Expression complexe : " ++ show complexAnd
  putStrLn "  → Toutes les conditions doivent être vraies"
  putStrLn ""
  
  putStrLn "🔴 Expressions avec OU (||) :"
  putStrLn $ "• False || False = " ++ show falseWithOr
  putStrLn $ "• (5 > 10) || (3 == 3) = " ++ show ((5 > 10) || (3 == 3))
  putStrLn $ "• Expression complexe : " ++ show complexOr
  putStrLn "  → Au moins une condition doit être vraie"
  putStrLn ""
  
  putStrLn "🔄 Expressions avec NOT :"
  putStrLn $ "• not False = " ++ show trueWithNot
  putStrLn $ "• not (5 > 10) = " ++ show (not falseComparison)
  putStrLn $ "• not (not True) = " ++ show (not (not True))
  putStrLn "  → Inverse la valeur booléenne"
  putStrLn ""
  
  -- =========================================================================
  -- SECTION 2: TABLES DE VÉRITÉ
  -- =========================================================================
  putStrLn "=== TABLES DE VÉRITÉ ==="
  putStrLn ""
  
  putStrLn "📊 Table de vérité pour && (ET) :"
  putStrLn "   A     |   B     | A && B"
  putStrLn "---------|---------|--------"
  mapM_ (\(a, b, result) -> 
    putStrLn $ " " ++ padShow a ++ " | " ++ padShow b ++ " |   " ++ show result
    ) andTruthTable
  putStrLn ""
  
  putStrLn "📊 Table de vérité pour || (OU) :"
  putStrLn "   A     |   B     | A || B"
  putStrLn "---------|---------|--------"
  mapM_ (\(a, b, result) -> 
    putStrLn $ " " ++ padShow a ++ " | " ++ padShow b ++ " |   " ++ show result
    ) orTruthTable
  putStrLn ""
  
  putStrLn "📊 Table de vérité pour not (NON) :"
  putStrLn "   A     | not A"
  putStrLn "---------|---------"
  mapM_ (\(a, result) -> 
    putStrLn $ " " ++ padShow a ++ " |  " ++ show result
    ) notTruthTable
  putStrLn ""
  
  -- =========================================================================
  -- SECTION 3: OPÉRATEURS DE COMPARAISON
  -- =========================================================================
  putStrLn "=== OPÉRATEURS DE COMPARAISON ==="
  putStrLn ""
  
  putStrLn "🔢 Comparaisons numériques :"
  mapM_ (\(expr, result) -> 
    putStrLn $ "• " ++ expr ++ " = " ++ show result
    ) numericComparisons
  putStrLn ""
  
  putStrLn "📝 Comparaisons de chaînes (ordre lexicographique) :"
  mapM_ (\(expr, result) -> 
    putStrLn $ "• " ++ expr ++ " = " ++ show result
    ) stringComparisons
  putStrLn ""
  
  putStrLn "🔤 Comparaisons de caractères (ordre ASCII) :"
  mapM_ (\(expr, result) -> 
    putStrLn $ "• " ++ expr ++ " = " ++ show result
    ) charComparisons
  putStrLn ""
  
  -- =========================================================================
  -- SECTION 4: FONCTIONS AVEC PARAMÈTRES BOOLÉENS
  -- =========================================================================
  putStrLn "=== FONCTIONS RETOURNANT DES BOOLÉENS ==="
  putStrLn ""
  
  putStrLn "📏 Test d'intervalle (inRange min max value) :"
  let rangeTests = [(1, 10, 5), (1, 10, 15), (0, 100, 50), (-5, 5, 0)]
  mapM_ (\(min, max, val) -> do
    let result = inRange min max val
    putStrLn $ "• inRange " ++ show min ++ " " ++ show max ++ " " ++ show val ++ " = " ++ show result
    ) rangeTests
  putStrLn ""
  
  putStrLn "🔢 Test pair ET positif :"
  let evenPosTests = [-4, -1, 0, 4, 7, 12]
  mapM_ (\n -> do
    let result = evenAndPositive n
    putStrLn $ "• evenAndPositive " ++ show n ++ " = " ++ show result ++ 
               " (pair: " ++ show (even n) ++ ", positif: " ++ show (n > 0) ++ ")"
    ) evenPosTests
  putStrLn ""
  
  putStrLn "📏 Test valeur extrême (< 10 OU > 1000) :"
  let extremeTests = [5, 15, 500, 1500]
  mapM_ (\n -> do
    let result = extremeValue n
    putStrLn $ "• extremeValue " ++ show n ++ " = " ++ show result
    ) extremeTests
  putStrLn ""
  
  -- =========================================================================
  -- SECTION 5: ÉVALUATION PARESSEUSE
  -- =========================================================================
  putStrLn "=== ÉVALUATION PARESSEUSE ==="
  putStrLn ""
  
  putStrLn "⚡ Démonstration de la paresse avec && et || :"
  putStrLn $ "• False && <erreur> = " ++ show lazyAndExample
  putStrLn "  → La deuxième expression n'est pas évaluée car False && _ = False"
  putStrLn ""
  putStrLn $ "• True || <erreur> = " ++ show lazyOrExample  
  putStrLn "  → La deuxième expression n'est pas évaluée car True || _ = True"
  putStrLn ""
  putStrLn "🎯 Avantages de l'évaluation paresseuse :"
  putStrLn "• Performance : évite les calculs inutiles"
  putStrLn "• Sécurité : évite les erreurs dans les branches non utilisées"
  putStrLn "• Expressivité : permet des idiomes comme 'null list || head list > 0'"
  putStrLn ""
  
  -- =========================================================================
  -- SECTION 6: EXPRESSIONS COMPLEXES
  -- =========================================================================
  putStrLn "=== EXPRESSIONS COMPLEXES ==="
  putStrLn ""
  
  putStrLn "🔗 Combinaisons avec parenthèses :"
  putStrLn $ "• (5 > 3 && 10 < 20) || (not (7 == 8)) = " ++ show mixedExpression
  putStrLn ""
  
  -- Expressions de De Morgan
  let a = True
  let b = False
  putStrLn "📐 Lois de De Morgan :"
  putStrLn $ "• not (a && b) = " ++ show (not (a && b)) ++ 
             ", (not a) || (not b) = " ++ show ((not a) || (not b))
  putStrLn $ "• not (a || b) = " ++ show (not (a || b)) ++
             ", (not a) && (not b) = " ++ show ((not a) && (not b))
  putStrLn "  → Les deux expressions de chaque ligne sont équivalentes"
  putStrLn ""
  
  -- =========================================================================
  -- SECTION 7: CONSEILS PRATIQUES
  -- =========================================================================
  putStrLn "=== CONSEILS PRATIQUES ==="
  putStrLn ""
  putStrLn "💡 Bonnes pratiques :"
  putStrLn "1. 🎯 LISIBILITÉ :"
  putStrLn "   • Utilisez des parenthèses pour clarifier la priorité"
  putStrLn "   • Nommez les expressions complexes avec let/where"
  putStrLn ""
  putStrLn "2. ⚡ PERFORMANCE :"
  putStrLn "   • Placez les conditions les plus probables en premier"
  putStrLn "   • Exploitez l'évaluation paresseuse de && et ||"
  putStrLn ""
  putStrLn "3. 🔒 SÉCURITÉ :"
  putStrLn "   • Vérifiez les conditions de sécurité avant les calculs coûteux"
  putStrLn "   • Exemple : null list || head list > 0"
  putStrLn ""
  
  putStrLn "✅ Les expressions booléennes sont le fondement de la logique en Haskell !"

-- | Fonction utilitaire pour formater l'affichage des booléens
padShow :: Bool -> String
padShow True = "True "
padShow False = "False"
```

### 🔍 Instructions GHCi

```ghci
-- Dans GHCi, testez les expressions booléennes :

-- Opérateurs logiques de base
True && True
-- True

False || True
-- True

not False
-- True

-- Comparaisons
5 > 3
-- True

"apple" < "banana"
-- True

'A' == 'A'
-- True

-- Expressions complexes
(5 > 3) && (10 < 20)
-- True

(2 > 5) || (3 == 3)
-- True

not (5 == 10)
-- True

-- Évaluation paresseuse
False && undefined
-- False (undefined n'est pas évaluée)

True || undefined  
-- True (undefined n'est pas évaluée)

-- Lois de De Morgan
let a = True; let b = False
not (a && b) == ((not a) || (not b))
-- True

not (a || b) == ((not a) && (not b))
-- True

-- Vérification des types
:t (5 > 3)
-- (5 > 3) :: Bool

:t (&&)
-- (&&) :: Bool -> Bool -> Bool

:t not
-- not :: Bool -> Bool
```

### 📊 Sortie Attendue
```
=== EXPRESSIONS BOOLÉENNES EN HASKELL ===

=== OPÉRATEURS LOGIQUES DE BASE ===

🔵 Expressions avec ET (&&) :
• True && True = True
• (5 > 3) && (10 == 10) = True
• Expression complexe : True
  → Toutes les conditions doivent être vraies

🔴 Expressions avec OU (||) :
• False || False = False
• (5 > 10) || (3 == 3) = True
• Expression complexe : True
  → Au moins une condition doit être vraie

🔄 Expressions avec NOT :
• not False = True
• not (5 > 10) = True
• not (not True) = True
  → Inverse la valeur booléenne

=== TABLES DE VÉRITÉ ===

📊 Table de vérité pour && (ET) :
   A     |   B     | A && B
---------|---------|--------
 False | False |   False
 False | True  |   False
 True  | False |   False
 True  | True  |   True

📊 Table de vérité pour || (OU) :
   A     |   B     | A || B
---------|---------|--------
 False | False |   False
 False | True  |   True
 True  | False |   True
 True  | True  |   True

📊 Table de vérité pour not (NON) :
   A     | not A
---------|---------"
 False |  True
 True  |  False

=== OPÉRATEURS DE COMPARAISON ===

🔢 Comparaisons numériques :
• 5 == 5 = True
• 3 /= 7 = True
• 10 > 8 = True
• 15 <= 15 = True
• 20 < 19 = False
• 25 >= 30 = False

📝 Comparaisons de chaînes (ordre lexicographique) :
• "abc" == "abc" = True
• "hello" /= "world" = True
• "apple" < "banana" = True
• "zebra" > "alpha" = True

=== ÉVALUATION PARESSEUSE ===

⚡ Démonstration de la paresse avec && et || :
• False && <erreur> = False
  → La deuxième expression n'est pas évaluée car False && _ = False

• True || <erreur> = True
  → La deuxième expression n'est pas évaluée car True || _ = True

✅ Les expressions booléennes sont le fondement de la logique en Haskell !
```

### 🚀 Points Importants à Retenir
1. **Type Bool** : Seulement deux valeurs possibles `True` et `False`
2. **Opérateurs logiques** : `&&` (ET), `||` (OU), `not` (NON) avec priorité des opérations
3. **Comparaisons** : `==`, `/=`, `<`, `<=`, `>`, `>=` retournent toujours Bool
4. **Évaluation paresseuse** : `&&` et `||` optimisent automatiquement les calculs
5. **Expressions complexes** : Utilisez des parenthèses pour clarifier la priorité

### 🧠 Explication Détaillée - Évaluation Paresseuse

L'évaluation paresseuse des opérateurs logiques est une optimisation puissante :

```haskell
-- AVEC && (ET)
condition1 && condition2
-- Si condition1 = False, condition2 n'est jamais évaluée
-- Car False && _ = False toujours

-- AVEC || (OU)  
condition1 || condition2
-- Si condition1 = True, condition2 n'est jamais évaluée
-- Car True || _ = True toujours

-- EXEMPLE PRATIQUE
safeCheck :: [Int] -> Bool
safeCheck list = not (null list) && head list > 0
-- Si la liste est vide, head list ne sera jamais appelé
-- (évite une erreur runtime)

-- ORDRE DES CONDITIONS IMPORTANT
efficientCheck :: Int -> Bool  
efficientCheck n = (n > 0) && (expensiveFunction n > 100)
-- Teste d'abord la condition rapide (n > 0)
-- N'appelle expensiveFunction que si nécessaire
```
