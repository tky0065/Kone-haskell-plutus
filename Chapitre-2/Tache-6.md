## ✅ HC2T6 - Comprendre Int vs Integer

### 🎯 Objectif
Comprendre la différence fondamentale entre **Int** et **Integer** en Haskell - deux types d'entiers avec des caractéristiques très différentes en termes de **taille**, **performance** et **précision**. Apprendre quand utiliser chaque type selon le contexte.

### 📝 Concepts Clés
- **Int** : Entier de taille fixe (généralement 64 bits), plus rapide mais limité
- **Integer** : Entier de précision arbitraire, illimité mais plus lent
- **Overflow** : Dépassement de capacité avec Int, impossible avec Integer
- **Performance** : Int plus rapide pour les calculs, Integer plus sûr pour les grands nombres

### 💻 Code Complet

```haskell
-- Main.hs

-- | Définition des variables avec types spécifiques
-- Ces exemples montrent les différences de capacité

-- Nombre utilisant Int - limité par la taille de la machine
smallNumber :: Int
smallNumber = 2^62  -- Proche de la limite maximale d'Int

-- Nombre utilisant Integer - pas de limite théorique
bigNumber :: Integer
bigNumber = 2^127   -- Bien au-delà des capacités d'Int

-- Très grand nombre pour démontrer la puissance d'Integer
hugeNumber :: Integer
hugeNumber = 2^1000  -- Impossible avec Int !

-- | Constantes pour tester les limites d'Int
-- Ces valeurs sont proches des limites système
maxIntApprox :: Int
maxIntApprox = 9223372036854775807  -- 2^63 - 1 (sur système 64 bits)

minIntApprox :: Int
minIntApprox = -9223372036854775808  -- -2^63 (sur système 64 bits)

-- | Fonctions pour démontrer les différences de comportement

-- Calcule la factorielle avec Int (risque d'overflow)
factorialInt :: Int -> Int
factorialInt n
  | n <= 0 = 1
  | otherwise = n * factorialInt (n - 1)

-- Calcule la factorielle avec Integer (pas de risque d'overflow)
factorialInteger :: Integer -> Integer
factorialInteger n
  | n <= 0 = 1
  | otherwise = n * factorialInteger (n - 1)

-- Calcule une puissance avec Int
powerInt :: Int -> Int -> Int
powerInt base exp
  | exp == 0 = 1
  | exp > 0 = base * powerInt base (exp - 1)
  | otherwise = error "Exposant négatif"

-- Calcule une puissance avec Integer
powerInteger :: Integer -> Integer -> Integer
powerInteger base exp
  | exp == 0 = 1
  | exp > 0 = base * powerInteger base (exp - 1)
  | otherwise = error "Exposant négatif"

-- | Fonctions de conversion entre Int et Integer

-- Convertit Int vers Integer (toujours sûr)
intToInteger :: Int -> Integer
intToInteger = fromIntegral

-- Convertit Integer vers Int (peut perdre de la précision)
integerToInt :: Integer -> Int
integerToInt = fromIntegral

-- | Fonctions de test pour comparer les performances

-- Suite de Fibonacci avec Int (limité)
fibInt :: Int -> Int
fibInt n
  | n <= 1 = n
  | otherwise = fibInt (n-1) + fibInt (n-2)

-- Suite de Fibonacci avec Integer (illimité)
fibInteger :: Integer -> Integer
fibInteger n
  | n <= 1 = fromIntegral n
  | otherwise = fibInteger (n-1) + fibInteger (n-2)

-- | Fonction de test des limites
testIntLimits :: IO ()
testIntLimits = do
  putStrLn "=== COMPARAISON INT VS INTEGER ==="
  putStrLn ""
  
  -- Affichage des valeurs de base
  putStrLn "🔢 Valeurs de test :"
  putStrLn $ "• smallNumber (Int) = " ++ show smallNumber
  putStrLn $ "• bigNumber (Integer) = " ++ show bigNumber
  putStrLn $ "• Longueur bigNumber : " ++ show (length (show bigNumber)) ++ " chiffres"
  putStrLn ""
  
  -- Démonstration des limites d'Int
  putStrLn "📏 Limites approximatives d'Int (système 64 bits) :"
  putStrLn $ "• Maximum Int ≈ " ++ show maxIntApprox
  putStrLn $ "• Minimum Int ≈ " ++ show minIntApprox
  putStrLn $ "• Plage : environ " ++ show (fromIntegral maxIntApprox / 10^18 :: Double) ++ " × 10^18"
  putStrLn ""

-- | Fonction principale pour démontrer les différences
main :: IO ()
main = do
  testIntLimits
  
  -- =========================================================================
  -- SECTION 1: COMPARAISON DES CAPACITÉS
  -- =========================================================================
  putStrLn "=== COMPARAISON DES CAPACITÉS ==="
  putStrLn ""
  
  putStrLn "🚀 Nombre énorme avec Integer :"
  putStrLn $ "• 2^1000 = " ++ take 50 (show hugeNumber) ++ "... (" ++ 
             show (length (show hugeNumber)) ++ " chiffres au total)"
  putStrLn "  → Impossible à représenter avec Int !"
  putStrLn ""
  
  -- =========================================================================
  -- SECTION 2: TESTS DE FACTORIELLES
  -- =========================================================================
  putStrLn "=== TESTS DE FACTORIELLES ==="
  putStrLn ""
  
  putStrLn "🔢 Factorielles avec Int (attention aux débordements) :"
  let intFactorials = [1, 5, 10, 15, 20]
  mapM_ (\n -> do
    let result = factorialInt n
    putStrLn $ "• " ++ show n ++ "! = " ++ show result ++ " (Int)"
    ) intFactorials
  putStrLn "  ⚠️  Au-delà de 20!, les résultats peuvent déborder"
  putStrLn ""
  
  putStrLn "🔢 Factorielles avec Integer (précision illimitée) :"
  let integerFactorials = [1, 5, 10, 20, 30, 50]
  mapM_ (\n -> do
    let result = factorialInteger (fromIntegral n)
    let resultStr = show result
    if length resultStr > 50
      then putStrLn $ "• " ++ show n ++ "! = " ++ take 30 resultStr ++ "... (" ++ 
                      show (length resultStr) ++ " chiffres)"
      else putStrLn $ "• " ++ show n ++ "! = " ++ resultStr ++ " (Integer)"
    ) integerFactorials
  putStrLn ""
  
  -- =========================================================================
  -- SECTION 3: TESTS DE PUISSANCES
  -- =========================================================================
  putStrLn "=== TESTS DE PUISSANCES ==="
  putStrLn ""
  
  putStrLn "⚡ Puissances avec Int (limité) :"
  let intPowers = [(2, 10), (2, 30), (2, 60)]
  mapM_ (\(base, exp) -> do
    let result = powerInt base exp
    putStrLn $ "• " ++ show base ++ "^" ++ show exp ++ " = " ++ show result ++ " (Int)"
    ) intPowers
  putStrLn "  ⚠️  2^60 et plus peuvent déborder"
  putStrLn ""
  
  putStrLn "⚡ Puissances avec Integer (illimité) :"
  let integerPowers = [(2, 10), (2, 100), (2, 200), (10, 50)]
  mapM_ (\(base, exp) -> do
    let result = powerInteger (fromIntegral base) (fromIntegral exp)
    let resultStr = show result
    if length resultStr > 40
      then putStrLn $ "• " ++ show base ++ "^" ++ show exp ++ " = " ++ take 20 resultStr ++ "... (" ++ 
                      show (length resultStr) ++ " chiffres)"
      else putStrLn $ "• " ++ show base ++ "^" ++ show exp ++ " = " ++ resultStr ++ " (Integer)"
    ) integerPowers
  putStrLn ""
  
  -- =========================================================================
  -- SECTION 4: CONVERSION ENTRE TYPES
  -- =========================================================================
  putStrLn "=== CONVERSIONS ENTRE INT ET INTEGER ==="
  putStrLn ""
  
  let testInt = 12345 :: Int
  let convertedToInteger = intToInteger testInt
  let convertedBackToInt = integerToInt convertedToInteger
  
  putStrLn $ "• Int original : " ++ show testInt
  putStrLn $ "• Converti en Integer : " ++ show convertedToInteger
  putStrLn $ "• Reconverti en Int : " ++ show convertedBackToInt
  putStrLn "  → Conversion Int → Integer → Int sans perte"
  putStrLn ""
  
  -- Test avec un grand Integer
  let bigInt = 999999999999999999999 :: Integer
  let truncatedInt = integerToInt bigInt
  putStrLn $ "• Grand Integer : " ++ show bigInt
  putStrLn $ "• Converti en Int : " ++ show truncatedInt
  putStrLn "  ⚠️  Perte de précision lors de la conversion !"
  putStrLn ""
  
  -- =========================================================================
  -- SECTION 5: ANALYSE DES PERFORMANCES
  -- =========================================================================
  putStrLn "=== ANALYSE DES PERFORMANCES ==="
  putStrLn ""
  
  putStrLn "🏃 Performance Int vs Integer :"
  putStrLn "• Int : Plus rapide pour les calculs simples"
  putStrLn "• Integer : Plus lent mais plus sûr pour les grands nombres"
  putStrLn ""
  
  -- Fibonacci avec des petits nombres
  putStrLn "🔢 Fibonacci (petits nombres) :"
  let fibTests = [10, 15, 20]
  mapM_ (\n -> do
    let resultInt = fibInt n
    let resultInteger = fibInteger (fromIntegral n)
    putStrLn $ "• fib(" ++ show n ++ ") = " ++ show resultInt ++ " (Int) = " ++ 
               show resultInteger ++ " (Integer)"
    ) fibTests
  putStrLn ""
  
  -- =========================================================================
  -- SECTION 6: RECOMMANDATIONS D'USAGE
  -- =========================================================================
  putStrLn "=== RECOMMANDATIONS D'USAGE ==="
  putStrLn ""
  putStrLn "🎯 Quand utiliser Int :"
  putStrLn "• ✅ Calculs simples et rapides"
  putStrLn "• ✅ Indices de tableaux, compteurs"
  putStrLn "• ✅ Valeurs garanties petites"
  putStrLn "• ✅ Performance critique"
  putStrLn ""
  putStrLn "🎯 Quand utiliser Integer :"
  putStrLn "• ✅ Calculs mathématiques complexes"
  putStrLn "• ✅ Factorielles, puissances élevées"
  putStrLn "• ✅ Cryptographie, grands nombres"
  putStrLn "• ✅ Precision absolue requise"
  putStrLn ""
  putStrLn "⚖️  Compromis :"
  putStrLn "• Int = Vitesse vs Limitation"
  putStrLn "• Integer = Précision vs Performance"
  putStrLn ""
  
  putStrLn "✅ Choisissez le bon type selon vos besoins !"
```

### 🔍 Instructions GHCi

```ghci
-- Dans GHCi, testez les différences Int vs Integer :

-- Vérification des types
:t (42 :: Int)
-- (42 :: Int) :: Int

:t (42 :: Integer)  
-- (42 :: Integer) :: Integer

-- Test des limites
let maxInt = 9223372036854775807 :: Int
maxInt
-- 9223372036854775807

let overflowTest = maxInt + 1 :: Int
overflowTest
-- -9223372036854775808 (débordement !)

-- Avec Integer, pas de débordement
let bigInteger = 9223372036854775807 :: Integer
let noProblem = bigInteger + 1 :: Integer  
noProblem
-- 9223372036854775808

-- Calculs avec grands nombres
let factorial20Int = product [1..20] :: Int
factorial20Int
-- 2432902008176640000

let factorial20Integer = product [1..20] :: Integer
factorial20Integer  
-- 2432902008176640000

let factorial50Integer = product [1..50] :: Integer
factorial50Integer
-- 30414093201713378043612608166064768844377641568960512000000000000

-- Conversions
fromIntegral (42 :: Int) :: Integer
-- 42

fromIntegral (999999999999999999999 :: Integer) :: Int
-- -1486618624 (troncature/débordement)

-- Types automatiques
:t 42
-- 42 :: Num a => a

:t (42 + 1)
-- (42 + 1) :: Num a => a
```

### 📊 Sortie Attendue
```
=== COMPARAISON INT VS INTEGER ===

🔢 Valeurs de test :
• smallNumber (Int) = 4611686018427387904
• bigNumber (Integer) = 170141183460469231731687303715884105728
• Longueur bigNumber : 39 chiffres

📏 Limites approximatives d'Int (système 64 bits) :
• Maximum Int ≈ 9223372036854775807
• Minimum Int ≈ -9223372036854775808
• Plage : environ 9.223 × 10^18

=== COMPARAISON DES CAPACITÉS ===

🚀 Nombre énorme avec Integer :
• 2^1000 = 10715086071862673209484250490600018105614... (302 chiffres au total)
  → Impossible à représenter avec Int !

=== TESTS DE FACTORIELLES ===

🔢 Factorielles avec Int (attention aux débordements) :
• 1! = 1 (Int)
• 5! = 120 (Int)
• 10! = 3628800 (Int)
• 15! = 1307674368000 (Int)
• 20! = 2432902008176640000 (Int)
  ⚠️  Au-delà de 20!, les résultats peuvent déborder

🔢 Factorielles avec Integer (précision illimitée) :
• 1! = 1 (Integer)
• 5! = 120 (Integer)
• 10! = 3628800 (Integer)
• 20! = 2432902008176640000 (Integer)
• 30! = 265252859812191058636308480000000... (33 chiffres)
• 50! = 304140932017133780436126081660647... (65 chiffres)

=== RECOMMANDATIONS D'USAGE ===

🎯 Quand utiliser Int :
• ✅ Calculs simples et rapides
• ✅ Indices de tableaux, compteurs
• ✅ Valeurs garanties petites
• ✅ Performance critique

🎯 Quand utiliser Integer :
• ✅ Calculs mathématiques complexes
• ✅ Factorielles, puissances élevées
• ✅ Cryptographie, grands nombres
• ✅ Precision absolue requise

⚖️  Compromis :
• Int = Vitesse vs Limitation
• Integer = Précision vs Performance

✅ Choisissez le bon type selon vos besoins !
```

### 🚀 Points Importants à Retenir
1. **Int = Taille fixe** : Limité à ~9 × 10^18, mais très rapide
2. **Integer = Précision arbitraire** : Pas de limite, mais plus lent
3. **Débordement d'Int** : Peut causer des erreurs silencieuses dangereuses
4. **Conversion** : `fromIntegral` pour convertir entre les types
5. **Choix selon contexte** : Performance vs Précision selon les besoins

### 🧠 Explication Détaillée - Débordement d'Int

Le débordement d'Int est un piège dangereux qu'Integer évite complètement :

```haskell
-- DÉBORDEMENT DANGEREUX AVEC INT
let maxInt = 9223372036854775807 :: Int  -- Maximum
let overflow = maxInt + 1 :: Int          -- Déborde vers le minimum !
-- overflow = -9223372036854775808       -- Résultat incorrect !

-- SÉCURITÉ AVEC INTEGER
let maxInteger = 9223372036854775807 :: Integer
let safe = maxInteger + 1 :: Integer      -- Pas de problème
-- safe = 9223372036854775808            -- Résultat correct

-- C'est pourquoi Integer est préférable pour :
-- - Calculs financiers précis
-- - Algorithmes cryptographiques  
-- - Factorielles et combinatoires
-- - Tout calcul où la précision est critique
```
