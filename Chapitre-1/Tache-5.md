## ✅ HC1T5 - Listes Infinies et Paresse (Lazy Evaluation)

### 🎯 Objectif
Maîtriser la **paresse (lazy evaluation)** en Haskell à travers les listes infinies. Comprendre comment Haskell peut manipuler des structures théoriquement infinies sans consommer toute la mémoire grâce à l'évaluation à la demande.

### 📝 Concepts Clés
- **Paresse (Lazy Evaluation)** : Les expressions ne sont évaluées que quand leur résultat est nécessaire
- **Listes infinies** : Structures de données sans limite théorique définies avec [1..], [1,3..]
- **Thunks** : Expressions non-évaluées stockées en mémoire jusqu'à ce qu'elles soient demandées
- **Évaluation à la demande** : Seuls les éléments effectivement utilisés sont calculés et stockés

### 💻 Code Complet

```haskell
-- Main.hs

-- | Liste infinie des nombres naturels [1,2,3,4,...]
-- La notation [1..] génère paresseusement tous les entiers à partir de 1
-- Aucun calcul n'est fait tant qu'on n'accède pas aux éléments
infiniteNumbers :: [Int]
infiniteNumbers = [1..]

-- | Liste infinie des nombres impairs [1,3,5,7,...]
-- [1,3..] utilise la progression arithmétique : commence à 1, puis 3, pas de 2
infiniteOdds :: [Int]
infiniteOdds = [1,3..]

-- | Liste infinie des nombres pairs [2,4,6,8,...]
-- Même principe que les impairs mais commence à 2
infiniteEvens :: [Int]
infiniteEvens = [2,4..]

-- | Suite de Fibonacci infinie définie récursivement
-- Élégante définition : chaque nombre = somme des deux précédents
-- infiniteFibs se référence à lui-même grâce à la paresse
infiniteFibs :: [Int]
infiniteFibs = 0 : 1 : zipWith (+) infiniteFibs (tail infiniteFibs)

-- | Puissances de 2 infinies [1,2,4,8,16,32,...]
-- Compréhension de liste : [2^n | n <- [0..]]
-- Calcule 2^n pour chaque n dans [0,1,2,3,...]
powersOfTwo :: [Int]
powersOfTwo = [2^n | n <- [0..]]

-- | Version alternative des puissances de 2 avec récursion
-- Plus efficace : multiplie par 2 au lieu de calculer 2^n à chaque fois
powersOfTwoRec :: [Int]
powersOfTwoRec = 1 : map (*2) powersOfTwoRec

-- | Nombres premiers par le crible d'Ératosthène
-- Algorithme antique : prendre le premier, éliminer ses multiples, répéter
-- Fonctionne sur une liste infinie grâce à la paresse !
primes :: [Int]
primes = sieve [2..]
  where
    -- Fonction locale qui implémente le crible
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- | Fonction générique pour créer des listes infinies par itération
-- iterate' f x génère [x, f(x), f(f(x)), f(f(f(x))), ...]
-- Redéfinition de la fonction iterate du Prelude à des fins pédagogiques
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

-- | Extrait les n premiers éléments d'une liste infinie
-- Encapsule l'opération take n pour plus de clarté
takeN :: Int -> [a] -> [a]
takeN n = take n

-- | Exemples de listes infinies avec leurs descriptions
infiniteExamples :: [(String, [Int])]
infiniteExamples = [
    ("Nombres naturels (10 premiers)", take 10 infiniteNumbers),
    ("Nombres impairs (10 premiers)", take 10 infiniteOdds),
    ("Nombres pairs (10 premiers)", take 10 infiniteEvens),
    ("Fibonacci (15 premiers)", take 15 infiniteFibs),
    ("Puissances de 2 (12 premiers)", take 12 powersOfTwo),
    ("Nombres premiers (20 premiers)", take 20 primes)
  ]

-- | Fonction principale pour démontrer les listes infinies
main :: IO ()
main = do
  putStrLn "=== LISTES INFINIES ET PARESSE EN HASKELL ==="
  putStrLn ""
  
  -- Démonstration des listes infinies de base
  putStrLn "=== Listes infinies fondamentales ==="
  mapM_ (\(description, list) -> do
    putStrLn $ "• " ++ description ++ " :"
    putStrLn $ "  " ++ show list
    putStrLn ""
    ) infiniteExamples
  
  -- Tests d'accès direct à des éléments éloignés
  putStrLn "=== Accès direct à des éléments éloignés ==="
  putStrLn $ "• 1000ème nombre naturel : " ++ show (infiniteNumbers !! 999)
  putStrLn "  → Calcul instantané grâce à la formule directe"
  putStrLn ""
  putStrLn $ "• 50ème nombre de Fibonacci : " ++ show (infiniteFibs !! 49)
  putStrLn "  → Seuls les 50 premiers sont calculés"
  putStrLn ""
  putStrLn $ "• 100ème nombre premier : " ++ show (primes !! 99)
  putStrLn "  → Le crible génère seulement ce qui est nécessaire"
  putStrLn ""
  
  -- Démonstration de filtres sur listes infinies
  putStrLn "=== Filtres sur listes infinies ==="
  let evenFibs = filter even infiniteFibs
  putStrLn $ "• Premiers 8 nombres de Fibonacci pairs :"
  putStrLn $ "  " ++ show (take 8 evenFibs)
  putStrLn "  → filter even appliqué à une liste infinie"
  putStrLn ""
  
  let fibsUnder1000 = takeWhile (<1000) infiniteFibs
  putStrLn $ "• Nombres de Fibonacci < 1000 :"
  putStrLn $ "  " ++ show fibsUnder1000
  putStrLn $ "  → " ++ show (length fibsUnder1000) ++ " nombres trouvés"
  putStrLn ""
  
  -- Démonstration de la fonction iterate'
  putStrLn "=== Fonction iterate' générique ==="
  let doubleSeries = iterate' (*2) 1
  putStrLn $ "• Doublements successifs (départ: 1) :"
  putStrLn $ "  " ++ show (take 10 doubleSeries)
  putStrLn "  → 1, 1*2, 2*2, 4*2, 8*2, ..."
  putStrLn ""
  
  let incrementSeries = iterate' (+1) 0
  putStrLn $ "• Incréments successifs (départ: 0) :"
  putStrLn $ "  " ++ show (take 10 incrementSeries)
  putStrLn "  → Équivalent à [0..]"
  putStrLn ""
  
  let squareSeries = iterate' (\x -> x * x) 2
  putStrLn $ "• Carrés successifs (départ: 2) :"
  putStrLn $ "  " ++ show (take 5 squareSeries)
  putStrLn "  → 2, 2², (2²)², ((2²)²)², ..."
  putStrLn ""
  
  -- Comparaison de performance entre les deux versions des puissances
  putStrLn "=== Comparaison d'efficacité ==="
  putStrLn $ "• Puissances par compréhension : " ++ show (take 10 powersOfTwo)
  putStrLn $ "• Puissances par récursion      : " ++ show (take 10 powersOfTwoRec)
  putStrLn "  → Même résultat, mais la version récursive est plus efficace"
  putStrLn "  → Compréhension: calcule 2^n à chaque fois"
  putStrLn "  → Récursion: multiplie par 2 le précédent"
  putStrLn ""
  
  -- Démonstration de la paresse avec de grandes listes
  putStrLn "=== Démonstration de la paresse ==="
  putStrLn $ "• Éléments 100 à 105 de la liste infinie :"
  putStrLn $ "  " ++ show (take 6 (drop 99 infiniteNumbers))
  putStrLn "  → Instantané ! Pas de calcul des 99 premiers"
  putStrLn ""
  putStrLn $ "• Premier élément > 1000 dans Fibonacci :"
  let firstFibOver1000 = head $ dropWhile (<=1000) infiniteFibs
  putStrLn $ "  " ++ show firstFibOver1000
  putStrLn "  → dropWhile s'arrête dès qu'un élément dépasse 1000"
```

### 🔍 Instructions GHCi

```ghci
-- Dans GHCi, tapez ces commandes pour explorer les listes infinies :

-- Test des listes infinies de base
take 5 [1..]
-- [1,2,3,4,5]

take 5 [1,3..]
-- [1,3,5,7,9]

[1..] !! 999
-- 1000

-- Fibonacci avec récursion élégante
let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
take 10 fibs
-- [0,1,1,2,3,5,8,13,21,34]

-- Nombres premiers avec le crible
let primes = sieve [2..] where sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
take 10 primes
-- [2,3,5,7,11,13,17,19,23,29]

-- Fonction iterate
take 8 (iterate (*2) 1)
-- [1,2,4,8,16,32,64,128]

-- Filtres sur listes infinies
take 5 (filter even [1..])
-- [2,4,6,8,10]

takeWhile (<100) (iterate (*2) 1)
-- [1,2,4,8,16,32,64]
```

### 📊 Sortie Attendue
```
=== LISTES INFINIES ET PARESSE EN HASKELL ===

=== Listes infinies fondamentales ===
• Nombres naturels (10 premiers) :
  [1,2,3,4,5,6,7,8,9,10]

• Nombres impairs (10 premiers) :
  [1,3,5,7,9,11,13,15,17,19]

• Nombres pairs (10 premiers) :
  [2,4,6,8,10,12,14,16,18,20]

• Fibonacci (15 premiers) :
  [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377]

• Puissances de 2 (12 premiers) :
  [1,2,4,8,16,32,64,128,256,512,1024,2048]

• Nombres premiers (20 premiers) :
  [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71]

=== Accès direct à des éléments éloignés ===
• 1000ème nombre naturel : 1000
  → Calcul instantané grâce à la formule directe

• 50ème nombre de Fibonacci : 12586269025
  → Seuls les 50 premiers sont calculés

• 100ème nombre premier : 541
  → Le crible génère seulement ce qui est nécessaire

=== Filtres sur listes infinies ===
• Premiers 8 nombres de Fibonacci pairs :
  [0,2,8,34,144,610,2584,10946]
  → filter even appliqué à une liste infinie

• Nombres de Fibonacci < 1000 :
  [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610]
  → 16 nombres trouvés

=== Fonction iterate' générique ===
• Doublements successifs (départ: 1) :
  [1,2,4,8,16,32,64,128,256,512]
  → 1, 1*2, 2*2, 4*2, 8*2, ...

• Incréments successifs (départ: 0) :
  [0,1,2,3,4,5,6,7,8,9]
  → Équivalent à [0..]

• Carrés successifs (départ: 2) :
  [2,4,16,256,65536]
  → 2, 2², (2²)², ((2²)²)², ...

=== Comparaison d'efficacité ===
• Puissances par compréhension : [1,2,4,8,16,32,64,128,256,512]
• Puissances par récursion      : [1,2,4,8,16,32,64,128,256,512]
  → Même résultat, mais la version récursive est plus efficace
  → Compréhension: calcule 2^n à chaque fois
  → Récursion: multiplie par 2 le précédent

=== Démonstration de la paresse ===
• Éléments 100 à 105 de la liste infinie :
  [100,101,102,103,104,105]
  → Instantané ! Pas de calcul des 99 premiers

• Premier élément > 1000 dans Fibonacci :
  1597
  → dropWhile s'arrête dès qu'un élément dépasse 1000
```

### 🚀 Points Importants à Retenir
1. **Paresse = Efficacité** : Seuls les éléments demandés sont calculés et stockés
2. **Listes infinies possibles** : Grâce à l'évaluation différée d'Haskell
3. **Récursion élégante** : Fibonacci se définit naturellement avec zipWith
4. **Filtres sans coût** : On peut filtrer des listes infinies sans problème
5. **Performance optimale** : Pas de pré-calcul, pas de gaspillage mémoire

### 🧠 Explication Détaillée - Suite de Fibonacci

La définition `infiniteFibs = 0 : 1 : zipWith (+) infiniteFibs (tail infiniteFibs)` fonctionne ainsi :

```
infiniteFibs     = [0, 1,  1,  2,  3,  5,  8, 13, ...]
tail infiniteFibs= [   1,  1,  2,  3,  5,  8, 13, 21,...]
zipWith (+)      = [   1,  2,  3,  5,  8, 13, 21, 34,...]
Résultat final   = [0, 1,  1,  2,  3,  5,  8, 13, 21,...]
```

Chaque nouveau nombre de Fibonacci est calculé en additionnant les deux précédents, exactement comme dans la définition mathématique !
