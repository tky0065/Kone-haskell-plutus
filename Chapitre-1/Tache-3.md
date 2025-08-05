## ✅ HC1T3 - Vérifier si un Nombre est Supérieur à 18

### 🎯 Objectif
Apprendre les **fonctions booléennes** et les **opérateurs de comparaison** en Haskell, ainsi que l'utilisation des fonctions d'ordre supérieur comme `filter` et `mapM_` pour traiter des listes.

### 📝 Concepts Clés
- **Fonctions booléennes** : Retournent `True` ou `False`
- **Opérateurs de comparaison** : `>`, `<`, `>=`, `<=`, `==`, `/=`
- **Traitement de listes** : `filter`, `map`, `mapM_`
- **Fonctions d'ordre supérieur** : Fonctions qui prennent d'autres fonctions en paramètre

### 💻 Code Complet

```haskell
-- Main.hs

-- | Vérifie si un nombre est supérieur à 18
-- Fonction booléenne pure : retourne True ou False
greaterThan18 :: Int -> Bool
greaterThan18 x = x > 18

-- | Vérifie si un nombre est majeur (>= 18)
-- Version alternative pour inclure exactement 18
isAdult :: Int -> Bool
isAdult x = x >= 18

-- | Vérifie si un nombre est mineur (< 18)
-- Fonction complémentaire à isAdult
isMinor :: Int -> Bool
isMinor x = x < 18

-- | Liste d'âges de test avec contexte
testAges :: [(Int, String)]
testAges = [
    (15, "Sophie"),
    (18, "Marc"),
    (20, "Julie"),
    (25, "Pierre"),
    (12, "Emma"),
    (17, "Thomas"),
    (21, "Lisa"),
    (16, "Alex")
  ]

-- | Fonction principale pour démontrer les concepts
main :: IO ()
main = do
  putStrLn "=== VÉRIFICATION DES ÂGES ==="
  
  -- Extraction des âges pour les tests
  let ages = map fst testAges
  putStrLn ("Âges à tester : " ++ show ages)
  putStrLn ""
  
  -- Test de la fonction greaterThan18
  putStrLn "--- Test : greaterThan18 (strictement > 18) ---"
  mapM_ (\(age, name) -> do
    let result = greaterThan18 age
    putStrLn (name ++ " (" ++ show age ++ " ans) > 18 ? " ++ show result)
    ) testAges
  
  putStrLn ""
  
  -- Test de la fonction isAdult (>= 18)
  putStrLn "--- Test : isAdult (majeur, >= 18 ans) ---"
  mapM_ (\(age, name) -> do
    let result = isAdult age
    let status = if result then "MAJEUR" else "MINEUR"
    putStrLn (name ++ " (" ++ show age ++ " ans) : " ++ status)
    ) testAges
  
  putStrLn ""
  
  -- Utilisation de filter pour séparer les groupes
  putStrLn "--- Utilisation de filter ---"
  let adultAges = filter isAdult ages
  let minorAges = filter isMinor ages
  
  putStrLn ("Majeurs (>= 18) : " ++ show adultAges)
  putStrLn ("Mineurs (< 18) : " ++ show minorAges)
  putStrLn ("Nombre de majeurs : " ++ show (length adultAges))
  putStrLn ("Nombre de mineurs : " ++ show (length minorAges))
  
  putStrLn ""
  
  -- Statistiques détaillées
  putStrLn "--- Statistiques ---"
  let strictlyOver18 = length (filter greaterThan18 ages)
  let exactlyEighteen = length (filter (== 18) ages)
  let totalAdults = length (filter isAdult ages)
  
  putStrLn ("Personnes > 18 ans : " ++ show strictlyOver18)
  putStrLn ("Personnes = 18 ans : " ++ show exactlyEighteen)  
  putStrLn ("Total majeurs (>= 18) : " ++ show totalAdults)
```

### 🔍 Explication Détaillée

#### 1. **Fonctions Booléennes**
```haskell
greaterThan18 :: Int -> Bool
greaterThan18 x = x > 18
```
- **Type de retour `Bool`** : Seulement `True` ou `False`
- **Opérateur `>`** : Comparaison "strictement supérieur"
- **Fonction pure** : Même entrée → même résultat booléen

#### 2. **Différence entre `>` et `>=`**
```haskell
greaterThan18 18 = False  -- 18 > 18 est False
isAdult 18 = True         -- 18 >= 18 est True
```

#### 3. **Fonction `filter`**
```haskell
filter greaterThan18 ages
```
- **Prend une fonction booléenne** et une liste
- **Retourne une nouvelle liste** avec les éléments satisfaisant la condition
- **Ne modifie pas** la liste originale

#### 4. **Fonction `mapM_`**
- **Applique une action d'E/S** à chaque élément de la liste
- **`_` signifie** qu'on ignore le résultat (juste pour l'affichage)

### 📊 Sortie Attendue
```
=== VÉRIFICATION DES ÂGES ===
Âges à tester : [15,18,20,25,12,17,21,16]

--- Test : greaterThan18 (strictement > 18) ---
Sophie (15 ans) > 18 ? False
Marc (18 ans) > 18 ? False
Julie (20 ans) > 18 ? True
Pierre (25 ans) > 18 ? True

--- Test : isAdult (majeur, >= 18 ans) ---
Sophie (15 ans) : MINEUR
Marc (18 ans) : MAJEUR
Julie (20 ans) : MAJEUR
Pierre (25 ans) : MAJEUR

--- Utilisation de filter ---
Majeurs (>= 18) : [18,20,25,21]
Mineurs (< 18) : [15,12,17,16]
Nombre de majeurs : 4
Nombre de mineurs : 4

--- Statistiques ---
Personnes > 18 ans : 3
Personnes = 18 ans : 1
Total majeurs (>= 18) : 4
```

### 🚀 Points Importants à Retenir
1. **Fonctions booléennes** : Outils essentiels pour les conditions
2. **`filter`** : Sélectionne les éléments qui satisfont une condition
3. **Immutabilité** : Les listes originales ne sont jamais modifiées
4. **Différence `>` vs `>=`** : Important pour la logique métier
