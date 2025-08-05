## ✅ HC1T1 - Composition de Fonctions et Pureté

### 🎯 Objectif
Comprendre la **composition de fonctions** en Haskell avec l'opérateur `.` et maîtriser le concept de **pureté fonctionnelle**. Apprendre à chaîner des fonctions pour créer des transformations complexes.

### 📝 Concepts Clés
- **Composition** : L'opérateur `.` permet de chaîner des fonctions
- **Ordre d'évaluation** : De droite à gauche, comme en mathématiques
- **Pureté** : Même entrée produit toujours la même sortie
- **Immutabilité** : Les valeurs ne changent jamais

### 💻 Code Complet

```haskell
-- Main.hs

-- | Fonction qui double un nombre
-- Exemple de fonction pure : même entrée → même sortie
double :: Int -> Int
double x = x * 2

-- | Fonction qui incrémente un nombre
increment :: Int -> Int
increment x = x + 1

-- | Composition : double puis incrémente
-- (f . g) x = f (g x)
doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double

-- | Composition inverse : incrémente puis double
incrementThenDouble :: Int -> Int
incrementThenDouble = double . increment

-- | Fonction principale pour démontrer la composition
main :: IO ()
main = do
  putStrLn "=== COMPOSITION DE FONCTIONS EN HASKELL ==="
  putStrLn ""
  
  let testValue = 5
  putStrLn $ "=== Test avec la valeur " ++ show testValue ++ " ==="
  putStrLn $ "doubleThenIncrement " ++ show testValue ++ " : " ++ show (doubleThenIncrement testValue)
  putStrLn ""
  
  putStrLn "--- Détail du calcul ---"
  putStrLn $ "1. double " ++ show testValue ++ " = " ++ show (double testValue)
  putStrLn $ "2. increment " ++ show (double testValue) ++ " = " ++ show (increment (double testValue))
  putStrLn $ "Résultat final : " ++ show (doubleThenIncrement testValue)
  putStrLn ""
  
  putStrLn "--- Comparaison avec l'ordre inverse ---"
  putStrLn $ "incrementThenDouble " ++ show testValue ++ " : " ++ show (incrementThenDouble testValue)
  putStrLn $ "Calcul : increment " ++ show testValue ++ " = " ++ show (increment testValue) ++ ", puis double " ++ show (increment testValue) ++ " = " ++ show (double (increment testValue))
  putStrLn ""
  
  putStrLn "--- Tests avec différentes valeurs ---"
  let testValues = [0, 1, -3, 10]
  mapM_ (\val -> putStrLn $ "doubleThenIncrement " ++ show val ++ " = " ++ show (doubleThenIncrement val)) testValues
```

### 🔍 Instructions GHCi

```ghci
-- Dans GHCi, tapez ces commandes pour explorer la composition :

:t double
-- double :: Int -> Int

:t increment  
-- increment :: Int -> Int

:t (increment . double)
-- (increment . double) :: Int -> Int

-- Test des fonctions
double 5
-- 10

increment 5
-- 6

(increment . double) 5
-- 11

(double . increment) 5
-- 12
```

### 📊 Sortie Attendue
```
=== COMPOSITION DE FONCTIONS EN HASKELL ===

=== Test avec la valeur 5 ===
doubleThenIncrement 5 : 11

--- Détail du calcul ---
1. double 5 = 10
2. increment 10 = 11
Résultat final : 11

--- Comparaison avec l'ordre inverse ---
incrementThenDouble 5 : 12
Calcul : increment 5 = 6, puis double 6 = 12

--- Tests avec différentes valeurs ---
doubleThenIncrement 0 = 1
doubleThenIncrement 1 = 3
doubleThenIncrement -3 = -5
doubleThenIncrement 10 = 21
```

### 🚀 Points Importants à Retenir
1. **Composition = Chaînage** : L'opérateur `.` permet de chaîner des fonctions
2. **Ordre de droite à gauche** : `f . g` signifie "appliquer g, puis f"
3. **Pureté fonctionnelle** : Aucun effet de bord, résultat prévisible
4. **Élégance mathématique** : Reflète la composition mathématique standard
