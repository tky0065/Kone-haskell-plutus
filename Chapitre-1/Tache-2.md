## ✅ HC1T2 - Fonction Pure : Aire d'un Cercle

### 🎯 Objectif
Comprendre les **fonctions pures** en Haskell à travers le calcul de l'aire d'un cercle. Une fonction pure produit toujours le même résultat pour les mêmes entrées et n'a aucun effet de bord.

### 📝 Concepts Clés
- **Fonction pure** : Déterministe et sans effet de bord
- **Immutabilité** : Les valeurs ne changent jamais
- **Prédictibilité** : Même entrée = même sortie, toujours
- **Transparence référentielle** : On peut remplacer un appel de fonction par son résultat

### 💻 Code Complet

```haskell
-- Main.hs
import Text.Printf (printf)

-- | Calcule l'aire d'un cercle à partir de son rayon
-- Formule mathématique : A = π × r²
-- Cette fonction est PURE : même rayon → même aire, toujours
circleArea :: Float -> Float
circleArea r = pi * r * r

-- | Version alternative avec la fonction power
-- Démontre différentes façons d'exprimer la même opération
circleAreaPower :: Float -> Float
circleAreaPower r = pi * (r ** 2)

-- | Calcule le périmètre d'un cercle (pour comparaison)
-- Formule : P = 2 × π × r
circlePerimeter :: Float -> Float
circlePerimeter r = 2 * pi * r

-- | Calcule les propriétés complètes d'un cercle
-- Retourne un tuple (aire, périmètre)
circleProperties :: Float -> (Float, Float)
circleProperties r = (circleArea r, circlePerimeter r)

-- | Liste de rayons de test avec leurs descriptions
testCircles :: [(Float, String)]
testCircles = [
    (1.0, "Cercle unitaire"),
    (2.0, "Petit cercle"),
    (5.0, "Cercle moyen"),
    (10.0, "Grand cercle"),
    (0.5, "Très petit cercle"),
    (100.0, "Très grand cercle")
  ]

-- | Fonction principale pour tester nos calculs
main :: IO ()
main = do
  putStrLn "=== CALCUL DE L'AIRE D'UN CERCLE ==="
  putStrLn ("Valeur de π utilisée : " ++ show pi)
  putStrLn ""
  
  -- Test avec des cercles de différentes tailles
  putStrLn "--- Aires calculées ---"
  mapM_ (\(radius, desc) -> do
    let area = circleArea radius
    printf "Rayon %.1f (%s) : Aire = %.6f\n" radius desc area
    ) testCircles
  
  putStrLn ""
  
  -- Comparaison des deux méthodes de calcul
  putStrLn "--- Vérification : deux méthodes de calcul ---"
  let testRadius = 3.0
  let area1 = circleArea testRadius
  let area2 = circleAreaPower testRadius
  printf "Méthode 1 (r * r) : %.10f\n" area1
  printf "Méthode 2 (r ** 2) : %.10f\n" area2
  printf "Différence : %.2e\n" (abs (area1 - area2))
  
  putStrLn ""
  
  -- Propriétés complètes des cercles
  putStrLn "--- Propriétés complètes ---"
  mapM_ (\(radius, desc) -> do
    let (area, perimeter) = circleProperties radius
    printf "%s (r=%.1f) : Aire=%.2f, Périmètre=%.2f\n" desc radius area perimeter
    ) (take 4 testCircles)
  
  putStrLn ""
  
  -- Démonstration de la pureté
  putStrLn "--- Démonstration de la pureté ---"
  let r = 2.5
  putStrLn ("Appel 1 : circleArea " ++ show r ++ " = " ++ show (circleArea r))
  putStrLn ("Appel 2 : circleArea " ++ show r ++ " = " ++ show (circleArea r))
  putStrLn ("Appel 3 : circleArea " ++ show r ++ " = " ++ show (circleArea r))
  putStrLn "→ Même résultat à chaque fois : c'est une fonction PURE !"
  
  -- Calculs avec précision
  putStrLn ""
  putStrLn "--- Calculs de précision ---"
  let preciseRadius = 1.0
  let preciseArea = circleArea preciseRadius
  printf "Aire du cercle unitaire : %.15f\n" preciseArea
  printf "Valeur théorique π : %.15f\n" pi
  printf "Différence : %.2e\n" (abs (preciseArea - pi))
```

### 🔍 Explication Détaillée

#### 1. **Qu'est-ce qu'une Fonction Pure ?**
```haskell
circleArea :: Float -> Float
circleArea r = pi * r * r
```
- **Déterministe** : `circleArea 2.0` retourne TOUJOURS la même valeur
- **Sans effet de bord** : Ne modifie rien en dehors de son calcul
- **Dépend uniquement des paramètres** : Seul `r` influence le résultat
- **Transparence référentielle** : On peut remplacer `circleArea 2.0` par `12.566371`

#### 2. **Formule Mathématique**
- **Aire d'un cercle** : A = π × r²
- **π (pi)** : Constante prédéfinie en Haskell ≈ 3.141592653589793
- **r²** : On peut utiliser `r * r` ou `r ** 2`

#### 3. **Avantages des Fonctions Pures**
- **Testabilité** : Facile à tester, résultats prévisibles
- **Parallélisation** : Sûres à exécuter en parallèle
- **Mise en cache** : Possibilité de mémoriser les résultats
- **Raisonnement** : Plus facile de comprendre le comportement
- **Débogage** : Pas d'effets cachés

#### 4. **Comparaison avec Fonctions Impures**
```haskell
-- PURE : toujours le même résultat
circleArea 5.0 = 78.53982  -- toujours

-- IMPURE (exemple en d'autres langages) :
-- getRandomCircleArea() -- résultat différent à chaque appel
-- getCurrentTimeCircleArea() -- dépend de l'heure
```

### 📊 Sortie Attendue
```
=== CALCUL DE L'AIRE D'UN CERCLE ===
Valeur de π utilisée : 3.1415927

--- Aires calculées ---
Rayon 1.0 (Cercle unitaire) : Aire = 3.141593
Rayon 2.0 (Petit cercle) : Aire = 12.566371
Rayon 5.0 (Cercle moyen) : Aire = 78.539818
Rayon 10.0 (Grand cercle) : Aire = 314.159271
Rayon 0.5 (Très petit cercle) : Aire = 0.785398
Rayon 100.0 (Très grand cercle) : Aire = 31415.927734

--- Vérification : deux méthodes de calcul ---
Méthode 1 (r * r) : 28.2743339539
Méthode 2 (r ** 2) : 28.2743339539
Différence : 0.00e+00

--- Propriétés complètes ---
Cercle unitaire (r=1.0) : Aire=3.14, Périmètre=6.28
Petit cercle (r=2.0) : Aire=12.57, Périmètre=12.57
Cercle moyen (r=5.0) : Aire=78.54, Périmètre=31.42
Grand cercle (r=10.0) : Aire=314.16, Périmètre=62.83

--- Démonstration de la pureté ---
Appel 1 : circleArea 2.5 = 19.634954
Appel 2 : circleArea 2.5 = 19.634954
Appel 3 : circleArea 2.5 = 19.634954
→ Même résultat à chaque fois : c'est une fonction PURE !

--- Calculs de précision ---
Aire du cercle unitaire : 3.141592741012573
Valeur théorique π : 3.141592653589793
Différence : 8.74e-08
```

### 🚀 Points Importants à Retenir
1. **Pureté = Prévisibilité** : Mêmes entrées → mêmes sorties
2. **Aucun effet de bord** : La fonction ne modifie rien d'externe
3. **π est une constante** : Prédéfinie en Haskell avec haute précision
4. **Testabilité excellente** : Facile à vérifier et valider
5. **Base de la programmation fonctionnelle** : Les fonctions pures sont l'idéal
