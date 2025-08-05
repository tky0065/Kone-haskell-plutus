## ✅ HC1T4 - Traitement de Données de Joueurs

### 🎯 Objectif
Maîtriser le **traitement de données structurées** en Haskell avec des tuples, apprendre les fonctions de **tri**, d'**extraction** et de **composition** pour manipuler des listes complexes.

### 📝 Concepts Clés
- **Tuples** : Structures de données hétérogènes `(String, Int)`
- **Compréhension de listes** : Syntaxe `[x | x <- liste, condition]`
- **Tri de données** : `sortBy` avec des critères personnalisés
- **Composition de fonctions** : Chaîner plusieurs transformations
- **Pattern matching** : Déconstruction des tuples avec `(nom, score)`

### 💻 Code Complet

```haskell
-- Main.hs
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))

-- | Données d'exemple : liste de tuples (nom, score)
-- Chaque tuple représente un joueur avec son nom et son score
players :: [(String, Int)]
players = [
    ("Alice", 30), 
    ("Bob", 25), 
    ("Carol", 40), 
    ("David", 35), 
    ("Eve", 20),
    ("Frank", 45),
    ("Grace", 38),
    ("Henry", 22),
    ("Iris", 42),
    ("Jack", 28)
  ]

-- | Étape 1 : Extraire seulement les noms des joueurs
-- Utilise la compréhension de listes avec pattern matching
extractPlayers :: [(String, Int)] -> [String]
extractPlayers xs = [name | (name, _) <- xs]

-- | Version alternative avec map et fst
extractPlayersMap :: [(String, Int)] -> [String]
extractPlayersMap = map fst

-- | Étape 2 : Trier par score en ordre décroissant (du plus haut au plus bas)
-- Down() inverse l'ordre naturel pour avoir décroissant
sortByScore :: [(String, Int)] -> [(String, Int)]
sortByScore = sortBy (comparing (Down . snd))

-- | Version alternative : tri croissant (du plus bas au plus haut)
sortByScoreAsc :: [(String, Int)] -> [(String, Int)]
sortByScoreAsc = sortBy (comparing snd)

-- | Étape 3 : Prendre les N premiers éléments
-- Fonction générique pour prendre les n meilleurs
topN :: Int -> [(String, Int)] -> [(String, Int)]
topN n xs = take n xs

-- | Étape 4 : Prendre spécifiquement les 3 meilleurs
topThree :: [(String, Int)] -> [(String, Int)]
topThree = topN 3

-- | Composition finale : obtenir les noms des 3 meilleurs joueurs
-- Combine toutes les étapes en une seule fonction
getTopThreePlayers :: [(String, Int)] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

-- | Version détaillée étape par étape pour la démonstration
getTopThreePlayersDetailed :: [(String, Int)] -> ([String], [(String, Int)], [(String, Int)])
getTopThreePlayersDetailed players = 
  let sorted = sortByScore players
      top3 = topThree sorted
      names = extractPlayers top3
  in (names, top3, sorted)

-- | Filtrer les joueurs par score minimum
playersAboveScore :: Int -> [(String, Int)] -> [(String, Int)]
playersAboveScore minScore = filter (\(_, score) -> score > minScore)

-- | Calculer les statistiques des scores
calculateStats :: [(String, Int)] -> (Int, Int, Double, Int)
calculateStats players = 
  let scores = map snd players
      minScore = minimum scores
      maxScore = maximum scores
      avgScore = fromIntegral (sum scores) / fromIntegral (length scores)
      totalPlayers = length players
  in (minScore, maxScore, avgScore, totalPlayers)

-- | Fonction principale pour démontrer toutes les fonctionnalités
main :: IO ()
main = do
  putStrLn "=== TRAITEMENT DE DONNÉES DE JOUEURS ==="
  putStrLn ""
  
  -- Affichage de la liste originale
  putStrLn "--- Liste originale des joueurs ---"
  mapM_ (\(name, score) -> 
    putStrLn ("  " ++ name ++ " : " ++ show score ++ " points")) players
  putStrLn ""
  
  -- Étape 1 : Extraction des noms
  putStrLn "--- Étape 1 : Extraction des noms ---"
  let allNames = extractPlayers players
  putStrLn ("Noms extraits : " ++ show allNames)
  putStrLn ("Nombre de joueurs : " ++ show (length allNames))
  putStrLn ""
  
  -- Étape 2 : Tri par score (décroissant)
  putStrLn "--- Étape 2 : Tri par score décroissant ---"
  let sorted = sortByScore players
  mapM_ (\(name, score) -> 
    putStrLn ("  " ++ name ++ " : " ++ show score ++ " points")) sorted
  putStrLn ""
  
  -- Étape 3 : Top 3
  putStrLn "--- Étape 3 : Top 3 des joueurs ---"
  let top3 = topThree sorted
  mapM_ (\(rank, (name, score)) -> 
    putStrLn ("  " ++ show rank ++ ". " ++ name ++ " : " ++ show score ++ " points")) 
    (zip [1..] top3)
  putStrLn ""
  
  -- Étape 4 : Composition finale
  putStrLn "--- Étape 4 : Noms du top 3 (composition) ---"
  let topNames = getTopThreePlayers players
  putStrLn ("Top 3 noms : " ++ show topNames)
  putStrLn ""
  
  -- Démonstration des étapes détaillées
  putStrLn "--- Processus détaillé ---"
  let (names, top3Players, sortedPlayers) = getTopThreePlayersDetailed players
  putStrLn ("1. Tri effectué sur " ++ show (length sortedPlayers) ++ " joueurs")
  putStrLn ("2. Top 3 sélectionné : " ++ show top3Players)
  putStrLn ("3. Noms extraits : " ++ show names)
  putStrLn ""
  
  -- Filtrage par score
  putStrLn "--- Filtrage par score ---"
  let highScorers = playersAboveScore 30 players
  putStrLn ("Joueurs avec > 30 points :")
  mapM_ (\(name, score) -> 
    putStrLn ("  " ++ name ++ " : " ++ show score ++ " points")) highScorers
  putStrLn ""
  
  -- Statistiques
  putStrLn "--- Statistiques ---"
  let (minScore, maxScore, avgScore, totalPlayers) = calculateStats players
  putStrLn ("Score minimum : " ++ show minScore)
  putStrLn ("Score maximum : " ++ show maxScore)
  putStrLn ("Score moyen : " ++ show (round avgScore))
  putStrLn ("Nombre total de joueurs : " ++ show totalPlayers)
```

### 🔍 Explication Détaillée

#### 1. **Structure des Données : Tuples**
```haskell
players :: [(String, Int)]
players = [("Alice", 30), ("Bob", 25), ...]
```
- **Tuple `(String, Int)`** : Paire nom-score
- **Liste de tuples** : Collection de joueurs
- **Types hétérogènes** : String ET Int dans la même structure

#### 2. **Compréhension de Listes avec Pattern Matching**
```haskell
extractPlayers xs = [name | (name, _) <- xs]
```
- **`(name, _)`** : Déconstruit le tuple, ignore le score avec `_`
- **`<- xs`** : Parcourt chaque élément de la liste
- **`[name | ...]`** : Construit une nouvelle liste avec seulement les noms

#### 3. **Tri Personnalisé**
```haskell
sortBy (comparing (Down . snd))
```
- **`snd`** : Extrait le deuxième élément du tuple (le score)
- **`Down`** : Inverse l'ordre (décroissant au lieu de croissant)
- **`comparing`** : Crée une fonction de comparaison
- **`sortBy`** : Trie selon la fonction de comparaison fournie

#### 4. **Composition de Fonctions**
```haskell
getTopThreePlayers = extractPlayers . topThree . sortByScore
```
- **Lecture de droite à gauche** :
  1. `sortByScore` : Trie par score décroissant
  2. `topThree` : Prend les 3 premiers
  3. `extractPlayers` : Extrait seulement les noms
- **Une seule fonction** qui combine toute la logique

#### 5. **Pattern Matching dans les Lambdas**
```haskell
filter (\(_, score) -> score > minScore)
```
- **`\(_, score)`** : Lambda qui déconstruit le tuple
- **`_`** : Ignore le nom, utilise seulement le score
- **Plus lisible** que `\tuple -> snd tuple > minScore`

### 📊 Sortie Attendue
```
=== TRAITEMENT DE DONNÉES DE JOUEURS ===

--- Liste originale des joueurs ---
  Alice : 30 points
  Bob : 25 points
  Carol : 40 points
  David : 35 points
  Eve : 20 points
  Frank : 45 points
  Grace : 38 points
  Henry : 22 points
  Iris : 42 points
  Jack : 28 points

--- Étape 1 : Extraction des noms ---
Noms extraits : ["Alice","Bob","Carol","David","Eve","Frank","Grace","Henry","Iris","Jack"]
Nombre de joueurs : 10

--- Étape 2 : Tri par score décroissant ---
  Frank : 45 points
  Iris : 42 points
  Carol : 40 points
  Grace : 38 points
  David : 35 points
  Alice : 30 points
  Jack : 28 points
  Bob : 25 points
  Henry : 22 points
  Eve : 20 points

--- Étape 3 : Top 3 des joueurs ---
  1. Frank : 45 points
  2. Iris : 42 points
  3. Carol : 40 points

--- Étape 4 : Noms du top 3 (composition) ---
Top 3 noms : ["Frank","Iris","Carol"]

--- Processus détaillé ---
1. Tri effectué sur 10 joueurs
2. Top 3 sélectionné : [("Frank",45),("Iris",42),("Carol",40)]
3. Noms extraits : ["Frank","Iris","Carol"]

--- Filtrage par score ---
Joueurs avec > 30 points :
  Carol : 40 points
  David : 35 points
  Frank : 45 points
  Grace : 38 points
  Iris : 42 points

--- Statistiques ---
Score minimum : 20
Score maximum : 45
Score moyen : 33
Nombre total de joueurs : 10
```

### 🚀 Points Importants à Retenir
1. **Tuples** : Structures simples pour grouper des données hétérogènes
2. **Pattern matching** : Déconstruction élégante des structures
3. **Composition** : Combiner plusieurs transformations en une seule fonction
4. **Tri personnalisé** : `sortBy` avec `comparing` pour des critères complexes
5. **Pipeline de données** : Transformer les données étape par étape
