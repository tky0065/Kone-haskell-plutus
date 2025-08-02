
## ✅ HC1T4 - Traitement de Données de Joueurs

### Code :

```haskell
-- Main.hs
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))

-- Données d'exemple
players :: [(String, Int)]
players = [("Alice", 30), ("Bob", 25), ("Carol", 40), ("David", 35), ("Eve", 20)]

-- Étape 1 : Extraire les noms
extractPlayers :: [(String, Int)] -> [String]
extractPlayers xs = [name | (name, _) <- xs]

-- Étape 2 : Trier par score décroissant
sortByScore :: [(String, Int)] -> [(String, Int)]
sortByScore = sortBy (comparing (Down . snd))

-- Étape 3 : Prendre les 3 meilleurs
topThree :: [(String, Int)] -> [(String, Int)]
topThree xs = take 3 xs

-- Étape 4 : Composition finale
getTopThreePlayers :: [(String, Int)] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

main :: IO ()
main = do
  putStrLn "Liste originale des joueurs :"
  mapM_ (\(name, score) -> putStrLn ("  " ++ name ++ " : " ++ show score)) players
  
  putStrLn "\nJoueurs triés par score :"
  let sorted = sortByScore players
  mapM_ (\(name, score) -> putStrLn ("  " ++ name ++ " : " ++ show score)) sorted
  
  putStrLn "\nTop 3 des joueurs :"
  let top3 = topThree sorted
  mapM_ (\(name, score) -> putStrLn ("  " ++ name ++ " : " ++ show score)) top3
  
  putStrLn "\nNoms du top 3 :"
  let topNames = getTopThreePlayers players
  putStrLn ("  " ++ show topNames)
```

### Explication :

* **Étape 1** : `extractPlayers` utilise la **compréhension de liste** pour extraire les noms.
* **Étape 2** : `sortByScore` utilise `sortBy` avec `comparing (Down . snd)` pour trier par score décroissant.
* **Étape 3** : `topThree` utilise `take 3` pour garder les 3 premiers.
* **Étape 4** : `getTopThreePlayers` compose toutes les fonctions avec l'opérateur `.`
* **Sortie** : Affiche chaque étape du traitement des données.
