## ✅ HC1T7 - Conversion Fahrenheit → Celsius

### 🎯 Objectif
Comprendre les **conversions de température** en Haskell avec les formules mathématiques de Fahrenheit vers Celsius et vice versa. Maîtriser les **opérations arithmétiques** sur les nombres flottants et la **précision des calculs**.

### 📝 Concepts Clés
- **Conversion de température** : Application des formules mathématiques C = (F-32) × 5/9 et F = C × 9/5 + 32
- **Arithmétique flottante** : Utilisation du type `Float` pour les nombres décimaux
- **Précision des calculs** : Gestion des arrondis et de la précision avec les nombres flottants
- **Fonctions pures** : Même température d'entrée produit toujours le même résultat

### 💻 Code Complet

```haskell
-- Main.hs

-- | Conversion Fahrenheit vers Celsius
-- Formule mathématique : C = (F - 32) × (5/9)
-- Exemple : 32°F = 0°C, 212°F = 100°C
fToC :: Float -> Float
fToC f = (f - 32) * (5/9)

-- | Conversion Celsius vers Fahrenheit  
-- Formule mathématique : F = C × (9/5) + 32
-- Exemple : 0°C = 32°F, 100°C = 212°F
cToF :: Float -> Float
cToF c = c * (9/5) + 32

-- | Arrondit un nombre flottant à 2 décimales pour l'affichage
-- Utilise la technique : arrondir (x * 100) / 100
roundTo2 :: Float -> Float
roundTo2 x = fromIntegral (round (x * 100)) / 100

-- | Températures de test avec leurs descriptions
-- Inclut des points de référence importants en Fahrenheit
testTemperaturesFahrenheit :: [(Float, String)]
testTemperaturesFahrenheit = [
    (32, "Point de congélation de l'eau"),
    (212, "Point d'ébullition de l'eau"), 
    (98.6, "Température corporelle normale"),
    (0, "Zéro Fahrenheit (très froid)"),
    (68, "Température ambiante confortable"),
    (-40, "Point où F = C"),
    (100, "Journée très chaude"),
    (451, "Température d'auto-combustion du papier")
  ]

-- | Températures de test en Celsius pour la conversion inverse
testTemperaturesCelsius :: [(Float, String)]
testTemperaturesCelsius = [
    (0, "Point de congélation de l'eau"),
    (100, "Point d'ébullition de l'eau"),
    (37, "Température corporelle normale"),
    (-17.8, "Zéro Fahrenheit en Celsius"),
    (20, "Température ambiante confortable"),
    (-40, "Point où F = C"),
    (37.8, "100°F en Celsius"),
    (-273.15, "Zéro absolu")
  ]

-- | Fonction qui teste la précision des conversions aller-retour
-- Convertit F→C→F pour vérifier qu'on retrouve la valeur originale
testRoundTrip :: Float -> (Float, Float, Float)
testRoundTrip originalF = 
  let celsius = fToC originalF
      backToF = cToF celsius
  in (originalF, celsius, backToF)

-- | Fonction principale pour démontrer les conversions de température
main :: IO ()
main = do
  putStrLn "=== CONVERSION FAHRENHEIT ↔ CELSIUS EN HASKELL ==="
  putStrLn ""
  
  -- Explication des formules
  putStrLn "=== Formules de Conversion ==="
  putStrLn "• Fahrenheit → Celsius : C = (F - 32) × (5/9)"
  putStrLn "• Celsius → Fahrenheit : F = C × (9/5) + 32"
  putStrLn ""
  putStrLn "Points de référence historiques :"
  putStrLn "• 32°F = 0°C   (congélation de l'eau)"
  putStrLn "• 212°F = 100°C (ébullition de l'eau)"
  putStrLn "• -40°F = -40°C (seul point d'égalité)"
  putStrLn ""
  
  -- Démonstration Fahrenheit → Celsius
  putStrLn "=== Conversion Fahrenheit → Celsius ==="
  putStrLn "Fonction : fToC :: Float -> Float"
  putStrLn ""
  
  mapM_ (\(tempF, description) -> do
    let tempC = fToC tempF
    let roundedC = roundTo2 tempC
    putStrLn $ "• " ++ show tempF ++ "°F = " ++ show roundedC ++ "°C"
    putStrLn $ "  (" ++ description ++ ")"
    putStrLn $ "  Calcul : (" ++ show tempF ++ " - 32) × (5/9) = " ++ show roundedC
    putStrLn ""
    ) testTemperaturesFahrenheit
  
  -- Démonstration Celsius → Fahrenheit
  putStrLn "=== Conversion Celsius → Fahrenheit ==="
  putStrLn "Fonction : cToF :: Float -> Float"
  putStrLn ""
  
  mapM_ (\(tempC, description) -> do
    let tempF = cToF tempC
    let roundedF = roundTo2 tempF
    putStrLn $ "• " ++ show tempC ++ "°C = " ++ show roundedF ++ "°F"
    putStrLn $ "  (" ++ description ++ ")"
    putStrLn $ "  Calcul : " ++ show tempC ++ " × (9/5) + 32 = " ++ show roundedF
    putStrLn ""
    ) testTemperaturesCelsius
  
  -- Test de précision aller-retour
  putStrLn "=== Test de Précision (Aller-Retour) ==="
  putStrLn "Vérification : F → C → F doit retrouver la valeur originale"
  putStrLn ""
  
  let testValues = [32, 68, 100, 212, (-40)]
  mapM_ (\tempF -> do
    let (original, celsius, backToF) = testRoundTrip tempF
    let roundedC = roundTo2 celsius
    let roundedBackF = roundTo2 backToF
    let difference = abs (original - backToF)
    
    putStrLn $ "• " ++ show original ++ "°F → " ++ show roundedC ++ "°C → " ++ show roundedBackF ++ "°F"
    if difference < 0.01
      then putStrLn "  ✅ Précision excellente (différence < 0.01°)"
      else putStrLn $ "  ⚠️  Différence : " ++ show (roundTo2 difference) ++ "°F"
    putStrLn ""
    ) testValues
  
  -- Démonstration de calculs étape par étape
  putStrLn "=== Calcul Détaillé Étape par Étape ==="
  putStrLn ""
  
  let exampleF = 98.6
  putStrLn $ "Conversion de " ++ show exampleF ++ "°F en Celsius :"
  putStrLn $ "1. Soustraire 32 : " ++ show exampleF ++ " - 32 = " ++ show (exampleF - 32)
  putStrLn $ "2. Multiplier par 5/9 : " ++ show (exampleF - 32) ++ " × (5/9) = " ++ show (roundTo2 ((exampleF - 32) * (5/9)))
  putStrLn $ "Résultat : " ++ show exampleF ++ "°F = " ++ show (roundTo2 (fToC exampleF)) ++ "°C"
  putStrLn ""
  
  let exampleC = 20
  putStrLn $ "Conversion de " ++ show exampleC ++ "°C en Fahrenheit :"
  putStrLn $ "1. Multiplier par 9/5 : " ++ show exampleC ++ " × (9/5) = " ++ show (exampleC * (9/5))
  putStrLn $ "2. Ajouter 32 : " ++ show (exampleC * (9/5)) ++ " + 32 = " ++ show (roundTo2 (exampleC * (9/5) + 32))
  putStrLn $ "Résultat : " ++ show exampleC ++ "°C = " ++ show (roundTo2 (cToF exampleC)) ++ "°F"
  putStrLn ""
  
  -- Quelques faits intéressants
  putStrLn "=== Faits Intéressants sur les Températures ==="
  putStrLn $ "• Zéro absolu : " ++ show (roundTo2 (cToF (-273.15))) ++ "°F = -273.15°C"
  putStrLn $ "• Surface du Soleil : ~" ++ show (roundTo2 (cToF 5500)) ++ "°F = 5500°C"
  putStrLn $ "• Azote liquide : " ++ show (roundTo2 (cToF (-196))) ++ "°F = -196°C"
  putStrLn $ "• Température de cuisson : " ++ show (roundTo2 (cToF 180)) ++ "°F = 180°C"
```

### 🔍 Instructions GHCi

```ghci
-- Dans GHCi, tapez ces commandes pour explorer les conversions de température :

-- Vérification des types
:t fToC
-- fToC :: Float -> Float

:t cToF  
-- cToF :: Float -> Float

-- Tests de conversion Fahrenheit → Celsius
fToC 32
-- 0.0

fToC 212
-- 100.0

fToC 98.6
-- 37.0

-- Tests de conversion Celsius → Fahrenheit
cToF 0
-- 32.0

cToF 100
-- 212.0

cToF 37
-- 98.6

-- Vérification du point d'égalité F = C
fToC (-40)
-- -40.0

cToF (-40)
-- -40.0

-- Test de précision aller-retour
let temp = 75.5
let celsius = fToC temp
let backToF = cToF celsius
(temp, celsius, backToF)
-- (75.5, 24.166666, 75.5)

-- Création de fonctions spécialisées
let freezingPointC = fToC 32
let boilingPointF = cToF 100
(freezingPointC, boilingPointF)
-- (0.0, 212.0)
```

### 📊 Sortie Attendue
```
=== CONVERSION FAHRENHEIT ↔ CELSIUS EN HASKELL ===

=== Formules de Conversion ===
• Fahrenheit → Celsius : C = (F - 32) × (5/9)
• Celsius → Fahrenheit : F = C × (9/5) + 32

Points de référence historiques :
• 32°F = 0°C   (congélation de l'eau)
• 212°F = 100°C (ébullition de l'eau)
• -40°F = -40°C (seul point d'égalité)

=== Conversion Fahrenheit → Celsius ===
Fonction : fToC :: Float -> Float

• 32.0°F = 0.0°C
  (Point de congélation de l'eau)
  Calcul : (32.0 - 32) × (5/9) = 0.0

• 212.0°F = 100.0°C
  (Point d'ébullition de l'eau)
  Calcul : (212.0 - 32) × (5/9) = 100.0

• 98.6°F = 37.0°C
  (Température corporelle normale)
  Calcul : (98.6 - 32) × (5/9) = 37.0

• 0.0°F = -17.78°C
  (Zéro Fahrenheit (très froid))
  Calcul : (0.0 - 32) × (5/9) = -17.78

=== Test de Précision (Aller-Retour) ===
Vérification : F → C → F doit retrouver la valeur originale

• 32.0°F → 0.0°C → 32.0°F
  ✅ Précision excellente (différence < 0.01°)

• 68.0°F → 20.0°C → 68.0°F
  ✅ Précision excellente (différence < 0.01°)

=== Calcul Détaillé Étape par Étape ===

Conversion de 98.6°F en Celsius :
1. Soustraire 32 : 98.6 - 32 = 66.6
2. Multiplier par 5/9 : 66.6 × (5/9) = 37.0
Résultat : 98.6°F = 37.0°C

Conversion de 20.0°C en Fahrenheit :
1. Multiplier par 9/5 : 20.0 × (9/5) = 36.0
2. Ajouter 32 : 36.0 + 32 = 68.0
Résultat : 20.0°C = 68.0°F
```

### 🚀 Points Importants à Retenir
1. **Formules précises** : Les conversions utilisent les formules mathématiques exactes
2. **Type Float** : Nécessaire pour gérer les nombres décimaux avec précision
3. **Fonctions pures** : Même température produit toujours le même résultat
4. **Précision des calculs** : Les conversions aller-retour préservent la valeur originale
5. **Applications pratiques** : Utile pour la météo, la cuisine, la science

### 🧠 Explication Détaillée - Origine des Formules

Les formules de conversion proviennent des définitions historiques des échelles :

```haskell
-- Échelle Fahrenheit (1724) :
-- • 0°F = température la plus froide obtenue par Fahrenheit
-- • 32°F = point de congélation de l'eau  
-- • 212°F = point d'ébullition de l'eau
-- • Écart = 212 - 32 = 180 degrés pour 100°C

-- Échelle Celsius (1742) :
-- • 0°C = point de congélation de l'eau
-- • 100°C = point d'ébullition de l'eau  
-- • Écart = 100 degrés entre congélation et ébullition

-- Donc le ratio est : 180°F / 100°C = 9/5
-- D'où les formules :
-- C = (F - 32) × (5/9)    -- Décalage puis ratio
-- F = C × (9/5) + 32      -- Ratio puis décalage
```
