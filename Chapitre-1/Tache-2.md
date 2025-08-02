## ✅ HC1T2 - Fonction Pure : Aire d'un Cercle

### Code :

```haskell
-- Main.hs
circleArea :: Float -> Float
circleArea r = pi * r * r

main :: IO ()
main = do
  let radius1 = 2.0
  let radius2 = 5.0
  putStrLn ("Aire d'un cercle de rayon " ++ show radius1 ++ " : " ++ show (circleArea radius1))
  putStrLn ("Aire d'un cercle de rayon " ++ show radius2 ++ " : " ++ show (circleArea radius2))
  putStrLn ("Valeur de pi utilisée : " ++ show pi)
```

### Explication :

* Une **fonction pure** dépend uniquement de ses **paramètres** et produit toujours le même résultat.
* `circleArea` calcule **l'aire d'un cercle** avec la formule : A = π × r²
* `pi` est une constante **prédéfinie** en Haskell (≈ 3.14159).
* **Sortie** : `Aire d'un cercle de rayon 2.0 : 12.566371` et `Aire d'un cercle de rayon 5.0 : 78.53982`
