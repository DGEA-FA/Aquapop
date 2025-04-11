---
title: "STYLE_GUIDE"
output: html_document
---

# STYLE_GUIDE.md

Guide de style du projet **AquaPop**

Ce guide définit les conventions de style, de structuration et de documentation du code utilisé dans le développement du package et de l’application Shiny **AquaPop**.

Il s’applique à toutes les fonctions métier, modules Shiny, scripts d’utilisation et fichiers d’exportation.

---

## Objectifs

- Être compréhensible par les biologistes non programmeurs
- Favoriser la lisibilité et la maintenance du code
- Garantir la cohérence du style dans tout le projet
- Rester compatible avec les conventions du tidyverse
- Produire une documentation professionnelle, réutilisable dans des rapports

---

## Style général : hybride français / anglais

Le projet utilise un style hybride, avec :

| Élément                       | Langue utilisée               |
|-------------------------------|-------------------------------|
| Noms de fonctions             | Français pour le concept, anglais pour l’action (ex: `mortalite_compare_modele`) |
| Arguments de fonction         | Anglais, simples et compatibles tidyverse (`data`, `group`, `method`) |
| Objets internes               | Français, toujours en `snake_case` (`table_resultats`, `espece`, `plot_byclass`) |
| Documentation `roxygen2`      | En français uniquement        |
| Commentaires                  | En français ou bilingues      |

---

## Conventions de nommage

| Élément                     | Règle                                                                 |
|-----------------------------|-----------------------------------------------------------------------|
| Nom des fonctions           | Concept métier en premier (`mortalite_`, `croissance_`), suivi d’un verbe en anglais |
| Arguments                   | En anglais, simples et compatibles tidyverse (`data`, `method`, `group`) |
| Nom du concept "modèle"     | Toujours écrit `modele`, au singulier                                |
| Documentation               | Entièrement en français                                               |
| Commentaires                | En français ou bilingues                                              |

### Convention pour les objets retournés

Le nom de l’objet retourné correspond au nom de la fonction suivi de `_res`.

Exemples :

```r
mortalite_compare_modele_res <- mortalite_compare_modele(data)
mortalite_compare_modele_res_data <- mortalite_compare_modele_res$data
mortalite_compare_modele_res_flextable <- mortalite_compare_modele_res$flextable
```

## Structuration interne des fonctions

### Ordre logique recommandé

1. Validation des données
2. Prétraitements
3. Calculs métier ou modèles
4. Création des visualisations (`ggplot2`)
5. Construction des tableaux (`flextable`)
6. Retour de la liste des objets

### Séparation visuelle des blocs

Chaque bloc est introduit par un commentaire structurant, rédigé en français et précédé de tirets pour en faciliter le repérage visuel. Exemple :

```r
# --- Calcul du Wr ---
# --- Construction du graphique par classe ---
```

### Bonnes pratiques

- Utiliser des noms d’objet **explicites**, toujours en `snake_case`
- Éviter les noms vagues comme `df`, `a`, `t`, `final`
- Utiliser systématiquement `data` comme nom d’argument principal, sauf cas justifié
- Ne pas utiliser de majuscules dans les noms d’objet (`IC95` → `ic95`, `Groupe` → `groupe`)
- Les objets retournés doivent avoir des noms clairs et typés selon leur contenu, par exemple :
  - `table_resultats` pour une table brute
  - `table_formatee` ou `table_flextable` pour une version stylisée
  - `plot_tous`, `plot_byclass` pour des figures différenciées
- Préférer des noms longs explicites plutôt que des raccourcis ambigus

---

## Documentation `roxygen2`

Chaque fonction exportée doit inclure une documentation complète, rédigée en français, en respectant les standards de `roxygen2`. Les blocs suivants sont requis :

- `@description` : Explication claire de l’objectif de la fonction
- `@param` : Un bloc par argument, avec nom, type et rôle
- `@return` : Description explicite de la structure retournée (ex. liste avec `data`, `plot`, `table`)
- `@examples` : Au moins un exemple reproductible avec données simulées ou jouet
- `@export` : Présent pour toute fonction rendue disponible à l’utilisateur
- `@references` : Le cas échéant, inclure les sources scientifiques utilisées dans les calculs

## Organisation des fichiers du projet

Les fonctions sont regroupées par **thème fonctionnel** ou **type de sortie**, selon les cas. Exemples :

- `mod_croissance.R`, `mod_condition.R`, `mod_mortalite.R` : pour les modules métier liés à un thème
- `utils_render.R`, `utils_flextable.R` : pour les fonctions de rendu génériques et les éléments réutilisables

Chaque fichier doit contenir **uniquement des fonctions cohérentes entre elles**, portant sur un même objectif fonctionnel.

Un fichier `exemple_utilisation.R` est maintenu pour illustrer comment utiliser les fonctions métier **en dehors de Shiny**, avec des appels reproductibles simples.

Les fichiers de tests (si présents) doivent être placés dans un répertoire `tests/` ou `tests/testthat/`, conformément aux conventions des packages R.

---

## Utilisation recommandée des packages

- Les appels à des fonctions de packages externes doivent être faits de façon explicite (`ggplot2::ggplot()`), sauf si le package est chargé dans le namespace (ex: via `library(dplyr)` au début d’un script indépendant)
- Les noms d’objets ne doivent pas entrer en conflit avec les noms de fonctions connues