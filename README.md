# AquapopQC

AquapopQC est une application R Shiny conçue pour automatiser les analyses les plus couramment utilisées pour décrire une population de poissons. Les analyses s’appliquent aux pêches expérimentales normalisées au touladi, au doré jaune et à l’omble de fontaine (PENT, PENDJ, PENOF). Les comparaisons spatiales et temporelles ne sont pas abordées dans cette version de l’outil.

# Structure du projet

 Voici une vue d'ensemble de la structure du projet :

 - `app.R`: Fichier principal pour exécuter l'application Shiny.
 - `AquapopQC.Rproj`: Fichier de projet RStudio.
 - `data/`: Contient les fichiers de données, par exemple, `exempledata.xlsx`.
 - `R/`: Contient tous les scripts R utilisés par l'application, y compris les modules de l'application (`app_ui.R`, `app_server.R`) et divers scripts de traitement des données et d'analyse.
 - `renv/`: Répertoire de gestion des dépendances avec `{renv}`.
 - `reprex/`: Contient des exemples reproductibles (`reprex`).
 - `rsconnect/`: Contient des fichiers de déploiement pour ShinyApps.io.
 - `texte/`: Contient des fichiers texte, principalement au format `.rmd`, pour générer des rapports ou des sections de texte.
 - `README.md`: Ce fichier de documentation.
 - `renv.lock`: Fichier de verrouillage des dépendances pour `{renv}`.
 - `DESCRIPTION`: Fichier décrivant les dépendances du projet.
 - `report.Rmd`: Un document RMarkdown pour générer un rapport.
 - `debugging.Rmd`, `testing_bordel.R`: Fichiers de débogage et de tests.

## Installation

### 1. Télécharger le projet

Pour télécharger le projet, suivez ces étapes :

1. Allez sur la page GitHub du projet : [lien vers votre dépôt GitHub].
2. Cliquez sur le bouton vert "Code".
3. Sélectionnez "Download ZIP".
4. Décompressez le fichier ZIP sur votre ordinateur.

### 2. Ouvrir le projet dans RStudio

1. Ouvrez RStudio.
2. Dans RStudio, allez dans "File" > "Open Project..." et sélectionnez le fichier `AquapopQC.Rproj` situé dans le dossier du projet que vous avez décompressé.

### 3. Installer les dépendances

Pour installer les packages nécessaires, suivez ces étapes simples dans RStudio :

1. Dans RStudio, ouvrez le fichier `app.R`.
2. Exécutez les commandes suivantes dans la console pour installer les dépendances.

 1. Assurez-vous que `renv` est installé :

```r
install.packages("renv")
```

 2. Restaurez l'environnement R à partir du fichier `renv.lock` :

```r
renv::restore()
```

 Cela installera toutes les versions exactes des packages utilisés lors du développement de l'application.

4. Exécuter l'application
Une fois les dépendances installées, vous pouvez lancer l'application en exécutant cette commande dans la console :

```r
shiny::runApp()
```

 Ou bien, ouvrez simplement le fichier `app.R` dans RStudio et cliquez sur "Run App".

## Dépendances

 Les packages suivants sont utilisés par cette application, et leurs versions minimales sont spécifiées dans le fichier `DESCRIPTION` :

 - shiny
 - car
 - DT
 - kableExtra
 - reactable
 - FSA
 - nlstools
 - shinyBS
 - gghighlight
 - htmltools
 - markdown
 - readxl
 - ggplot2
 - scales
 - dplyr
 - patchwork
 - reactlog
 - stringr
 - chron
 - purrr
 - writexl
 - shinycssloaders
 - glue
 - fishmethods
 - hnp
 - MASS
 - glmmTMB
 - MuMIn
 - plotly
 - gapminder
 - AER
 - pROC
 - DescTools
 - emdbook
 - AICcmodavg
 - investr

## Contribution

 Si vous souhaitez contribuer à ce projet, veuillez soumettre une pull request ou ouvrir une issue pour discuter de vos modifications.

## Licence

 Cette application est sous licence MIT. Consultez le fichier `LICENSE` pour plus de détails.
