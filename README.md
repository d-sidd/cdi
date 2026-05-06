# Car Dependency Index (CDI) — France

Script R générique pour calculer l'**Indice de Dépendance à la Voiture (CDI)** de n'importe quelle ville française, basé sur la méthode de :

> Campanelli et al. (2026) *"Car Dependency in Urban Accessibility"* — [arXiv:2604.01019](https://arxiv.org/abs/2604.01019)

Le CDI mesure, pour chaque hexagone habité d'une ville, dans quelle mesure les résidents dépendent de la voiture pour accéder aux opportunités (commerces, services, emplois, loisirs…) par rapport aux transports en commun. Il varie entre **−1** (TC largement supérieur) et **+1** (voiture largement supérieure).

---

## Fonctionnement

Le script réalise les étapes suivantes de manière entièrement automatisée :

1. **Résolution géographique** — contour de la ville via OpenStreetMap/Nominatim
2. **Téléchargement des données** — OSM (Geofabrik), INSEE Filosofi 2021, GTFS fourni par l'utilisateur
3. **Grille H3** — découpage du territoire en hexagones (~0,1 km² à résolution 9)
4. **Population** — agrégation des carreaux INSEE 200 m sur la grille H3
5. **POIs OSM** — collecte de 26 catégories de points d'intérêt via Overpass API (avec cache)
6. **Matrices de temps de trajet** — calcul voiture et TC via [r5r](https://ipeagit.github.io/r5r/) sur 14 heures de départ
7. **Scores d'accessibilité** — fonction d'opportunité exponentielle (Hansen 1959)
8. **CDI** — `(O_voiture − O_TC) / (O_voiture + O_TC)` pondéré par la population
9. **Exports** — GeoPackage, CSV, rapport HTML (RMarkdown optionnel)

---

## Données requises

| Source | Téléchargement | Notes |
|--------|---------------|-------|
| OSM (PBF régional) | Automatique via [Geofabrik](https://download.geofabrik.de/europe/france/) | ~100–500 Mo selon la région |
| INSEE Filosofi 2021 | Automatique via [data.gouv.fr](https://www.data.gouv.fr/) | ~90 Mo, téléchargé une seule fois |
| GTFS réseau TC | **Manuel** — [transport.data.gouv.fr](https://transport.data.gouv.fr/) | Requis pour le calcul TC |
| Congestion TomTom | **Optionnel** — [TomTom Traffic Index](https://www.tomtom.com/traffic-index/) | Fichier Excel Heure × Jour |

---

## Installation

### Prérequis

- **R ≥ 4.2**
- **Java ≥ 11** (requis par r5r)
- **osmium-tool** (recommandé pour la découpe OSM — optionnel)
  - Windows : `conda install -c conda-forge osmium-tool`
  - Linux/macOS : `apt install osmium-tool` / `brew install osmium-tool`

### Packages R

Les packages sont installés automatiquement au lancement du script. Les principaux sont :

```r
r5r, h3jsr, sf, dplyr, osmdata, sfarrow, arrow, data.table,
ggplot2, tidyr, lubridate, httr, readxl, purrr, fs
```

> `sfarrow` est installé depuis GitHub (`wcjochem/sfarrow`) si absent.

---

## Utilisation


### 1. Préparer les fichiers GTFS

Télécharger le GTFS de votre réseau sur [transport.data.gouv.fr](https://transport.data.gouv.fr/) et le placer dans `data/<ville>/gtfs/`.

### 2. Configurer les paramètres

Éditer la section **0. PARAMETRES** en tête de `cdi_france.R` :

```r
CODE_INSEE      <- "44109"          # Code INSEE commune
NOM_VILLE       <- "Nantes"         # Nom affiché
GTFS_FICHIERS   <- c("data/nantes/gtfs/gtfs_nantes.zip")
CONGESTION_FICHIER <- NULL          # ou chemin vers un fichier Excel TomTom
DATE_ANALYSE    <- "2026-05-05"
RAM_JAVA_GO     <- 8                # Go de RAM pour r5r
```


### Structure des dossiers générée automatiquement

```
data/
├── pbf_osm/               # PBF régionaux OSM (partagés entre villes)
├── filosofi/              # Parquet INSEE (partagé entre villes)
└── <ville>/
    ├── r5r_network/       # Réseau r5r + OSM découpé + GTFS
    ├── pois_cache/        # Cache POIs par catégorie (.rds)
    ├── ttm_cache/         # Cache matrices de trajet (.rds)
    └── sorties/           # Résultats finaux
```

---

## Sorties

| Fichier | Description |
|---------|-------------|
| `<Ville>_CDI_resultats.gpkg` | GeoPackage H3 avec CDI, population, scores |
| `<Ville>_CDI_hexagones.csv` | Même données en CSV |
| `<Ville>_CDI_quintiles.csv` | Distribution par quintile de population |
| `<Ville>_CDI_synthese.csv`  | Indicateurs globaux (CDI moyen pondéré, etc.) |
| `<Ville>_CDI_rapport.html`  | Rapport cartographique (si `.Rmd` présent) |

---

## Paramètres méthodologiques

| Paramètre | Défaut | Description |
|-----------|--------|-------------|
| `RESOLUTION_H3` | `9` | Résolution grille H3 (~0,1 km²/hex) |
| `TAU` | `60` | Demi-vie accessibilité (min) |
| `PARKING_MIN` | `15` | Temps de stationnement voiture (min) |
| `HEURES_DEPART` | `8:21` | Plage horaire analysée |
| `TEMPS_MAX_MIN` | `60` | Durée max de trajet (min) |

---

## Résultats de référence

| Ville | CDI moyen pondéré | Population analysée |
|-------|:-----------------:|:-------------------:|
| Paris | −0.67 | 2 133 111 |
| Bordeaux | +0.18 | 249 712 |
| Montpellier | +0.22 | 290 053 |
| Nantes | +0.05 | 313 237 |

*Résultats indicatifs obtenus avec les paramètres par défaut et les données GTFS officielles.*

---

## Limitations connues

- Le contour de la ville est récupéré via Nominatim (OSM) ; pour les communes avec un contour complexe ou mal référencé, une bbox rectangulaire est utilisée en fallback.
- Sans fichier GTFS, les transports en commun sont simulés par la marche à pied uniquement, ce qui surestime fortement le CDI.

---

## Citation

Si vous utilisez ce script dans vos travaux, merci de citer :

```
Campanelli et al. (2026). Car Dependency in Urban Accessibility. arXiv:2604.01019
```
