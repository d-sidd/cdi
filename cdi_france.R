# =============================================================================
# Car Dependency Index (CDI) — Script générique France
# =============================================================================
# Basé sur : Campanelli et al. (2026) "Car Dependency in Urban Accessibility"
# arXiv:2604.01019
#
# UTILISATION :
#   1. Renseignez la section "0. PARAMETRES" ci-dessous
#   2. Fournissez le(s) fichier(s) GTFS de votre réseau TC local
#   3. (Optionnel) Fournissez un fichier Excel de congestion TomTom
#   4. Lancez le script de haut en bas
#
# Toutes les autres données (OSM, INSEE, POIs) sont téléchargées automatiquement.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. PARAMETRES — A RENSEIGNER
# -----------------------------------------------------------------------------

CODE_INSEE      <- "75056"   # Code INSEE de la commune (ex: "44109", "33063", "34172")
NOM_VILLE       <- "Paris"   # Nom de la ville (ex: "Nantes", "Bordeaux", "Montpellier")

# Dossier spécifique à la ville (résultats, caches POIs/TTM, r5r_network)
DOSSIER_TRAVAIL <- file.path("data", tolower(NOM_VILLE))

# GTFS : vecteur de chemins vers les .zip du réseau TC local
# Télécharger sur https://transport.data.gouv.fr/
# Laisser c() si aucun disponible (TC = marche uniquement)
GTFS_FICHIERS <- c(
  "data/paris/gtfs/IDFM-gtfs.zip"
)

# Congestion TomTom (optionnel)
# Fichier Excel avec colonnes : Heure, Lundi..Dimanche
# Valeurs = niveau congestion (ex: 0.75 = +75% vs flux libre)
# Mettre NULL pour désactiver la correction de congestion
# Données disponibles sur https://www.tomtom.com/traffic-index/
CONGESTION_FICHIER <- NULL
# CONGESTION_FICHIER <- "data/paris/congestion_tomtom_2025.xlsx"

# Dossiers partagés entre toutes les villes (téléchargés une seule fois)
DOSSIER_OSM      <- file.path("data", "pbf_osm")    # PBF régionaux Geofabrik
DOSSIER_FILOSOFI <- file.path("data", "filosofi")   # Parquet INSEE Filosofi

# Paramètres méthodologiques
RESOLUTION_H3 <- 9      # Résolution de la grille H3 (7 à 10, défaut : 9 ≈ 0.1 km²)
TAU           <- 60     # Demi-vie de la fonction d'accessibilité (minutes)
PARKING_MIN   <- 15     # Temps de stationnement forfaitaire voiture (minutes)
HEURES_DEPART <- 8:21   # Plage horaire analysée (heures de départ)
DATE_ANALYSE  <- "2026-05-05"  # Date de référence (format YYYY-MM-DD)
TEMPS_MAX_MIN <- 60     # Durée maximale de trajet (minutes)
RAM_JAVA_GO   <- 8      # RAM allouée à la JVM pour r5r (Go)

# =============================================================================
# FIN DES PARAMETRES
# =============================================================================


# -----------------------------------------------------------------------------
# 1. PACKAGES
# -----------------------------------------------------------------------------

pkgs <- c("r5r", "h3jsr", "osmdata", "sf", "dplyr", "data.table",
          "ggplot2", "tidyr", "lubridate", "viridis", "scales",
          "rJavaEnv", "httr", "fs", "purrr", "readxl", "tidyverse",
          "sfarrow", "arrow", "stringr", "rmarkdown")

manquants <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if (length(manquants)) install.packages(manquants)
if (!"sfarrow" %in% installed.packages()[,"Package"]) {
  if (!"remotes" %in% installed.packages()[,"Package"]) install.packages("remotes")
  remotes::install_github("wcjochem/sfarrow")
}

suppressPackageStartupMessages({
  library(sf);       library(dplyr);    library(data.table)
  library(ggplot2);  library(tidyr);    library(lubridate)
  library(h3jsr);    library(osmdata);  library(viridis)
  library(scales);   library(httr);     library(fs)
  library(purrr);    library(readxl);   library(tidyverse)
  library(sfarrow);  library(arrow);    library(stringr)
})

# Assurer la disponibilité d'osmium (Windows : via miniforge/conda)
if (Sys.which("osmium") == "") {
  candidats_path <- c(
    "C:/ProgramData/miniforge3/Library/bin",
    paste0(Sys.getenv("USERPROFILE"), "/miniforge3/Library/bin"),
    paste0(Sys.getenv("USERPROFILE"), "/AppData/Local/miniforge3/Library/bin")
  )
  nouveaux_paths <- candidats_path[dir.exists(candidats_path)]
  if (length(nouveaux_paths) > 0) {
    Sys.setenv(PATH = paste(c(nouveaux_paths, Sys.getenv("PATH")),
                            collapse = .Platform$path.sep))
    message("PATH étendu — osmium : '", Sys.which("osmium"), "'")
  }
}

options(java.parameters = paste0("-Xmx", RAM_JAVA_GO, "G"))
library(r5r)

# Création des dossiers nécessaires
dirs_ville <- c(DOSSIER_TRAVAIL,
                file.path(DOSSIER_TRAVAIL, "r5r_network"),
                file.path(DOSSIER_TRAVAIL, "sorties"),
                file.path(DOSSIER_TRAVAIL, "pois_cache"),
                file.path(DOSSIER_TRAVAIL, "ttm_cache"))
dirs_partages <- c(DOSSIER_OSM, DOSSIER_FILOSOFI)

fs::dir_create(c(dirs_ville, dirs_partages))
message(sprintf("✓ Environnement prêt — %s [%s]", NOM_VILLE, CODE_INSEE))
message(sprintf("  Dossier ville    : %s", DOSSIER_TRAVAIL))
message(sprintf("  Dossier OSM      : %s", DOSSIER_OSM))
message(sprintf("  Dossier Filosofi : %s", DOSSIER_FILOSOFI))


# -----------------------------------------------------------------------------
# 2. RÉSOLUTION GÉOGRAPHIQUE
# -----------------------------------------------------------------------------

message("\n═══ RÉSOLUTION GÉOGRAPHIQUE ═══")

## 2a. Contour via Nominatim ----
contour_raw <- tryCatch(
  osmdata::getbb(paste0(NOM_VILLE, ", France"),
                 featuretype = "city", format_out = "sf_polygon", limit = 1),
  error = function(e) NULL
)

if (is.null(contour_raw) || nrow(contour_raw) == 0) {
  bbox_raw <- tryCatch(
    osmdata::getbb(paste0(NOM_VILLE, ", France")), error = function(e) NULL)
  if (!is.null(bbox_raw)) {
    contour_wgs84 <- st_sf(geometry = st_sfc(st_polygon(list(matrix(
      c(bbox_raw[1,1], bbox_raw[2,1], bbox_raw[1,2], bbox_raw[2,1],
        bbox_raw[1,2], bbox_raw[2,2], bbox_raw[1,1], bbox_raw[2,2],
        bbox_raw[1,1], bbox_raw[2,1]), ncol = 2, byrow = TRUE))), crs = 4326))
    message("  ✓ Bbox rectangulaire utilisée")
  } else {
    stop("Impossible de récupérer le contour de ", NOM_VILLE)
  }
} else {
  contour_wgs84 <- st_transform(contour_raw, 4326)
  if (inherits(st_geometry(contour_wgs84), "sfc_MULTIPOLYGON"))
    contour_wgs84 <- contour_wgs84 %>%
      mutate(aire = as.numeric(st_area(.))) %>% slice_max(aire, n=1) %>% select(-aire)
}
message(sprintf("✓ Contour %s : %.1f km²",
                NOM_VILLE, as.numeric(st_area(contour_wgs84)) / 1e6))

## 2b. Lookup région Geofabrik ----
dept <- substr(CODE_INSEE, 1,
               ifelse(nchar(CODE_INSEE)==5 & substr(CODE_INSEE,1,2)=="97", 3, 2))
GEOFABRIK_REGIONS <- list(
  # Alsace (Grand Est)
  "67" = "alsace", "68" = "alsace",

  # Aquitaine (Nouvelle-Aquitaine)
  "24" = "aquitaine", "33" = "aquitaine", "40" = "aquitaine",
  "47" = "aquitaine", "64" = "aquitaine",

  # Auvergne (Auvergne-Rhône-Alpes)
  "03" = "auvergne", "15" = "auvergne", "43" = "auvergne", "63" = "auvergne",

  # Basse-Normandie (Normandie)
  "14" = "basse-normandie", "50" = "basse-normandie", "61" = "basse-normandie",

  # Bourgogne (Bourgogne-Franche-Comté)
  "21" = "bourgogne", "58" = "bourgogne", "71" = "bourgogne", "89" = "bourgogne",

  # Bretagne
  "22" = "bretagne", "29" = "bretagne", "35" = "bretagne", "56" = "bretagne",

  # Centre (Centre-Val de Loire)
  "18" = "centre", "28" = "centre", "36" = "centre",
  "37" = "centre", "41" = "centre", "45" = "centre",

  # Champagne-Ardenne (Grand Est)
  "08" = "champagne-ardenne", "10" = "champagne-ardenne",
  "51" = "champagne-ardenne", "52" = "champagne-ardenne",

  # Corse
  "2A" = "corse", "2B" = "corse",

  # Franche-Comté (Bourgogne-Franche-Comté)
  "25" = "franche-comte", "39" = "franche-comte",
  "70" = "franche-comte", "90" = "franche-comte",

  # Haute-Normandie (Normandie)
  "27" = "haute-normandie", "76" = "haute-normandie",

  # Île-de-France
  "75" = "ile-de-france", "77" = "ile-de-france", "78" = "ile-de-france",
  "91" = "ile-de-france", "92" = "ile-de-france", "93" = "ile-de-france",
  "94" = "ile-de-france", "95" = "ile-de-france",

  # Languedoc-Roussillon (Occitanie)
  "09" = "languedoc-roussillon", "11" = "languedoc-roussillon",
  "12" = "languedoc-roussillon", "30" = "languedoc-roussillon",
  "34" = "languedoc-roussillon", "48" = "languedoc-roussillon",
  "66" = "languedoc-roussillon",

  # Limousin (Nouvelle-Aquitaine)
  "19" = "limousin", "23" = "limousin", "87" = "limousin",

  # Lorraine (Grand Est)
  "54" = "lorraine", "55" = "lorraine", "57" = "lorraine", "88" = "lorraine",

  # Midi-Pyrénées (Occitanie)
  "31" = "midi-pyrenees", "32" = "midi-pyrenees", "46" = "midi-pyrenees",
  "65" = "midi-pyrenees", "81" = "midi-pyrenees", "82" = "midi-pyrenees",

  # Nord-Pas-de-Calais (Hauts-de-France)
  "59" = "nord-pas-de-calais", "62" = "nord-pas-de-calais",

  # Pays de la Loire
  "44" = "pays-de-la-loire", "49" = "pays-de-la-loire", "53" = "pays-de-la-loire",
  "72" = "pays-de-la-loire", "85" = "pays-de-la-loire",

  # Picardie (Hauts-de-France)
  "02" = "picardie", "60" = "picardie", "80" = "picardie",

  # Poitou-Charentes (Nouvelle-Aquitaine)
  "16" = "poitou-charentes", "17" = "poitou-charentes",
  "79" = "poitou-charentes", "86" = "poitou-charentes",

  # Provence-Alpes-Côte d'Azur
  "04" = "provence-alpes-cote-d-azur", "05" = "provence-alpes-cote-d-azur",
  "06" = "provence-alpes-cote-d-azur", "13" = "provence-alpes-cote-d-azur",
  "83" = "provence-alpes-cote-d-azur", "84" = "provence-alpes-cote-d-azur",

  # Rhône-Alpes (Auvergne-Rhône-Alpes)
  "01" = "rhone-alpes", "07" = "rhone-alpes", "26" = "rhone-alpes",
  "38" = "rhone-alpes", "42" = "rhone-alpes", "69" = "rhone-alpes",
  "73" = "rhone-alpes", "74" = "rhone-alpes",

  # DOM
  "971" = "guadeloupe", "972" = "martinique", "973" = "guyane",
  "974" = "reunion",    "976" = "mayotte"
)

region_geofabrik <- GEOFABRIK_REGIONS[[dept]]
if (is.null(region_geofabrik))
  stop(sprintf("Département '%s' introuvable. Ajoutez-le dans GEOFABRIK_REGIONS.", dept))
message(sprintf("✓ Région Geofabrik : %s (dept %s)", region_geofabrik, dept))


# -----------------------------------------------------------------------------
# 3. TÉLÉCHARGEMENT DES DONNÉES
# -----------------------------------------------------------------------------

message("\n═══ TÉLÉCHARGEMENT ═══")

## 3a. OSM ----
# Le PBF régional est stocké dans DOSSIER_OSM (partagé entre villes).
# Plusieurs villes d'une même région n'ont besoin que d'un seul PBF.
osm_region_pbf <- file.path(DOSSIER_OSM,
                             paste0(region_geofabrik, "-latest.osm.pbf"))
url_osm <- sprintf("https://download.geofabrik.de/europe/france/%s-latest.osm.pbf",
                   region_geofabrik)

if (!file.exists(osm_region_pbf)) {
  message(sprintf("Téléchargement OSM %s → %s...", region_geofabrik, DOSSIER_OSM))
  download.file(url_osm, destfile = osm_region_pbf, mode = "wb", method = "libcurl")
  message("✓ OSM téléchargé")
} else {
  message(sprintf("✓ OSM déjà présent : %s (%.0f Mo)",
                  basename(osm_region_pbf), file.size(osm_region_pbf)/1e6))
}

nom_ville_safe <- tolower(gsub("[^a-z0-9]", "_", tolower(NOM_VILLE)))
osm_ville_pbf  <- file.path(DOSSIER_TRAVAIL, "r5r_network",
                             paste0(nom_ville_safe, ".osm.pbf"))

if (!file.exists(osm_ville_pbf)) {
  bbox_c   <- st_bbox(st_buffer(contour_wgs84, dist = 0.08))
  bbox_str <- sprintf("%.6f,%.6f,%.6f,%.6f",
                      bbox_c["xmin"], bbox_c["ymin"], bbox_c["xmax"], bbox_c["ymax"])
  message(sprintf("Découpe OSM %s (bbox %s)...", NOM_VILLE, bbox_str))

  osmium_cmd <- Sys.which("osmium")
  if (osmium_cmd == "") {
    candidats <- c(
      "C:/ProgramData/miniforge3/Library/bin/osmium.exe",
      "C:/ProgramData/miniforge3/Scripts/osmium.exe",
      paste0(Sys.getenv("USERPROFILE"), "/miniforge3/Library/bin/osmium.exe"),
      paste0(Sys.getenv("USERPROFILE"), "/AppData/Local/miniforge3/Library/bin/osmium.exe"),
      "C:/ProgramData/Anaconda3/Library/bin/osmium.exe")
    found <- candidats[file.exists(candidats)]
    osmium_cmd <- if (length(found) > 0) found[1] else ""
  }

  osmium_ok <- if (osmium_cmd != "") tryCatch({
    ret <- system2(osmium_cmd,
                   args = c("extract","--bbox", bbox_str,"--set-bounds",
                             "--strategy","complete-ways",
                             osm_region_pbf, "-o", osm_ville_pbf, "--overwrite"),
                   stdout = TRUE, stderr = TRUE)
    attr(ret, "status") == 0 || is.null(attr(ret, "status"))
  }, error = function(e) FALSE) else FALSE

  if (!osmium_ok || !file.exists(osm_ville_pbf)) {
    message("⚠ osmium indisponible — copie PBF régional complet")
    file.copy(osm_region_pbf, osm_ville_pbf, overwrite = TRUE)
  } else {
    message(sprintf("✓ OSM découpé (%.0f Mo)", file.size(osm_ville_pbf)/1e6))
  }
} else {
  message(sprintf("✓ OSM déjà découpé (%.0f Mo)", file.size(osm_ville_pbf)/1e6))
}

## 3b. GTFS ----
gtfs_valides <- GTFS_FICHIERS[file.exists(GTFS_FICHIERS)]
if (length(gtfs_valides) == 0) {
  message("⚠ Aucun GTFS valide. TC = marche uniquement.")
  message("  Téléchargez sur https://transport.data.gouv.fr/")
} else {
  for (g in gtfs_valides) {
    dest <- file.path(DOSSIER_TRAVAIL, "r5r_network", basename(g))
    if (!file.exists(dest)) file.copy(g, dest)
  }
  message(sprintf("✓ %d GTFS copiés", length(gtfs_valides)))
}

## 3c. Congestion TomTom ----
# Source : https://www.tomtom.com/traffic-index/
congestion_disponible <- FALSE
if (!is.null(CONGESTION_FICHIER) && file.exists(CONGESTION_FICHIER)) {
  congestion_tomtom <- readxl::read_excel(CONGESTION_FICHIER)
  cols_ok <- all(c("Heure","Lundi","Mardi","Mercredi","Jeudi",
                   "Vendredi","Samedi","Dimanche") %in% names(congestion_tomtom))
  if (cols_ok) {
    congestion_disponible <- TRUE
    jour_analyse_fr <- c("Dimanche","Lundi","Mardi","Mercredi",
                         "Jeudi","Vendredi","Samedi")[as.POSIXlt(DATE_ANALYSE)$wday+1]
    congestion_long <- congestion_tomtom %>%
      tidyr::pivot_longer(-Heure, names_to = "jour_fr", values_to = "congestion") %>%
      mutate(
        heure_num = as.integer(sub(":.*", "", Heure)),
        heure_num = case_when(
          grepl("am", Heure) & heure_num == 12 ~ 0L,
          grepl("am", Heure)                   ~ heure_num,
          grepl("pm", Heure) & heure_num == 12 ~ 12L,
          grepl("pm", Heure)                   ~ heure_num + 12L),
        facteur_vitesse = 1 / (1 + congestion)
      )
    message(sprintf("✓ Congestion TomTom chargée — jour : %s", jour_analyse_fr))
  } else {
    message("⚠ Format congestion invalide → vitesses théoriques OSM utilisées")
  }
} else {
  message("ℹ Pas de TomTom → vitesses OSM théoriques")
}

facteur_pour_heure <- function(h) {
  if (!congestion_disponible) return(1.0)
  f <- congestion_long %>%
    filter(jour_fr == jour_analyse_fr, heure_num == h) %>%
    pull(facteur_vitesse)
  if (length(f) == 0) 1.0 else f[1]
}

## 3d. INSEE Filosofi 2021 parquet ----
# Le parquet est stocké dans DOSSIER_FILOSOFI (partagé entre toutes les villes).
# Il est téléchargé une seule fois (~90 Mo) et réutilisé pour chaque analyse.
insee_parquet <- file.path(DOSSIER_FILOSOFI, "carreaux_200m_filosofi2021.parquet")
if (!file.exists(insee_parquet)) {
  message(sprintf("Téléchargement INSEE Filosofi 2021 (~90 Mo) → %s...", DOSSIER_FILOSOFI))
  resp <- httr::GET(
    "https://www.data.gouv.fr/api/1/datasets/r/55432374-a91d-43d0-923d-4514dc3eb951",
    httr::write_disk(insee_parquet, overwrite = TRUE), httr::progress())
  if (httr::http_error(resp)) { file.remove(insee_parquet); stop("Téléchargement INSEE échoué.") }
  message("✓ Parquet INSEE téléchargé")
} else {
  message(sprintf("✓ Parquet INSEE présent : %s (%.0f Mo)",
                  DOSSIER_FILOSOFI, file.size(insee_parquet)/1e6))
}

message("✓ Données prêtes !")


# -----------------------------------------------------------------------------
# 4. GRILLE H3
# -----------------------------------------------------------------------------

message("\n═══ GRILLE H3 ═══")

hex_ids <- h3jsr::polygon_to_cells(geometry = contour_wgs84,
                                   res = RESOLUTION_H3, simple = FALSE)
hexagones_sf <- h3jsr::cell_to_polygon(input = hex_ids$h3_address,
                                       simple = FALSE) %>%
  rename(h3_id = h3_address)

dans_ville   <- st_within(st_centroid(hexagones_sf), contour_wgs84, sparse=FALSE)[,1]
hexagones_sf <- hexagones_sf[dans_ville, ]

pts_od <- hexagones_sf %>% st_centroid() %>%
  mutate(id = h3_id, lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>% select(id, lon, lat) %>% as.data.table()

message(sprintf("✓ %d hexagones H3-%d", nrow(hexagones_sf), RESOLUTION_H3))


# -----------------------------------------------------------------------------
# 5. POPULATION ET INDICATEURS SOCIAUX
# -----------------------------------------------------------------------------

message("\n═══ POPULATION INSEE ═══")

bbox_elargie <- st_bbox(st_buffer(contour_wgs84, dist = 0.05))

vars_age_jeunes <- c("ind_0_3","ind_4_5","ind_6_10","ind_11_17")
vars_age_ages   <- c("ind_65_79","ind_80p")
vars_sociales   <- c("men_pauv","men","ind_snv", vars_age_jeunes, vars_age_ages)

message("Lecture parquet INSEE...")
carreaux_raw <- tryCatch({
  cs <- sfarrow::st_read_parquet(insee_parquet)
  cs <- st_set_crs(cs,3035)
  cs <- st_transform(cs, 4326)
  cs[st_intersects(cs, st_as_sfc(bbox_elargie), sparse=FALSE)[,1], ]
}, error = function(e) stop("Lecture parquet échouée : ", conditionMessage(e)))

# Détection variable population (noms variables selon millésime)
pop_vars <- c("ind","ind_c","pop","IND")
pop_var  <- pop_vars[pop_vars %in% names(carreaux_raw)][1]
if (is.na(pop_var)) stop("Variable population non détectée. Colonnes : ",
                         paste(names(carreaux_raw), collapse=", "))

carreaux_ville <- carreaux_raw %>%
  rename(population_carreau = all_of(pop_var)) %>%
  mutate(population_carreau = as.numeric(population_carreau),
         aire_carreau = as.numeric(st_area(.)))

vars_presentes <- vars_sociales[vars_sociales %in% names(carreaux_ville)]

message(sprintf("✓ %d carreaux | %s hab.",
                nrow(carreaux_ville),
                format(round(sum(carreaux_ville$population_carreau, na.rm=TRUE)),
                       big.mark=" ")))

# Intersection carreaux × hexagones (pondération surfacique)
message("Intersection carreaux × hexagones...")
intersections_raw <- st_intersection(
  carreaux_ville %>% select(all_of(c("population_carreau","aire_carreau", vars_presentes))),
  hexagones_sf %>% select(h3_id)
) %>% mutate(aire_inter = as.numeric(st_area(.)),
             poids = aire_inter / aire_carreau,
             pop_hexagone = population_carreau * poids)

intersections <- intersections_raw %>%
  st_drop_geometry() %>% group_by(h3_id) %>%
  summarise(
    population = round(sum(pop_hexagone, na.rm=TRUE)),
    men_pauv   = if("men_pauv"  %in% names(cur_data())) sum(men_pauv *poids,na.rm=T) else NA_real_,
    men        = if("men"       %in% names(cur_data())) sum(men      *poids,na.rm=T) else NA_real_,
    ind_snv    = if("ind_snv"   %in% names(cur_data())) sum(ind_snv  *poids,na.rm=T) else NA_real_,
    ind_0_3    = if("ind_0_3"   %in% names(cur_data())) sum(ind_0_3  *poids,na.rm=T) else NA_real_,
    ind_4_5    = if("ind_4_5"   %in% names(cur_data())) sum(ind_4_5  *poids,na.rm=T) else NA_real_,
    ind_6_10   = if("ind_6_10"  %in% names(cur_data())) sum(ind_6_10 *poids,na.rm=T) else NA_real_,
    ind_11_17  = if("ind_11_17" %in% names(cur_data())) sum(ind_11_17*poids,na.rm=T) else NA_real_,
    ind_65_79  = if("ind_65_79" %in% names(cur_data())) sum(ind_65_79*poids,na.rm=T) else NA_real_,
    ind_80p    = if("ind_80p"   %in% names(cur_data())) sum(ind_80p  *poids,na.rm=T) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    taux_pauvrete = if_else(!is.na(men) & men>0, men_pauv/men, NA_real_),
    pop_jeunes    = ind_0_3 + ind_4_5 + ind_6_10 + ind_11_17,
    pop_ages      = ind_65_79 + ind_80p,
    indice_vieillissement = if_else(!is.na(pop_jeunes) & pop_jeunes>0,
                                    pop_ages/pop_jeunes*100, NA_real_)
  )

hexagones_sf    <- hexagones_sf %>% left_join(intersections, by="h3_id") %>%
  mutate(population = replace_na(population, 0L))
hexagones_habites <- hexagones_sf %>% filter(population > 0)
message(sprintf("✓ %s hab. dans %d hexagones",
                format(sum(hexagones_habites$population), big.mark=" "),
                nrow(hexagones_habites)))


# -----------------------------------------------------------------------------
# 6. POIs OSM (avec cache)
# -----------------------------------------------------------------------------

message("\n═══ POIs OSM ═══")

bbox_ville <- st_bbox(contour_wgs84)

OVERPASS_SERVERS <- c(
  "https://overpass-api.de/api/interpreter",
  "https://maps.mail.ru/osm/tools/overpass/api/interpreter",
  "https://overpass.openstreetmap.ru/api/interpreter"
)
detecter_overpass <- function() {
  for (s in OVERPASS_SERVERS) {
    ok <- tryCatch({
      httr::status_code(httr::GET(s, query=list(data="[out:json];node(1);out;"),
                                  httr::timeout(10))) == 200
    }, error=function(e) FALSE)
    if (ok) { message("✓ Overpass : ", s); return(s) }
  }
  stop("Aucun serveur Overpass disponible")
}
overpass_url <- detecter_overpass()
osmdata::set_overpass_url(overpass_url)

requete_overpass <- function(key, value, bbox) {
  serveurs <- c(overpass_url, setdiff(OVERPASS_SERVERS, overpass_url))
  for (srv in serveurs) {
    osmdata::set_overpass_url(srv)
    for (i in 1:3) {
      res <- tryCatch({
        q <- opq(bbox=bbox, timeout=180) %>%
          add_osm_feature(key=key, value=value) %>% osmdata_sf()
        bind_rows(
          if (!is.null(q$osm_points))   q$osm_points   %>% select(geometry),
          if (!is.null(q$osm_polygons)) q$osm_polygons %>% st_centroid() %>% select(geometry)
        ) %>% mutate(categorie = paste0(key,"=",value))
      }, error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("connect|timeout|resolve|network|403|429|Forbidden|curl", msg, ignore.case=TRUE))
          return("RESEAU_KO")
        if (i < 3) { Sys.sleep(5*i); return("RETRY") }
        message(sprintf("  ⚠ Abandon %s=%s", key, value)); NULL
      })
      if (identical(res,"RESEAU_KO")) break
      if (!identical(res,"RETRY")) return(res)
    }
  }
  NULL
}

# Catégories de POIs à collecter
categories <- list(
  list(key="shop",    value="supermarket"),  list(key="shop",    value="convenience"),
  list(key="shop",    value="bakery"),        list(key="shop",    value="butcher"),
  list(key="amenity", value="marketplace"),   list(key="amenity", value="hospital"),
  list(key="amenity", value="clinic"),        list(key="amenity", value="doctors"),
  list(key="amenity", value="pharmacy"),      list(key="amenity", value="school"),
  list(key="amenity", value="college"),       list(key="amenity", value="university"),
  list(key="amenity", value="kindergarten"),  list(key="amenity", value="bank"),
  list(key="amenity", value="post_office"),   list(key="office",  value="government"),
  list(key="amenity", value="townhall"),      list(key="amenity", value="restaurant"),
  list(key="amenity", value="cafe"),          list(key="amenity", value="fast_food"),
  list(key="leisure", value="park"),          list(key="amenity", value="cinema"),
  list(key="amenity", value="theatre"),       list(key="leisure", value="sports_centre"),
  list(key="amenity", value="library"),       list(key="tourism",  value="museum")
)

dir_cache_pois <- file.path(DOSSIER_TRAVAIL, "pois_cache")
pois_liste     <- vector("list", length(categories))

for (i in seq_along(categories)) {
  cat    <- categories[[i]]
  cat_id <- paste0(cat$key, "_", cat$value)
  cache  <- file.path(dir_cache_pois, paste0(cat_id, ".rds"))
  if (file.exists(cache)) {
    pois_liste[[i]] <- readRDS(cache)
    message(sprintf("  ✓ Cache [%2d/%d] %s=%s", i, length(categories), cat$key, cat$value))
  } else {
    message(sprintf("  → Fetch [%2d/%d] %s=%s", i, length(categories), cat$key, cat$value))
    res <- requete_overpass(cat$key, cat$value, bbox_ville)
    pois_liste[[i]] <- res
    saveRDS(res, cache)
  }
}

pois_sf    <- bind_rows(Filter(Negate(is.null), pois_liste)) %>% st_as_sf(crs=4326)
pois_ville <- pois_sf[st_within(pois_sf, contour_wgs84, sparse=FALSE)[,1], ]
pois_h3    <- h3jsr::point_to_cell(pois_ville, res=RESOLUTION_H3, simple=TRUE)
nb_pois_hex <- data.table(h3_id=pois_h3)[!is.na(h3_id)][, .(nb_pois=.N), by=h3_id]
hexagones_sf <- hexagones_sf %>% left_join(nb_pois_hex, by="h3_id") %>%
  mutate(nb_pois = replace_na(nb_pois, 0L))
message(sprintf("✓ %d POIs", nrow(pois_ville)))


# -----------------------------------------------------------------------------
# 7. MATRICES DE TEMPS DE TRAJET (r5r)
# -----------------------------------------------------------------------------

message("\n═══ MATRICES r5r ═══")

reseau <- build_network(data_path = file.path(DOSSIER_TRAVAIL, "r5r_network"))
message("✓ Réseau construit")

pts_origines <- pts_od %>% filter(id %in% hexagones_habites$h3_id)
pts_dest     <- pts_od %>% filter(id %in% nb_pois_hex$h3_id)

score_opportunite <- function(ttm, nb_pois, tau, parking_min=0) {
  as.data.table(ttm) %>%
    left_join(nb_pois, by=c("to_id"="h3_id")) %>%
    mutate(nb_pois  = replace_na(nb_pois, 0L),
           t_ajuste = travel_time_p50 + parking_min,
           poids    = (1/tau) * exp(-(t_ajuste*2)/tau),
           contrib  = nb_pois * poids) %>%
    group_by(from_id) %>%
    summarise(score = sum(contrib, na.rm=TRUE), .groups="drop")
}

dir_cache_ttm <- file.path(DOSSIER_TRAVAIL, "ttm_cache")

calculer_ttm <- function(mode_label, h, dt, facteur) {
  cache <- file.path(dir_cache_ttm, sprintf("ttm_%s_h%02d.rds", mode_label, h))
  if (file.exists(cache)) {
    message(sprintf("    ✓ Cache %s h%02d", mode_label, h)); return(readRDS(cache))
  }
  message(sprintf("    → Calcul %s h%02d (f=%.2f)...", mode_label, h, facteur))
  res <- tryCatch({
    if (mode_label == "car")
      travel_time_matrix(r5r_network=reseau, origins=pts_origines, destinations=pts_dest,
                         mode="CAR", departure_datetime=dt,
                         max_trip_duration=TEMPS_MAX_MIN, carspeed_scale=facteur, progress=FALSE)
    else
      travel_time_matrix(r5r_network=reseau, origins=pts_origines, destinations=pts_dest,
                         mode=c("WALK","TRANSIT"), departure_datetime=dt,
                         max_trip_duration=TEMPS_MAX_MIN, max_walk_time=15, progress=FALSE)
  }, error=\(e) { message("    ⚠ ", mode_label, " h", h, " : ", e$message); NULL })
  saveRDS(res, cache); res
}

ttm_results <- map(HEURES_DEPART, function(h) {
  dt  <- as.POSIXct(sprintf("%s %02d:00:00", DATE_ANALYSE, h), tz="Europe/Paris")
  f   <- facteur_pour_heure(h)
  message(sprintf("  [h=%02d] congestion=%.0f%%  f=%.2f", h, (1/f-1)*100, f))
  list(car=calculer_ttm("car",h,dt,f), tc=calculer_ttm("tc",h,dt,f))
}) %>% setNames(paste0("h", sprintf("%02d", HEURES_DEPART)))

scores_car <- map(ttm_results, \(x) if(!is.null(x$car))
  score_opportunite(x$car, nb_pois_hex, TAU, PARKING_MIN) else NULL)
scores_pt  <- map(ttm_results, \(x) if(!is.null(x$tc))
  score_opportunite(x$tc,  nb_pois_hex, TAU, 0) else NULL)

r5r::stop_r5(reseau); rJava::.jgc(R.gc=TRUE)
message("✓ Matrices calculées")


# -----------------------------------------------------------------------------
# 8. CDI
# -----------------------------------------------------------------------------

message("\n═══ CDI ═══")

O_car <- bind_rows(purrr::compact(scores_car)) %>% group_by(from_id) %>%
  summarise(O_car=mean(score,na.rm=TRUE),.groups="drop") %>% rename(h3_id=from_id)
O_pt  <- bind_rows(purrr::compact(scores_pt)) %>% group_by(from_id) %>%
  summarise(O_pt =mean(score,na.rm=TRUE),.groups="drop") %>% rename(h3_id=from_id)

resultats <- hexagones_habites %>%
  left_join(O_car, by="h3_id") %>%
  left_join(O_pt,  by="h3_id") %>%
  left_join(nb_pois_hex, by="h3_id") %>%
  mutate(O_car=replace_na(O_car,0), O_pt=replace_na(O_pt,0),
         CDI=if_else((O_car+O_pt)>0, (O_car-O_pt)/(O_car+O_pt), NA_real_))

cdi_global <- resultats %>% st_drop_geometry() %>% filter(!is.na(CDI)) %>%
  summarise(CDI_moy_pondere=weighted.mean(CDI,population),
            CDI_min=min(CDI), CDI_max=max(CDI),
            pop_analysee=sum(population), n_hex=n())

quintiles <- resultats %>% st_drop_geometry() %>% filter(!is.na(CDI)) %>%
  arrange(CDI) %>%
  mutate(cum_pop_pct=cumsum(population)/sum(population)) %>%
  mutate(quintile=cut(cum_pop_pct, breaks=c(0,.2,.4,.6,.8,1),
                      labels=c("Q1 – Très faible","Q2 – Faible","Q3 – Modérée",
                               "Q4 – Forte","Q5 – Très forte"),
                      include.lowest=TRUE)) %>%
  group_by(quintile) %>%
  summarise(CDI_moyen=round(weighted.mean(CDI,population),4),
            population=sum(population), n_hexagones=n())

message(sprintf("✓ CDI %s = %+.4f (pop. %s)",
                NOM_VILLE, cdi_global$CDI_moy_pondere,
                format(cdi_global$pop_analysee, big.mark=" ")))


# -----------------------------------------------------------------------------
# 9. EXPORTS
# -----------------------------------------------------------------------------

message("\n═══ EXPORTS ═══")

dossier_sorties <- file.path(DOSSIER_TRAVAIL, "sorties")

vars_export <- intersect(
  c("h3_id","population","nb_pois","O_car","O_pt","CDI",
    "taux_pauvrete","ind_snv","men_pauv","men",
    "indice_vieillissement","pop_jeunes","pop_ages"),
  names(resultats))

st_write(resultats %>% select(all_of(vars_export)),
         file.path(dossier_sorties, paste0(NOM_VILLE,"_CDI_resultats.gpkg")),
         driver="GPKG", delete_dsn=TRUE, quiet=TRUE)

write.csv(resultats %>% st_drop_geometry() %>% select(all_of(vars_export)),
          file.path(dossier_sorties, paste0(NOM_VILLE,"_CDI_hexagones.csv")),
          row.names=FALSE)
write.csv(quintiles,
          file.path(dossier_sorties, paste0(NOM_VILLE,"_CDI_quintiles.csv")),
          row.names=FALSE)
write.csv(data.frame(
  ville=NOM_VILLE, code_insee=CODE_INSEE, date_analyse=DATE_ANALYSE,
  resolution_h3=RESOLUTION_H3, tau_min=TAU, parking_min=PARKING_MIN,
  n_heures=length(HEURES_DEPART), n_hexagones_habites=nrow(hexagones_habites),
  n_pois=nrow(pois_ville), population=cdi_global$pop_analysee,
  CDI_moyen_pondere=round(cdi_global$CDI_moy_pondere,4),
  CDI_min=round(cdi_global$CDI_min,4), CDI_max=round(cdi_global$CDI_max,4),
  correction_congestion=congestion_disponible),
  file.path(dossier_sorties, paste0(NOM_VILLE,"_CDI_synthese.csv")),
  row.names=FALSE)

message(sprintf("✓ Exports dans : %s", dossier_sorties))


# -----------------------------------------------------------------------------
# 10. RAPPORT RMARKDOWN (optionnel)
# -----------------------------------------------------------------------------

rmd_template <- "rapport_cdi_france.Rmd"

if (file.exists(rmd_template)) {
  rmd_output <- file.path(dossier_sorties, paste0(NOM_VILLE, "_CDI_rapport.html"))
  rmarkdown::render(
    input       = rmd_template,
    output_file = rmd_output,
    params = list(
      gpkg_path     = file.path(dossier_sorties, paste0(NOM_VILLE,"_CDI_resultats.gpkg")),
      csv_hex       = file.path(dossier_sorties, paste0(NOM_VILLE,"_CDI_hexagones.csv")),
      csv_quintiles = file.path(dossier_sorties, paste0(NOM_VILLE,"_CDI_quintiles.csv")),
      csv_synthese  = file.path(dossier_sorties, paste0(NOM_VILLE,"_CDI_synthese.csv")),
      nom_ville     = NOM_VILLE,
      date_analyse  = DATE_ANALYSE,
      tau           = TAU,
      parking_min   = PARKING_MIN
    ), quiet=FALSE)
  message("✓ Rapport : ", rmd_output)
} else {
  message("⚠ rapport_cdi_france.Rmd introuvable — étape ignorée.")
}

message(sprintf(
  "\n══════════════════════════════════════\n  CDI %s = %+.4f\n══════════════════════════════════════",
  NOM_VILLE, cdi_global$CDI_moy_pondere))
