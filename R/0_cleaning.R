# =============================================================================
# SCRIPT 0 : NETTOYAGE DES DONNÉES
# =============================================================================
# Ce script nettoie les données brutes d'avis Google du restaurant "La Ligne Rouge".
# Il importe le fichier Excel brut, filtre pour ne garder que les avis pertinents,
# retire les colonnes inutiles et enregistre le résultat dans un fichier RDS
# pour utilisation dans les scripts d'analyse suivants.
# =============================================================================

# -----------------------------------------------------------------------------
# 1. CHARGEMENT DES BIBLIOTHÈQUES
# -----------------------------------------------------------------------------
library(dplyr)     # Pour la manipulation des données
library(readxl)    # Pour lire les fichiers Excel

# -----------------------------------------------------------------------------
# 2. IMPORTATION ET NETTOYAGE DES DONNÉES
# -----------------------------------------------------------------------------
# Lecture du fichier Excel brut contenant les avis Google
df <- read_xlsx("data/ligne_rouge_raw.xlsx") %>%
  # Filtrer pour ne garder que les avis du restaurant "La Ligne Rouge"
  filter(name == "La Ligne Rouge") %>% 
  # Supprimer toutes les colonnes non nécessaires pour l'analyse
  select(
    -query,                                  # Terme de recherche utilisé
    -google_id,                              # Identifiant Google
    -place_id,                               # Identifiant de l'établissement
    -location_link,                          # Lien vers l'emplacement
    -reviews_link,                           # Lien vers les avis
    -reviews,                                # Nombre total d'avis
    -review_id,                              # Identifiant de l'avis
    -review_pagination_id,                   # Identifiant de pagination
    -author_link,                            # Lien vers le profil de l'auteur
    -author_id,                              # Identifiant de l'auteur
    -author_image,                           # Image de profil de l'auteur
    -review_img_urls,                        # URLs des images dans l'avis
    -review_img_url,                         # URL de l'image principale
    -review_questions,                       # Questions dans l'avis
    -review_photo_ids,                       # Identifiants des photos
    -owner_answer_timestamp,                 # Horodatage de la réponse du propriétaire
    -owner_answer_timestamp_datetime_utc,    # Horodatage UTC de la réponse
    -review_link,                            # Lien vers l'avis
    -review_timestamp,                       # Horodatage de l'avis
    -review_datetime_utc,                    # Horodatage UTC de l'avis
    -review_likes,                           # Nombre de "j'aime" sur l'avis
    -reviews_id,                             # Identifiant des avis
    -reviews_per_score_1,                    # Nombre d'avis avec score 1
    -reviews_per_score_2,                    # Nombre d'avis avec score 2
    -reviews_per_score_3,                    # Nombre d'avis avec score 3
    -reviews_per_score_4,                    # Nombre d'avis avec score 4
    -reviews_per_score_5,                    # Nombre d'avis avec score 5
    -starts_with("review_question")          # Toutes les colonnes liées aux questions
  ) %>%
  # Filtrer pour retirer les avis sans texte (vides)
  filter(!is.na(review_text))

# -----------------------------------------------------------------------------
# 3. SAUVEGARDE DES DONNÉES NETTOYÉES
# -----------------------------------------------------------------------------
# Enregistrement du dataframe nettoyé au format RDS
saveRDS(df, "data/ligne_rouge_cleaned.rds")
