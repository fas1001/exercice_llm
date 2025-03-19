# =============================================================================
# SCRIPT 1 : ANALYSE DES AVIS AVEC UN MODÈLE DE LANGAGE (LLM)
# =============================================================================
# Ce script utilise l'API DeepSeek pour analyser 
# automatiquement le contenu des avis clients du restaurant "La Ligne Rouge".
# Pour chaque avis, le LLM extrait:
# - La langue de l'avis (français ou anglais)
# - Les sujets mentionnés (nourriture, service, ambiance, etc.)
# - Le sentiment général (de -1 très négatif à +1 très positif)
# - Les recommandations pour améliorer le restaurant
# - Les points forts et points faibles identifiés
# =============================================================================

# -----------------------------------------------------------------------------
# 1. CONFIGURATION INITIALE
# -----------------------------------------------------------------------------
# Chargement des bibliothèques nécessaires
library(dplyr)      # Pour la manipulation des données
library(jsonlite)   # Pour traiter les réponses JSON du LLM (implicitement chargé)

# Importation des données nettoyées
donnees <- readRDS("data/ligne_rouge_cleaned.rds")

# Pour la démonstration, limiter à 100 avis
# Supprimer cette ligne pour analyser l'ensemble complet des données
donnees <- donnees %>% slice(1:100)

# -----------------------------------------------------------------------------
# 2. PRÉPARATION DU DATAFRAME POUR LES RÉSULTATS
# -----------------------------------------------------------------------------
# Ajout des colonnes qui contiendront les résultats de l'analyse
donnees$language <- NA        # Langue de l'avis (français ou anglais)
donnees$sentiment <- NA       # Score de sentiment (de -1 à +1)
donnees$topics <- NA          # Sujets abordés dans l'avis
donnees$recommendations <- NA # Suggestions d'amélioration
donnees$strengths <- NA       # Points forts identifiés
donnees$weaknesses <- NA      # Points faibles identifiés

# -----------------------------------------------------------------------------
# 3. CONFIGURATION DE L'API DU MODÈLE DE LANGAGE
# -----------------------------------------------------------------------------
# Définition du rôle du système pour le LLM
system_prompt <- "Your role is to analyze the sentiment of restaurant reviews and classify them according to specific categories"

# Initialisation de la connexion avec l'API DeepSeek via le package ellmer
# (Nécessite une clé API configurée dans l'environnement)
deepseek <- ellmer::chat_deepseek(
  system_prompt = system_prompt,
  model = "deepseek-chat"     # Utilisation du modèle DeepSeek
)

# -----------------------------------------------------------------------------
# 4. TRAITEMENT DE CHAQUE AVIS AVEC LE LLM
# -----------------------------------------------------------------------------
# Boucle principale: traitement de chaque avis un par un
for (i in 1:nrow(donnees)) {
  # Construction du prompt (instruction) pour le LLM
  # Ce prompt demande au modèle d'analyser l'avis et de retourner les informations
  # structurées dans un format JSON précis
  prompt <- paste0(
    "Analyze this restaurant review (which may be in either English or French) and extract the following information in JSON format:\n\n",
    
    "1. LANGUAGE: Identify whether the review is in English or French\n",
    "2. TOPICS: List only the most relevant topics mentioned from these categories: food quality, service, ambiance, cleanliness, price, portion size, wait time, menu variety, accessibility, parking, other\n",
    "3. SENTIMENT: Rate the overall sentiment from -1 (very negative) to 1 (very positive)\n",
    "4. RECOMMENDATIONS: Extract specific suggestions for improvement\n",
    "5. STRENGTHS: Identify what the restaurant is doing well\n",
    "6. WEAKNESSES: Identify specific areas where the restaurant is underperforming\n\n",
    
    "IMPORTANT: Regardless of the review's language, ALWAYS provide your analysis in English.\n\n",
    
    "Response must be ONLY valid JSON with no additional text. Use this exact format:\n",
    "{\n",
    "  \"language\": \"english OR french\",\n",
    "  \"topics\": [\"example_topic1\", \"example_topic2\"],\n",
    "  \"sentiment\": 0.5,\n",
    "  \"recommendations\": [\"Example improvement suggestion 1\", \"Example suggestion 2\"],\n",
    "  \"strengths\": [\"Example strength 1\", \"Example strength 2\"],\n",
    "  \"weaknesses\": [\"Example weakness 1\", \"Example weakness 2\"]\n",
    "}\n\n",
    
    "If a category has no relevant information, use an empty array [].\n",
    "For sentiment, use only one decimal place of precision.\n\n",
    
    "Review: ", donnees$review_text[i]  # Ajout du texte de l'avis à analyser
  )
  
  # -----------------------------------------------------------------------------
  # 5. APPEL À L'API ET GESTION DES ERREURS
  # -----------------------------------------------------------------------------
  # Envoi de la requête à l'API DeepSeek avec gestion d'erreur
  response <- tryCatch({
    # Appel de l'API - envoie le prompt et récupère la réponse
    deepseek$chat(prompt)
  }, error = function(e) {
    # En cas d'erreur, afficher un avertissement et continuer
    warning("Erreur d'appel à l'API pour l'avis ", i, ": ", e$message)
    return(NULL)  # Retourner NULL pour indiquer une erreur
  })
  
  # Vérification que la réponse existe avant de la traiter
  if (!is.null(response)) {
    # Conversion (parsing) de la réponse JSON en objet R
    response_parsed <- tryCatch({
      jsonlite::fromJSON(response)
    }, error = function(e) {
      # En cas d'erreur de parsing, afficher un avertissement
      warning("Erreur de parsing JSON pour l'avis ", i, ": ", e$message)
      return(NULL)
    })
    
    # -----------------------------------------------------------------------------
    # 6. STOCKAGE DES RÉSULTATS DANS LE DATAFRAME
    # -----------------------------------------------------------------------------
    # Si le parsing a réussi, enregistrer les résultats
    if (!is.null(response_parsed)) {
      # Stockage des valeurs simples
      donnees$language[i] <- response_parsed$language
      donnees$sentiment[i] <- response_parsed$sentiment
      
      # Transformation des listes en chaînes de caractères pour stockage
      # (avec gestion des cas où l'information est absente)
      donnees$topics[i] <- if(length(response_parsed$topics) > 0) {
        paste(response_parsed$topics, collapse = ", ")
      } else {
        NA
      }
      
      donnees$recommendations[i] <- if(length(response_parsed$recommendations) > 0) {
        paste(response_parsed$recommendations, collapse = ", ")
      } else {
        NA
      }
      
      donnees$strengths[i] <- if(length(response_parsed$strengths) > 0) {
        paste(response_parsed$strengths, collapse = ", ")
      } else {
        NA
      }
      
      donnees$weaknesses[i] <- if(length(response_parsed$weaknesses) > 0) {
        paste(response_parsed$weaknesses, collapse = ", ")
      } else {
        NA
      }
    }
  }
  
  # Pause pour respecter les limites de l'API (rate limiting)
  # Évite de faire trop de requêtes en trop peu de temps
  Sys.sleep(2)  # Attendre 2 secondes entre chaque requête
}

# -----------------------------------------------------------------------------
# 7. SAUVEGARDE DES RÉSULTATS
# -----------------------------------------------------------------------------
# Enregistrement du dataframe avec les résultats d'analyse
saveRDS(donnees, "data/ligne_rouge_prompted.rds")
