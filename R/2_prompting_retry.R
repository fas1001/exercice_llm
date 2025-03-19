# =============================================================================
# SCRIPT 2 : RÉESSAI D'ANALYSE DES AVIS MANQUÉS
# =============================================================================
# Ce script reprend les avis clients qui n'ont pas été correctement analysés 
# lors du premier passage (script 1_prompting.R) et fait plusieurs tentatives
# pour extraire les informations manquantes à l'aide du modèle de langage (LLM).
# Cette approche permet d'améliorer le taux de réussite global en réessayant
# l'analyse des avis problématiques avec plusieurs tentatives successives.
# =============================================================================

# -----------------------------------------------------------------------------
# 1. INITIALISATION ET VÉRIFICATION
# -----------------------------------------------------------------------------
# Chargement des bibliothèques nécessaires
library(dplyr)      # Pour la manipulation des données

# Lecture du fichier de données partiellement analysées lors du premier passage
donnees <- readRDS("data/ligne_rouge_prompted.rds")

# Identifier les avis qui ont besoin d'être réanalysés
# (ceux où la langue ou le sentiment n'a pas été correctement extrait)
a_reessayer <- is.na(donnees$language) | is.na(donnees$sentiment)
nombre_reessais <- sum(a_reessayer)
cat("Nombre d'avis à réanalyser:", nombre_reessais, "\n")

# Si tous les avis ont déjà été analysés avec succès, on peut terminer le script
if (nombre_reessais == 0) {
  cat("Aucun avis à réanalyser. Le script se termine.\n")
  quit(save = "no")
}

# -----------------------------------------------------------------------------
# 2. CONFIGURATION DE L'API DU MODÈLE DE LANGAGE
# -----------------------------------------------------------------------------
# Définition du rôle du système pour le LLM (identique au script original)
system_prompt <- "Your role is to analyze the sentiment of restaurant reviews and classify them according to specific categories"

# Initialisation de la connexion avec l'API DeepSeek
groq <- ellmer::chat_deepseek(
  system_prompt = system_prompt,
  model = "deepseek-chat"
)

# -----------------------------------------------------------------------------
# 3. PARAMÈTRES DE RÉESSAI
# -----------------------------------------------------------------------------
# Nombre maximum de tentatives pour chaque avis problématique
max_tentatives <- 3  # On essaiera jusqu'à 3 fois par avis

# Compteur pour suivre la progression
compteur_reessais <- 0

# -----------------------------------------------------------------------------
# 4. BOUCLE PRINCIPALE DE RÉANALYSE
# -----------------------------------------------------------------------------
# Parcourir tous les avis et réanalyser uniquement ceux qui ont échoué
for (i in 1:nrow(donnees)) {
  # Vérifier si cet avis a besoin d'être réanalysé
  if (is.na(donnees$language[i]) || is.na(donnees$sentiment[i])) {
    # Mise à jour et affichage de la progression
    compteur_reessais <- compteur_reessais + 1
    cat(sprintf("Réessai %d/%d - Avis #%d\n", compteur_reessais, nombre_reessais, i))
    
    # -----------------------------------------------------------------------------
    # 5. TENTATIVES MULTIPLES POUR CHAQUE AVIS
    # -----------------------------------------------------------------------------
    # Boucle de tentatives successives pour un même avis
    for (tentative in 1:max_tentatives) {
      cat(sprintf("  Tentative %d/%d\n", tentative, max_tentatives))
      
      # Construction du prompt avec l'avis à analyser (identique au script original)
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
        
        "Review: ", donnees$review_text[i]
      )
      
      # -----------------------------------------------------------------------------
      # 6. APPEL À L'API ET GESTION DES ERREURS
      # -----------------------------------------------------------------------------
      # Envoi de la requête à l'API avec gestion d'erreur
      response <- tryCatch({
        groq$chat(prompt)
      }, error = function(e) {
        warning("Erreur d'appel à l'API pour l'avis ", i, " (tentative ", tentative, "): ", e$message)
        return(NULL)
      })
      
      # Vérification que la réponse existe avant de continuer
      if (!is.null(response)) {
        # Conversion (parsing) de la réponse JSON en objet R
        response_parsed <- tryCatch({
          jsonlite::fromJSON(response)
        }, error = function(e) {
          warning("Erreur de parsing JSON pour l'avis ", i, " (tentative ", tentative, "): ", e$message)
          return(NULL)
        })
        
        # -----------------------------------------------------------------------------
        # 7. VALIDATION ET STOCKAGE DES RÉSULTATS
        # -----------------------------------------------------------------------------
        # Vérification plus stricte que le premier script: on s'assure que les champs obligatoires existent
        if (!is.null(response_parsed) && !is.null(response_parsed$language) && !is.null(response_parsed$sentiment)) {
          # Stockage des valeurs clés
          donnees$language[i] <- response_parsed$language
          donnees$sentiment[i] <- response_parsed$sentiment
          
          # Transformation des listes en chaînes pour stockage dans le dataframe
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
          
          # Si le traitement a réussi, sortir de la boucle des tentatives
          cat("  Réussi!\n")
          break  # Passer à l'avis suivant
        } else {
          cat("  Échec du parsing ou données incomplètes, nouvelle tentative...\n")
        }
      } else {
        cat("  Échec de l'API, nouvelle tentative...\n")
      }
      
      # Si c'est la dernière tentative et qu'elle a échoué, enregistrer l'échec
      if (tentative == max_tentatives) {
        cat("  Toutes les tentatives ont échoué pour cet avis.\n")
      } else {
        # Pause plus longue entre les tentatives pour éviter les problèmes d'API
        Sys.sleep(5)  # Attendre 5 secondes entre chaque tentative
      }
    }
    
    # Pause pour respecter les limites de l'API entre chaque avis
    Sys.sleep(2)
  }
}

# -----------------------------------------------------------------------------
# 8. STATISTIQUES ET SAUVEGARDE FINALE
# -----------------------------------------------------------------------------
# Calcul du taux de réussite final après les réessais
nombre_analyses_reussies <- sum(!is.na(donnees$language) & !is.na(donnees$sentiment))
pourcentage_reussite <- nombre_analyses_reussies / nrow(donnees) * 100

# Affichage du résultat
cat("\nAnalyse terminée après réessais!\n")
cat("Nombre d'avis analysés avec succès:", nombre_analyses_reussies, "/", nrow(donnees), 
    sprintf("(%.1f%%)\n", pourcentage_reussite))

# Sauvegarde des résultats finaux dans un nouveau fichier
saveRDS(donnees, "data/ligne_rouge_prompted_final.rds")
