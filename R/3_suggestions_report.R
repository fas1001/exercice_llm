# =============================================================================
# SCRIPT 3 : GÉNÉRATION DE RAPPORT AVEC UN LLM
# =============================================================================
# Ce script utilise les avis clients analysés pour générer un rapport complet
# et professionnel pour la direction du restaurant "La Ligne Rouge".
# Il combine:
# - L'analyse statistique des données d'avis clients
# - L'utilisation d'un LLM pour interpréter ces données et rédiger des analyses
# - La création d'un document Quarto avec visualisations et analyses textuelles
# - La génération automatique d'un rapport final en PDF
# =============================================================================

# -----------------------------------------------------------------------------
# 1. CHARGEMENT DES BIBLIOTHÈQUES
# -----------------------------------------------------------------------------
library(dplyr)        # Pour la manipulation des données
library(ggplot2)      # Pour les graphiques
library(stringr)      # Pour le traitement des chaînes de caractères
library(tidyr)        # Pour restructurer les données
library(quarto)       # Pour générer le rapport PDF
library(wordcloud)    # Pour visualisations de nuages de mots (optionnel)
library(RColorBrewer) # Pour les palettes de couleurs
library(knitr)        # Pour les tableaux
library(jsonlite)     # Pour le traitement JSON

# -----------------------------------------------------------------------------
# 2. CHARGEMENT ET PRÉPARATION DES DONNÉES
# -----------------------------------------------------------------------------
# Lecture des données d'avis complètement analysées
donnees <- readRDS("data/ligne_rouge_prompted_final.rds")

# -----------------------------------------------------------------------------
# 3. CONFIGURATION DES MODÈLES DE LANGAGE POUR L'ANALYSE
# -----------------------------------------------------------------------------
# Définition du rôle du système pour les LLMs (analyste restauration)
system_prompt <- "You are a professional restaurant business analyst who helps restaurant owners understand customer feedback data and provide strategic recommendations for improvement"

# Initialisation des connexions aux APIs de modèles de langage
gpt <- ellmer::chat_openai(
  system_prompt = system_prompt,
  model = "gpt-4o"  # Modèle avancé pour les analyses détaillées
)

# -----------------------------------------------------------------------------
# 4. PRÉPARATION DES STATISTIQUES DESCRIPTIVES
# -----------------------------------------------------------------------------
# Calcul du nombre total d'avis analysés
total_reviews <- nrow(donnees)

# Statistiques sur le sentiment client
sentiment_mean <- mean(donnees$sentiment, na.rm = TRUE)  # Moyenne des scores de sentiment
sentiment_median <- median(donnees$sentiment, na.rm = TRUE)  # Médiane des scores
# Répartition des sentiments par catégorie
sentiment_distribution <- table(cut(donnees$sentiment, 
                                    breaks = c(-1, -0.5, 0, 0.5, 1), 
                                    labels = c("Very Negative", "Negative", "Neutral", "Positive")))

# Analyse des langues utilisées dans les avis
languages <- table(donnees$language)

# -----------------------------------------------------------------------------
# 5. ANALYSE DES SUJETS MENTIONNÉS
# -----------------------------------------------------------------------------
# Extraction et préparation de tous les sujets mentionnés dans les avis
topics_list <- donnees %>%
  filter(!is.na(topics)) %>%     # Exclure les avis sans sujets identifiés
  pull(topics) %>%               # Extraire la colonne des sujets
  paste(collapse = ", ") %>%     # Concaténer tous les sujets
  str_split(", ") %>%            # Séparer chaque sujet individuel
  unlist() %>%                   # Aplatir la liste
  str_trim()                     # Supprimer les espaces superflus

# Comptage des occurrences de chaque sujet
topics_count <- as.data.frame(table(topics_list))
# Tri par fréquence décroissante
topics_count <- topics_count[order(-topics_count$Freq), ]
# Renommage des colonnes pour plus de clarté
names(topics_count) <- c("Topic", "Count")

# Sélection des 10 sujets les plus mentionnés pour le graphique
top_topics <- head(topics_count, 10)

# -----------------------------------------------------------------------------
# 6. FONCTIONS UTILITAIRES POUR LE TRAITEMENT DES DONNÉES
# -----------------------------------------------------------------------------
# Fonction pour préparer les listes d'éléments à partir des colonnes du dataframe
# (utilisée pour les forces, faiblesses et recommandations)
prepare_list_data <- function(column_data) {
  # Remplacer les NA par des chaînes vides
  column_data[is.na(column_data)] <- ""
  
  # Extraire et traiter tous les éléments
  all_items <- column_data %>% 
    paste(collapse = ", ") %>%   # Concaténer tous les éléments
    str_split(", ") %>%          # Séparer les éléments individuels
    unlist() %>%                 # Aplatir la liste
    str_trim()                   # Supprimer les espaces
  
  # Ne garder que les éléments non vides
  all_items <- all_items[all_items != ""]
  return(all_items)
}

# Fonction pour nettoyer les textes et éviter les problèmes de caractères spéciaux
clean_text <- function(text_vector) {
  # Remplacer les caractères qui posent problème pour le PDF
  cleaned <- gsub("'", "", text_vector)   # Enlever les apostrophes simples
  cleaned <- gsub('"', "", cleaned)       # Enlever les guillemets doubles
  cleaned <- gsub(',', "", cleaned)       # Enlever les virgules
  cleaned <- gsub('\\(', "", cleaned)     # Enlever les parenthèses ouvrantes
  cleaned <- gsub('\\)', "", cleaned)     # Enlever les parenthèses fermantes
  return(cleaned)
}

# -----------------------------------------------------------------------------
# 7. PRÉPARATION DES DONNÉES POUR LES TABLEAUX ET ANALYSES
# -----------------------------------------------------------------------------
# Extraire et traiter les forces, faiblesses et recommandations
strengths_list <- prepare_list_data(donnees$strengths)
weaknesses_list <- prepare_list_data(donnees$weaknesses)
recommendations_list <- prepare_list_data(donnees$recommendations)

# Nettoyage des textes pour éviter les problèmes dans le document
if(length(strengths_list) > 0) {
  strengths_list <- clean_text(strengths_list)
}
if(length(weaknesses_list) > 0) {
  weaknesses_list <- clean_text(weaknesses_list)
}

# Calcul des tableaux de fréquence pour les principaux éléments
# Top 10 des forces identifiées
top_strengths <- sort(table(strengths_list), decreasing = TRUE)
top_strengths <- head(top_strengths, min(10, length(top_strengths)))

# Top 10 des faiblesses identifiées
top_weaknesses <- sort(table(weaknesses_list), decreasing = TRUE)
top_weaknesses <- head(top_weaknesses, min(10, length(top_weaknesses)))

# Top 10 des recommandations des clients
top_recommendations <- sort(table(recommendations_list), decreasing = TRUE)
top_recommendations <- head(top_recommendations, min(10, length(top_recommendations)))

# -----------------------------------------------------------------------------
# 8. FONCTION POUR APPELS API SÉCURISÉS
# -----------------------------------------------------------------------------
# Cette fonction permet d'appeler l'API du LLM avec gestion des erreurs
# et génération de contenu de secours en cas d'échec
safe_api_call <- function(prompt, retry_count = 3, sleep_time = 2) {
  for (i in 1:retry_count) {
    tryCatch({
      # Essayer l'appel API avec GPT-4o
      response <- gpt$chat(prompt)
      
      # Vérifier que la réponse est valide
      if (is.null(response) || response == "" || response == "NA") {
        warning("Réponse vide reçue, nouvelle tentative...")
      } else {
        return(response)
      }
    }, error = function(e) {
      warning("Erreur API (tentative ", i, "/", retry_count, "): ", e$message)
    })
    
    # Pause avant la prochaine tentative
    if (i < retry_count) {
      Sys.sleep(sleep_time)
    }
  }
  
  # Si toutes les tentatives échouent, générer une analyse par défaut
  return(generate_fallback_analysis(prompt))
}

# -----------------------------------------------------------------------------
# 9. FONCTION DE GÉNÉRATION D'ANALYSE PAR DÉFAUT
# -----------------------------------------------------------------------------
# Cette fonction crée un contenu d'analyse de secours basé sur les statistiques
# si tous les appels API échouent
generate_fallback_analysis <- function(prompt) {
  # Identifier le type d'analyse demandée à partir du prompt
  if (grepl("sentiment", prompt, ignore.case = TRUE)) {
    # Analyse de sentiment par défaut
    return(paste0(
      "# Customer Sentiment Analysis\n\n",
      "The analysis of ", total_reviews, " customer reviews for La Ligne Rouge reveals an overall sentiment score of ", 
      round(sentiment_mean, 2), " on a scale from -1 to 1, indicating a general ", 
      ifelse(sentiment_mean > 0.5, "very positive", 
             ifelse(sentiment_mean > 0, "positive", 
                    ifelse(sentiment_mean > -0.5, "negative", "very negative"))), 
      " perception of the restaurant.\n\n",
      "The sentiment distribution shows that ", sentiment_distribution[4], " reviews are positive (", 
      round(sentiment_distribution[4]/total_reviews*100), "%), while ", 
      sentiment_distribution[1] + sentiment_distribution[2], 
      " reviews are negative or very negative (", 
      round((sentiment_distribution[1] + sentiment_distribution[2])/total_reviews*100), "%).\n\n",
      "These results suggest that the majority of customers are satisfied with their experience at La Ligne Rouge, ",
      "but there are opportunities for improvement to address concerns raised in the negative reviews."
    ))
  } else if (grepl("topics", prompt, ignore.case = TRUE)) {
    # Analyse des sujets par défaut
    topics_text <- paste0(
      "# Analysis of Mentioned Topics\n\n",
      "Customers of La Ligne Rouge most frequently mention the following topics in their reviews:\n\n"
    )
    
    for (i in 1:nrow(top_topics)) {
      topics_text <- paste0(
        topics_text,
        "- **", top_topics$Topic[i], "** (", top_topics$Count[i], " mentions): This topic represents ", 
        round(top_topics$Count[i]/sum(top_topics$Count)*100, 1), "% of the main topics mentioned.\n"
      )
    }
    
    topics_text <- paste0(
      topics_text, 
      "\nThis distribution of topics suggests that customers place particular importance on ",
      top_topics$Topic[1], " and ", top_topics$Topic[2], 
      ", which constitute the most commented aspects of the restaurant experience."
    )
    
    return(topics_text)
  } else if (grepl("strengths.*weaknesses", prompt, ignore.case = TRUE)) {
    # Analyse des forces et faiblesses par défaut
    strengths_text <- "## Main Strengths\n\nCustomers particularly appreciate:\n\n"
    for (i in 1:length(top_strengths)) {
      strengths_text <- paste0(
        strengths_text,
        "- **", names(top_strengths)[i], "** (mentioned ", top_strengths[i], " times)\n"
      )
    }
    
    weaknesses_text <- "\n\n## Main Weaknesses\n\nAreas for improvement according to customers are:\n\n"
    for (i in 1:length(top_weaknesses)) {
      weaknesses_text <- paste0(
        weaknesses_text,
        "- **", names(top_weaknesses)[i], "** (mentioned ", top_weaknesses[i], " times)\n"
      )
    }
    
    analysis_text <- paste0(
      strengths_text, 
      weaknesses_text,
      "\n\nThe analysis shows that the restaurant should capitalize on its recognized strengths while working ",
      "to improve the weaknesses identified by customers. Special attention should be paid to ",
      names(top_weaknesses)[1], " and ", names(top_weaknesses)[2], ", which are the most frequently cited weaknesses."
    )
    
    return(analysis_text)
  } else if (grepl("recommendations", prompt, ignore.case = TRUE)) {
    # Analyse des recommandations par défaut
    reco_text <- "## Main Customer Recommendations\n\nCustomers suggest the following improvements:\n\n"
    for (i in 1:length(top_recommendations)) {
      reco_text <- paste0(
        reco_text,
        "- **", names(top_recommendations)[i], "** (suggested ", top_recommendations[i], " times)\n"
      )
    }
    
    action_plan <- paste0(
      "\n\n## Priority Action Plan\n\n",
      "Based on customer reviews, here are the recommended priority actions:\n\n",
      "1. **", names(top_recommendations)[1], "** - Being the most frequent recommendation, this should be addressed as a priority.\n",
      "2. **", names(top_recommendations)[2], "** - Represents a significant opportunity to improve customer experience.\n",
      "3. **", ifelse(length(top_recommendations) >= 3, names(top_recommendations)[3], "General service improvement"), 
      "** - Would address an important customer expectation.\n\n",
      "Implementing these improvements should have a positive impact on overall customer satisfaction."
    )
    
    return(paste0(reco_text, action_plan))
  } else {
    # Analyse générique par défaut
    return("The data analysis shows interesting trends in customer reviews that deserve management attention. Detailed recommendations based on this data would significantly improve the customer experience.")
  }
}

# -----------------------------------------------------------------------------
# 10. CRÉATION DU DOSSIER POUR LE RAPPORT
# -----------------------------------------------------------------------------
# Créer le dossier docs/ s'il n'existe pas
if (!dir.exists("docs")) {
  dir.create("docs")
}

# -----------------------------------------------------------------------------
# 11. GÉNÉRATION DU FICHIER QUARTO
# -----------------------------------------------------------------------------
# Création du fichier Quarto pour le rapport
report_file <- "docs/restaurant_report.qmd"

# En-tête du document avec métadonnées
cat("---\ntitle: \"Customer Review Analysis - La Ligne Rouge\"\ndate: \"", format(Sys.Date(), "%B %d, %Y"), "\"\nformat: pdf\n---\n\n", file = report_file)

# Ajout d'une introduction au rapport
cat("# Customer Review Analysis Report - La Ligne Rouge\n\n", append = TRUE, file = report_file)
cat("This report was automatically generated from the analysis of ", total_reviews, " customer reviews.\n\n", append = TRUE, file = report_file)

# -----------------------------------------------------------------------------
# 12. CRÉATION DES VISUALISATIONS DE DONNÉES
# -----------------------------------------------------------------------------
# Section des visualisations
cat("## Data Visualizations\n\n", append = TRUE, file = report_file)

# 12.1 Graphique de distribution des sentiments
cat("### Sentiment Distribution\n\n```{r sentiment-distribution, echo=FALSE, fig.height=4, fig.width=8}\n", append = TRUE, file = report_file)
cat("barplot(c(", 
    paste(sentiment_distribution, collapse = ", "), 
    "), names.arg = c('Very Negative', 'Negative', 'Neutral', 'Positive'),
    main = 'Sentiment Distribution', 
    xlab = 'Sentiment Category', 
    ylab = 'Number of Reviews',
    col = c('#FF6B6B', '#FFD166', '#06D6A0', '#118AB2'))\n", 
    append = TRUE, file = report_file)
cat("```\n\n", append = TRUE, file = report_file)

# 12.2 Graphique des 10 sujets les plus mentionnés
cat("### Top Mentioned Topics\n\n```{r top-topics, echo=FALSE, fig.height=5, fig.width=8, message=FALSE, warning=FALSE}\n", append = TRUE, file = report_file)
cat("library(ggplot2)\n", append = TRUE, file = report_file)
cat("top_topics <- data.frame(\n", append = TRUE, file = report_file)
cat("  Topic = c(", paste(paste0("\"", gsub("'", "", top_topics$Topic), "\""), collapse = ", "), "),\n", append = TRUE, file = report_file)
cat("  Count = c(", paste(top_topics$Count, collapse = ", "), ")\n", append = TRUE, file = report_file)
cat(")\n\n", append = TRUE, file = report_file)
cat("ggplot(top_topics, aes(x = reorder(Topic, Count), y = Count)) +\n", append = TRUE, file = report_file)
cat("  geom_bar(stat = 'identity', fill = '#4472C4') +\n", append = TRUE, file = report_file)
cat("  coord_flip() +\n", append = TRUE, file = report_file)
cat("  labs(title = 'Top 10 Mentioned Topics', x = '', y = 'Number of Mentions') +\n", append = TRUE, file = report_file)
cat("  theme_minimal() +\n", append = TRUE, file = report_file)
cat("  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'))\n", append = TRUE, file = report_file)
cat("```\n\n", append = TRUE, file = report_file)

# 12.3 Tableau des forces principales
cat("### Main Strengths\n\n```{r strengths-table, echo=FALSE, message=FALSE, warning=FALSE}\n", append = TRUE, file = report_file)
cat("library(knitr)\n\n", append = TRUE, file = report_file)
cat("# Création du dataframe de forces\n", append = TRUE, file = report_file)
cat("top_strengths_df <- data.frame(\n", append = TRUE, file = report_file)
cat("  Strength = c(", paste(paste0("\"", names(top_strengths), "\""), collapse = ", "), "),\n", append = TRUE, file = report_file)
cat("  Mentions = c(", paste(top_strengths, collapse = ", "), ")\n", append = TRUE, file = report_file)
cat(")\n\n", append = TRUE, file = report_file)
cat("kable(top_strengths_df, caption = 'Top 10 Mentioned Strengths')\n", append = TRUE, file = report_file)
cat("```\n\n", append = TRUE, file = report_file)

# 12.4 Tableau des faiblesses principales
cat("### Main Weaknesses\n\n```{r weaknesses-table, echo=FALSE, message=FALSE, warning=FALSE}\n", append = TRUE, file = report_file)
cat("library(knitr)\n\n", append = TRUE, file = report_file)
cat("# Création du dataframe de faiblesses\n", append = TRUE, file = report_file)
cat("top_weaknesses_df <- data.frame(\n", append = TRUE, file = report_file)
cat("  Weakness = c(", paste(paste0("\"", names(top_weaknesses), "\""), collapse = ", "), "),\n", append = TRUE, file = report_file)
cat("  Mentions = c(", paste(top_weaknesses, collapse = ", "), ")\n", append = TRUE, file = report_file)
cat(")\n\n", append = TRUE, file = report_file)
cat("kable(top_weaknesses_df, caption = 'Top 10 Mentioned Weaknesses')\n", append = TRUE, file = report_file)
cat("```\n\n", append = TRUE, file = report_file)

# -----------------------------------------------------------------------------
# 13. GÉNÉRATION DES ANALYSES TEXTUELLES AVEC LE LLM
# -----------------------------------------------------------------------------

# 13.1 Analyse générale et sentiment
general_prompt <- paste0(
  "Based on the analysis of ", total_reviews, " customer reviews for restaurant 'La Ligne Rouge', please provide an overview of the sentiment analysis with the following information:\n\n",
  
  "SENTIMENT OVERVIEW:\n",
  "- Overall sentiment score: ", round(sentiment_mean, 2), " (on a scale from -1 to 1)\n",
  "- Sentiment distribution: Very Negative: ", sentiment_distribution[1], ", Negative: ", sentiment_distribution[2], 
  ", Neutral: ", sentiment_distribution[3], ", Positive: ", sentiment_distribution[4], "\n\n",
  
  "Write a concise executive summary (200-300 words) that interprets the sentiment data and what it means for the restaurant. Include a brief introduction to the report."
)

cat("Generating sentiment analysis...\n")
general_response <- safe_api_call(general_prompt)

# 13.2 Analyse des sujets mentionnés
topics_prompt <- paste0(
  "Based on customer reviews for restaurant 'La Ligne Rouge', analyze the following top mentioned topics:\n\n",
  
  "TOP MENTIONED TOPICS (with frequency):\n",
  paste0(paste0("- ", top_topics$Topic, ": ", top_topics$Count), collapse = "\n"), "\n\n",
  
  "Please provide an analysis (200-300 words) of the most important topics mentioned by customers, including:\n",
  "1. What areas of the restaurant experience are customers focusing on the most?\n",
  "2. Are there any surprising or unexpected topics that appear frequently?\n",
  "3. What does the distribution of topics suggest about customer priorities?"
)

cat("Generating topic analysis...\n")
topics_response <- safe_api_call(topics_prompt)

# 13.3 Analyse des forces et faiblesses
strengths_weaknesses_prompt <- paste0(
  "Based on customer reviews for restaurant 'La Ligne Rouge', analyze the following strengths and weaknesses:\n\n",
  
  "TOP STRENGTHS (most frequently mentioned):\n",
  paste0(paste0("- ", names(top_strengths), ": ", top_strengths), collapse = "\n"), "\n\n",
  
  "TOP WEAKNESSES (most frequently mentioned):\n",
  paste0(paste0("- ", names(top_weaknesses), ": ", top_weaknesses), collapse = "\n"), "\n\n",
  
  "Please provide an analysis (300-400 words) of the restaurant's strengths and weaknesses, including:\n",
  "1. What are the restaurant's key competitive advantages based on customer feedback?\n",
  "2. What are the most critical areas for improvement?\n",
  "3. Are there any patterns or relationships between the strengths and weaknesses?\n",
  "4. How could the restaurant leverage its strengths to address its weaknesses?"
)

cat("Generating strengths and weaknesses analysis...\n")
strengths_weaknesses_response <- safe_api_call(strengths_weaknesses_prompt)

# 13.4 Recommandations et plan d'action
recommendations_prompt <- paste0(
  "Based on customer reviews for restaurant 'La Ligne Rouge', analyze the following customer recommendations:\n\n",
  
  "TOP RECOMMENDATIONS (most frequently mentioned):\n",
  paste0(paste0("- ", names(top_recommendations), ": ", top_recommendations), collapse = "\n"), "\n\n",
  
  "Please provide:\n",
  "1. A concise analysis of the most important customer recommendations (150-200 words)\n",
  "2. A practical 'Priority Action Plan' with 3-5 specific, actionable improvements the restaurant should make (250-300 words)\n",
  "3. A brief 'Strategic Outlook' section that suggests long-term directions for the restaurant (150-200 words)"
)

cat("Generating recommendations...\n")
recommendations_response <- safe_api_call(recommendations_prompt)

# -----------------------------------------------------------------------------
# 14. AJOUT DES ANALYSES AU RAPPORT
# -----------------------------------------------------------------------------
# Intégration des analyses générées dans le document Quarto
cat("## Customer Sentiment Analysis\n\n", append = TRUE, file = report_file)
cat(general_response, "\n\n", append = TRUE, file = report_file)

cat("## Analysis of Mentioned Topics\n\n", append = TRUE, file = report_file)
cat(topics_response, "\n\n", append = TRUE, file = report_file)

cat("## Analysis of Strengths and Weaknesses\n\n", append = TRUE, file = report_file)
cat(strengths_weaknesses_response, "\n\n", append = TRUE, file = report_file)

cat("## Recommendations and Action Plan\n\n", append = TRUE, file = report_file)
cat(recommendations_response, "\n\n", append = TRUE, file = report_file)

# -----------------------------------------------------------------------------
# 15. GÉNÉRATION DU RAPPORT FINAL
# -----------------------------------------------------------------------------
# Compilation du document Quarto en PDF
cat("Compiling Quarto document to PDF...\n")
tryCatch({
  # Tentative de génération du PDF
  quarto_render(report_file)
  cat("Report generated successfully!\n")
  cat("Quarto file: restaurant_report.qmd\n")
  cat("PDF file: restaurant_report.pdf\n")
}, error = function(e) {
  # En cas d'échec, tentative de génération en HTML
  cat("Error generating PDF: ", e$message, "\n")
  cat("Trying to generate HTML format instead...\n")
  
  tryCatch({
    # Modification du format dans le fichier Quarto
    qmd_content <- readLines(report_file)
    qmd_content <- gsub("format: pdf", "format: html", qmd_content)
    writeLines(qmd_content, report_file)
    
    # Génération du rapport en HTML
    quarto_render(report_file)
    cat("Report successfully generated in HTML format!\n")
    cat("Quarto file: restaurant_report.qmd\n")
    cat("HTML file: restaurant_report.html\n")
  }, error = function(e2) {
    cat("Failed to generate report in HTML format as well: ", e2$message, "\n")
  })
})
