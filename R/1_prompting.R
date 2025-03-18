library(dplyr)

# select the first 100 rows for demonstration purposes <- REMOVE THE SLICE IF YOU WANT TO ANALYSE THE FULL DATASET
df <- readRDS("data/ligne_rouge_cleaned.rds") %>% 
  slice(1:100)

df$language <- NA
df$sentiment <- NA
df$topics <- NA
df$recommendations <- NA
df$strengths <- NA
df$weaknesses <- NA

system_prompt <- "Your role is to analyze the sentiment of restaurant reviews and classify them according to specific categories"

groq <- ellmer::chat_deepseek(
  system_prompt = system_prompt,
  model = "deepseek-chat"
)

i <- 1
print(df$review_text[i])

for (i in 1:nrow(df)) {
  
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
    
    "Review: ", df$review_text[i]
  )

  response <- groq$chat(prompt)
  response_parsed <- jsonlite::fromJSON(response)
  
  df$language[i] <- response_parsed$language
  df$sentiment[i] <- response_parsed$sentiment
  df$topics[i] <- paste(response_parsed$topics, collapse = ", ")
  df$recommendations[i] <- paste(response_parsed$recommendations, collapse = ", ")
  df$strengths[i] <- paste(response_parsed$strengths, collapse = ", ")
  df$weaknesses[i] <- paste(response_parsed$weaknesses, collapse = ", ")

  Sys.sleep(5)
}
