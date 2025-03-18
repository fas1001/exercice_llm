library(dplyr)
library(readxl)

df <- read_xlsx("data/ligne_rouge_raw.xlsx") %>%
  filter(name == "La Ligne Rouge") %>% 
  select(
    -query, 
    -google_id, 
    -place_id, 
    -location_link, 
    -reviews_link, 
    -reviews, 
    -review_id, 
    -review_pagination_id, 
    -author_link, 
    -author_id, 
    -author_image, 
    -review_img_urls, 
    -review_img_url, 
    -review_questions, 
    -review_photo_ids, 
    -owner_answer_timestamp, 
    -owner_answer_timestamp_datetime_utc, 
    -review_link, 
    -review_timestamp, 
    -review_datetime_utc, 
    -review_likes, 
    -reviews_id, 
    -reviews_per_score_1, 
    -reviews_per_score_2, 
    -reviews_per_score_3, 
    -reviews_per_score_4, 
    -reviews_per_score_5,
    -starts_with("review_question")
  ) %>%
  filter(!is.na(review_text))

saveRDS(df, "data/ligne_rouge_cleaned.rds")
