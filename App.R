library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)

# load data
original_data_gephi <- read.csv(file = "data-raw/data_twitter_gephi.csv")
original_data_gephi <- original_data_gephi %>%
  filter(twitter_type == "Tweet") %>%
  arrange(desc(Id)) %>%
  select(Id, Label) %>%
  sample_n(100)
data_training <- read.csv(file = "data-raw/data_training.csv")

# get spesific column
all_data <- data.frame(text = original_data_gephi$Label,
                       sentiment = NA) %>%
  rbind(data_training) %>%
  mutate(id = row_number(), .before = "text")

# split data training & data testing
data_predict_full <- all_data[1:100,]
data_training <- all_data[101:250,]

# cleaning data
temp_data_cleaning <- all_data

## remove retweet entities
temp_data_cleaning$text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", temp_data_cleaning$text)
## remove at people
temp_data_cleaning$text <- gsub("@\\w+", " ", temp_data_cleaning$text)
## remove hastag
temp_data_cleaning$text <- gsub("#\\w+", " ", temp_data_cleaning$text)
## remove html links
temp_data_cleaning$text <- gsub("https://t.co/\\w+", " ", temp_data_cleaning$text)
## remove emoticon
temp_data_cleaning$text <- gsub('[^\x01-\x7F]', "", temp_data_cleaning$text)
## remove dot
temp_data_cleaning$text <- gsub('[\\.\\,]', " ", temp_data_cleaning$text)
## remove puntuation
temp_data_cleaning$text <- gsub('[[:punct:]]', "", temp_data_cleaning$text)
## remove control character
temp_data_cleaning$text <- gsub('[[:cntrl:]]', " ", temp_data_cleaning$text)
## remove digit
temp_data_cleaning$text <- gsub('\\d+', "", temp_data_cleaning$text)
## remove unnecessary spaces
temp_data_cleaning$text <- gsub("[ \t]{2,}", " ", temp_data_cleaning$text)
temp_data_cleaning$text <- gsub("^\\s+|\\s+$", "", temp_data_cleaning$text)
## change to lower case
temp_data_cleaning$text <- tolower(temp_data_cleaning$text)
temp_data_cleaning[temp_data_cleaning == ""] <- NA
## remove stop words
temp_data_cleaning <- temp_data_cleaning %>%
  select(id, text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  group_by(id) %>%
  summarize(text = str_c(word, collapse = " ")) %>%
  ungroup()

# split clean data training & data testing
clean_data_training <- data_training %>%
  left_join(temp_data_cleaning, by = "id") %>%
  select(id, text.y)
colnames(clean_data_training)[2] <- "text"

clean_data_predict <- data_predict_full %>%
  left_join(temp_data_cleaning, by = "id") %>%
  select(id, text.y)
colnames(clean_data_predict)[2] <- "text"

# predict all data
result_predict <- data_predict_full

for (j in seq_len(nrow(clean_data_predict))) {
  cat(sprintf("\nProses: (%d / %d)", j, nrow(clean_data_predict)))
  # Executing Process
  data_predict <- clean_data_predict[j,]
  tidy_data <- clean_data_training %>%
    rbind(data_predict)

  tf_idf <- tidy_data %>%
    unnest_tokens(word, text) %>%
    count(id, word, sort = TRUE) %>%
    bind_tf_idf(word, id, n)

  # wdi*wdj
  bobot_predict <- tf_idf %>%
    filter(id == data_predict$id)

  bobot_training <- data.frame(id = integer(),
                               sum = numeric())

  for (i in seq_len(nrow(clean_data_training))) {
    temp_data <- tf_idf %>%
      filter(id == clean_data_training$id[i])

    join <- bobot_predict %>%
      inner_join(temp_data, by = "word") %>%
      mutate(kali = tf_idf.x * tf_idf.y)

    bobot_training <- bobot_training %>%
      rbind(data.frame(id = clean_data_training$id[i], sum = sum(join$kali)))
  }

  # panjang vektor
  kuadrat_bobot <- tf_idf
  kuadrat_bobot$tf_idf <- kuadrat_bobot$tf_idf^2

  vektor <- data.frame(id = integer(),
                       sum = numeric(),
                       sqrt = numeric())

  for (i in seq_len(nrow(tidy_data))) {
    temp_data <- kuadrat_bobot %>%
      filter(id == tidy_data$id[i])

    temp_sum <- sum(temp_data$tf_idf)
    temp_sqrt <- sqrt(temp_sum)

    vektor <- vektor %>%
      rbind(data.frame(id = tidy_data$id[i],
                       sum = temp_sum,
                       sqrt = temp_sqrt))
  }

  # cosine similarity
  vektor_predict <- vektor %>% filter(id == data_predict$id)

  cosine <- data.frame(id = integer(),
                       cosine = numeric())
  for (i in seq_len(nrow(clean_data_training))) {
    temp_id <- clean_data_training$id[i]
    temp_bobot <- bobot_training %>% filter(id == temp_id)
    temp_vektor <- vektor %>% filter(id == temp_id)

    temp_cosine <- temp_bobot$sum / (vektor_predict$sqrt * temp_vektor$sqrt)

    cosine <- cosine %>%
      rbind(data.frame(id = temp_id,
                       cosine = temp_cosine))
  }

  # knn
  k <- 5

  cek <- cosine %>%
    left_join(data_training, by = "id") %>%
    select(id, cosine, sentiment) %>%
    arrange(desc(cosine)) %>%
    head(k)

  sentiment_predict <- cek %>%
    count(sentiment)
  sentiment_predict <- sentiment_predict$sentiment[which.max(sentiment_predict$n)]

  result_predict$sentiment[j] <- sentiment_predict
}
write.csv(data_predict_full, "data-raw/data_predict.csv", row.names = FALSE)
write.csv(clean_data_predict, "data-raw/data_predict_clean.csv", row.names = FALSE)
write.csv(result_predict, "data-raw/data_predict_result.csv", row.names = FALSE)
cat(sprintf("\nSelesai"))

#save.image("App.RData")
#load('App.RData')