# Title     : TODO
# Objective : TODO
# Created by: RTA
# Created on: 16/01/2021

library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)

# load data
original_data <- read.csv(file = "data-raw/data_twitter_kaggle.csv")

# get data
original_data <- original_data %>%
  filter(Sentiment == "Extremely Positive") %>%
  arrange(UserName) %>%
  head(75) %>%
  rbind(original_data %>%
          filter(Sentiment == "Extremely Negative") %>%
          arrange(UserName) %>%
          head(75) %>%
          rbind(original_data %>%
                  filter(Sentiment == "Extremely Positive") %>%
                  arrange(desc(UserName)) %>%
                  head(25) %>%
                  rbind(original_data %>%
                          filter(Sentiment == "Extremely Negative") %>%
                          arrange(desc(UserName)) %>%
                          head(25))))
csv <- original_data[1:150,]
# get spesific column
data <- data.frame(text = original_data$OriginalTweet, sentiment = original_data$Sentiment) %>%
  mutate(id = row_number(), .before = text)
data$sentiment <- ifelse(data$sentiment == "Extremely Positive", "Positive", "Negative")

# split data training & data testing
data_training <- data[1:150,]
data_training %>% select(-id) %>% write.csv("data-raw/data_training.csv", row.names = FALSE)
data_testing <- data[151:nrow(data),]

# cleaning data
temp_data_cleaning <- data

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

clean_data_testing <- data_testing %>%
  left_join(temp_data_cleaning, by = "id") %>%
  select(id, text.y)
colnames(clean_data_testing)[2] <- "text"

# predict all data testing
compare_accuracy <- data.frame(k = numeric(),
                               accuracy = numeric(),
                               precision = numeric(),
                               recall = numeric(),
                               f_measure = numeric())

for (j in c(3, 5, 7, 9, 11, 15, 21, 31, 51)) {
  result_predict <- data.frame(id = integer(),
                               predict = character())
  for (i in seq_len(nrow(clean_data_testing))) {
    cat(sprintf("\nProses k-%d: (%d / %d)", j, i, nrow(clean_data_testing)))
    # Executing Process
    data_predict <- clean_data_testing[i,]
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
    k <- j

    cek <- cosine %>%
      left_join(data_training, by = "id") %>%
      select(id, cosine, sentiment) %>%
      arrange(desc(cosine)) %>%
      head(k)

    sentiment_predict <- cek %>%
      count(sentiment)
    sentiment_predict <- sentiment_predict$sentiment[which.max(sentiment_predict$n)]

    result_predict <- result_predict %>%
      rbind(data.frame(id = data_predict$id,
                       predict = sentiment_predict))
  }
  result_predict <- data_testing %>%
    left_join(result_predict, by = "id")

  #testing accuracy
  compare_predict <- result_predict %>%
    mutate(interpretation = ifelse(sentiment == predict, 1, 0),
           TP = ifelse(sentiment == "Positive" & predict == "Positive", 1, 0),
           FP = ifelse(sentiment == "Negative" & predict == "Positive", 1, 0),
           TN = ifelse(sentiment == "Negative" & predict == "Negative", 1, 0),
           FN = ifelse(sentiment == "Positive" & predict == "Negative", 1, 0))

  accuracy <- (compare_predict %>%
    filter(interpretation == 1) %>%
    count)$n /
    (compare_predict %>% count)$n

  precision <- sum(compare_predict$TP) /
    (sum(compare_predict$TP) + sum(compare_predict$FP))

  recall <- sum(compare_predict$TP) /
    (sum(compare_predict$TP) + sum(compare_predict$FN))

  f_measure <- 2 * ((precision * recall) / (precision + recall))

  compare_accuracy <- compare_accuracy %>%
    rbind(data.frame(k = j,
                     accuracy = accuracy,
                     precision = precision,
                     recall = recall,
                     f_measure = f_measure))
}

#save.image("AppTesting.RData")
#load('AppTesting.RData')