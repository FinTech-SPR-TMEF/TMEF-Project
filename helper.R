library(quanteda)
library(ggrepel)
library(textclean)
library(tidyverse)
library(glmnet)
library(sentimentr)
library(stm)
library(doc2concrete)

# load functions
source("vectorFunctions.R") # a new one!
source("TMEF_dfm.R")
source("kendall_acc.R")

vecSmall <- readRDS("vecSmall.RDS")
load("wfFile.RData")

calculate_accuracy <- function(train_df, test_df, text_column, predict_column) {
  
  print("Function loaded")
  
  train_dfm <- TMEF_dfm(train_df[[text_column]], min.prop = 0, ngrams = 1)
  test_dfm <- TMEF_dfm(test_df[[text_column]], min.prop = 0, ngrams = 1) %>% 
    dfm_match(colnames(train_dfm))
  
  test_df_movie <- test_df %>% group_by(Movie) %>% 
                    summarise(actual = mean(!!sym(predict_column), na.rm = TRUE))
  
  # WORD COUNT Model
  test_df <- test_df %>%
    mutate(text_wdct = str_count(!!sym(text_column), "[[:alpha:]]+"))
  word_count_accuracy <- kendall_acc(test_df$text_wdct, test_df[[predict_column]])
  
  cat("\n") 
  print("Accuracy for WORD COUNT Model :: ")
  print(word_count_accuracy)
  
  
  # NGRAM Model
  lasso_model <- glmnet::cv.glmnet(x = train_dfm, y = train_df[[predict_column]])
  test_predict_ngram <- predict(lasso_model, newx = test_dfm)[,1]
  ngram_accuracy <- kendall_acc(test_predict_ngram, test_df[[predict_column]])
  
  cat("\n") 
  print("Accuracy for NGRAM Model :: ")
  print(ngram_accuracy)
  
  # NGRAM Model - Accuracy per movie
  test_predict_ngram_movie <- data.frame(Movie = test_df$Movie, Prediction = test_predict_ngram)
  test_predict_ngram_movie <- test_predict_ngram_movie %>% group_by(Movie) %>% 
    summarise(average_prediction = mean(Prediction, na.rm = TRUE))
  ngram_accuracy_movie <- kendall_acc(test_predict_ngram_movie$average_prediction, 
                                      test_df_movie$actual)
  cat("\n")  
  print("Accuracy for NGRAM Model (Movie) :: ")
  print(ngram_accuracy_movie)
   
  
  # SENTIMENT Model
  test_df <- test_df %>%
    mutate(sentiment = !!sym(text_column) %>%
             get_sentences() %>%
             sentiment_by() %>%
             pull(ave_sentiment))
  sentiment_accuracy <- kendall_acc(test_df$sentiment, test_df[[predict_column]])
  
  cat("\n") 
  print("Accuracy for SENTIMENT Model :: ")
  print(sentiment_accuracy)
  
  
  # WORD TO VECTOR Model
  vdat_train <- vecCheck(train_df[[text_column]], vecSmall, wfFile, PCAtrim=1)
  vdat_test <- vecCheck(test_df[[text_column]], vecSmall, wfFile, PCAtrim=1)
  lasso_vec <- glmnet::cv.glmnet(x=vdat_train, y=train_df[[predict_column]])
  
  test_predict_vec <- predict(lasso_vec, newx = vdat_test)[,1]
  vector_data_accuracy <- kendall_acc(test_predict_vec, test_df[[predict_column]])

  cat("\n") 
  print("Accuracy for WORD TO VECTOR Model :: ")
  print(vector_data_accuracy)
  
  # WORD TO VECTOR Model - Movie
  
  test_predict_vec_movie <- data.frame(Movie = test_df$Movie, Prediction = test_predict_vec)
  test_predict_vec_movie <- test_predict_vec_movie %>% group_by(Movie) %>% 
    summarise(average_prediction = mean(Prediction, na.rm = TRUE))
  vector_data_accuracy_movie <- kendall_acc(test_predict_vec_movie$average_prediction, 
                                      test_df_movie$actual)
  
  cat("\n")  
  print("Accuracy for WORD TO VECTOR Model (Movie) :: ")
  print(vector_data_accuracy_movie)
  
  
  # VECTOR and NGRAM Model
  combined_x_train = cbind(vdat_train, train_dfm)
  combined_x_test = cbind(vdat_test, test_dfm)
  lasso_vec_ngram <- glmnet::cv.glmnet(x=combined_x_train,
                                       y=train_df[[predict_column]])
  test_predict_vec_ngram <- predict(lasso_vec_ngram, newx = combined_x_test)[,1]
  vector_ngram_accuracy <- kendall_acc(test_predict_vec_ngram, test_df[[predict_column]])
 
  cat("\n") 
  print("Accuracy for WORD2VEC + NGRAM Model :: ")
  print(vector_ngram_accuracy)
  
  # VECTOR and NGRAM Model - Movie
  
  test_predict_vec_ngram_movie <- data.frame(Movie = test_df$Movie, Prediction = test_predict_vec_ngram)
  test_predict_vec_ngram_movie <- test_predict_vec_ngram_movie %>% group_by(Movie) %>% 
    summarise(average_prediction = mean(Prediction, na.rm = TRUE))
  vector_ngram_accuracy_movie <- kendall_acc(test_predict_vec_ngram_movie$average_prediction, 
                                            test_df_movie$actual)
  
  cat("\n")  
  print("Accuracy for WORD2VEC + NGRAM Model :: ")
  print(vector_ngram_accuracy_movie)
  
   
  # DICTIONARY Model
  positive_dict <- textdata::lexicon_loughran() %>%
    filter(sentiment=="positive") %>%
    pull(word) %>% paste(collapse=" ")

  lsims <- vecSimCalc(x=test_df[[text_column]], y=positive_dict, 
                      vecfile=vecSmall, wffile = wfFile, PCAtrim=1)
  dictionary_accuracy <- kendall_acc(lsims, test_df[[predict_column]])

  cat("\n") 
  print("Accuracy for DICTIONARY Model :: ")
  print(dictionary_accuracy)
  
  
  # # TOPIC MODEL
  # topic_model <- stm(train_dfm, K=30)
  # topic_train <- topic_model$theta
  # topic_model_stm <- glmnet::cv.glmnet(x=topic_train, y=train_df[[predict_column]])
  # topic_test <- fitNewDocuments(topic_model,
  #                               test_dfm %>% convert(to="stm") %>% `$`(documents))
  # test_stm_predict <- predict(topic_model_stm, newx = topic_test$theta)
  # topic_model_accuracy <- kendall_acc(test_stm_predict, test_df[[predict_column]])
  # 
  # cat("\n") 
  # print("Accuracy for TOPIC Model :: ")
  # print(topic_model_accuracy)
  
  
  # Storing accuracies in a dataframe
  accuracy_df <- data.frame(bind_rows(
                    word_count_accuracy %>% mutate(field="wordcount"),
                    sentiment_accuracy %>% mutate(field="sentiment"),
                    ngram_accuracy %>% mutate(field="ngrams"),
                    ngram_accuracy_movie %>% mutate(field="ngrams_movie"),
                    vector_data_accuracy %>% mutate(field="word2vec"),
                    vector_data_accuracy_movie %>% mutate(field="word2vec_movie"),
                    vector_ngram_accuracy %>% mutate(field="word2vecNgram"),
                    vector_ngram_accuracy_movie %>% mutate(field="word2vecNgram_movie"),
                    dictionary_accuracy %>% mutate(field="dictionary")))
  return(accuracy_df)
}


