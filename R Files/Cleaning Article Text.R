library(tidyverse)
library(tidytext)
library(tm)
library(stringr)



articles_word <- article_texts %>% 
  unnest_tokens(word, text)

articles_sent <- article_texts %>% 
  unnest_tokens(sentence, text, token = "sentences")

tidy_words <- articles_word %>% 
  anti_join(stop_words)

  
