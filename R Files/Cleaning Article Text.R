library(tidyverse)
library(tidytext)
library(tm)
library(stringr)

articles_word <- articles %>% 
  unnest_tokens(word, text)

articles_sent <- articles %>% 
  unnest_tokens(sentence, text, token = "sentences")

tidy_words <- articles_word %>% 
  anti_join(stop_words)

tidy_words %>% 
  gsub("\\d", "", .)
count(word, sort = T)


test <- c("hello \"35\"", "\"524\"Another test" )
writeLines(test)
clean_articles <- article_texts %>% 
  transform(text = gsub("\"[0-9]+\"", "", .))
