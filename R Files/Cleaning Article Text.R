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

tidy_words %>% 
  gsub("\\d", "", .)
count(word, sort = T)


test <- c("hello \"35\"", "\"524\"Another test","hello \",\" \"hey\"")
writeLines(test)
test <- text %>% 
  mutate(text = gsub("\"[0-9]+\"", "", text))
  mutate(text = gsub("\",[a-z]\"", "", .))

str_replace_all(text, "\"[0-9]+\"", "")


article_texts %>% 
  str_detect(text, "\"[0-9]+\"")
article_texts$text[str_detect(article_texts$text, "\"[0-9]+\"")]

peak <- article_texts %>% 
  filter(str_detect(article_texts$text, "\"[0-9]+\",\"")) %>% 
  mutate(text = gsub())
  
  
another_peak <- article_texts %>% 
  filter(!str_detect(article_texts$text, "\"[0-9]+\",\""))

writeLines("\"[0-9]+\",\"")
