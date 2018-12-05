library(tidyverse)
library(tidytext)
library(tm)
library(stringr)
library(textmineR)
library(gganimate)
library(tweenr)
library(wordcloud)
library(reshape2)
library(memoise)
library(ggraph)
library(igraph)
library(lubridate)


art_words <- clean_articles%>% 
  separate(date, into = c("year", "month", "day"), sep = "-") %>% 
  unnest_tokens(word, text) %>% 
  count(year, word, sort = T) %>% 
  ungroup() %>% 
  inner_join(get_sentiments("afinn"), by = c(word = "word"))


tot_art <- art_words %>% 
  group_by(year,  word) %>% 
  summarize(total = sum(n))

country_words <- left_join(speech_words, total_speech_words)

speech_tf <- country_words  %>% 
  bind_tf_idf(word, country, n) %>% 
  spread(key = context, value = tf)


txt_bigram <- full_txt %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  nest(word) %>% 
  mutate(text = map(data, unlist),
         text = map_chr(text, paste, collapse = " ")) %>% 
  select(name, date, text) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

txt_bigram %>% 
  count(bigram, sort = T)

bi_sp <- txt_bigram %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

bi_filt <- bi_sp %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word1 == "figure") %>% 
  filter(!word1 == "table") %>% 
  filter(!word1 == "model") %>% 
  filter(!word1 == "0") %>% 
  filter(!word1 == "1")

bi_count <- bi_filt %>% 
  count(word1, word2, sort = T) 

bi_united <- bi_filt %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  count(name, bigram)

bigram_graph <- bi_count %>% 
  filter(n > 20) %>% 
  graph_from_data_frame()

set.seed(1997)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))


  word_txt <- full_txt %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    count(name, date, word, sort = T) %>% 
    inner_join(get_sentiments("afinn"), by = c(word = "word"))
  
  word_txt2 <- word_txt %>%
    mutate(week = week(ymd(date)),
           year = year(date)) %>%
    group_by(year_week = floor_date(date, "1 week"))
  
p <- ggplot(word_txt2, aes(y = n, x = year_week, color = score)) + 
  geom_point(alpha = 0.5, show.legend = F) +
  labs(title = 'Year: {frame_time}', x = 'Time', y = 'Count')+
  transition_time(year_week) +
  ease_aes('linear')

animate(p)

