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
    
  p <- word_txt2 %>% 
    mutate(proportion = n / sum(n)) %>% 
    ggplot(., aes(y = n, x = proportion, color = score)) + 
    geom_point(alpha = 0.5, show.legend = F) +
    labs(title = 'Year: {frame_time}', x = 'Time', y = 'Count')+
    transition_time(year_week) +
    ease_aes('linear')
  
  animate(p)
  
  #_____________________________________
  
  speech_words <- clean_speeches%>% 
    unnest_tokens(word, text) %>% 
    count(country, year, context, word, sort = T)
  
  
  total_speech_words <- speech_words %>% 
    group_by(country, year, context) %>% 
    summarize(total = sum(n))
  
  country_words <- left_join(speech_words, total_speech_words)
  
  
  speech_tf <- country_words  %>% 
    bind_tf_idf(word, country, n) %>% 
    spread(key = context, value = tf)
  
  test <-speech_tf %>% 
    gather(context, tf, SOTU:UNGD)
  test <- subset(test, !is.na(tf))
  test1 <- subset(test, context == "SOTU")
  test2 <- subset(test, context == "UNGD")
  
  test3 <- test1 %>% left_join(test2, by = c("country", "year", "word"))
  test3 <- subset(test3, !is.na(tf.y))
  
  #``````````````````````
  
  txt_words <- full_txt %>% 
    unnest_tokens(word, text) %>% 
    count(name, date, source, year, word, sort = T)
  
  tot_txt_words <- txt_words %>% 
    group_by(name, date, year, source) %>% 
    summarize(total = sum(n))
  
  source_words <- left_join(txt_words, tot_txt_words)
  
  txt_tf <- source_words %>% 
    bind_tf_idf(word, name, n) %>% 
    spread(key = source, value = tf)
  
  final_tf1 <- txt_tf %>% 
    gather(source, tf, APSA:PSQ) %>% 
    filter(!is.na(tf))
  
  final_tf2 <- subset(final_tf1, source == "APSA")
  final_tf3 <- subset(final_tf1, source == "PSQ")
  
  final_tf4 <- final_tf2 %>% left_join(final_tf3, by = c("date", "year", "word"))
  
  final_tf4 <- subset(final_tf4, !is.na(tf.y))
  
  final_tf5 <- final_tf4 %>% 
    left_join(economic_sentiment)
  
  final_tf5 <- subset(final_tf5, !is.na(sentiment))
  
  final_tf5$sentiment[is.na(final_tf5$sentiment)] <-  "none"


write_csv(final_tf5, "final_tf5.csv")

final_tf5 <- final_tf5 %>% 
mutate(week = week(ymd(date)),
       year = year(date)) %>%
  group_by(year_week = floor_date(date, "1 week"))

p1 <- final_tf5 %>% 
  top_n(10) %>% 
  ggplot(., aes(y = log(tf.y), x = log(tf.x), color = sentiment)) +
  geom_text(aes(label = word), size = 3, hjust = -0.15) +
  geom_point(alpha = 0.5, show.legend = F) +
  geom_abline(slope = 1, intercept = 0) +
  geom_jitter() +
  facet_grid(source.x ~ source.y) +
  labs(title = 'Year: {frame_time}', x = 'APSA Frequnecy', y = 'PSQ Frequnecy') +
  transition_time(year_week) +
  ease_aes('linear')

animate(p1, fps = 2)

word_txt <- full_txt %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(source, name, date, word, sort = T) %>% 
  inner_join(get_sentiments("afinn"), by = c(word = "word"))





word_txt <- word_txt %>% 
  left_join(economic_sentiment)

word_txt$sentiment[is.na(word_txt$sentiment)]<- "none"

word_txt2 <- word_txt %>%
  mutate(week = week(ymd(date)),
         year = year(date)) %>%
  group_by(year_week = floor_date(date, "1 week"))


word_txt2 %>% 
  group_by(source, year_week) %>% 
  summarize()

word_txt2 %>% 
  mutate(proportion = n / sum(n)) %>% 
  ggplot(., aes(x = proportion, y = score, color = abs(score - proportion))) +
  geom_point()


p2 <- word_txt2 %>%
  group_by(source, year_week) %>%
  summarize(score = sum(score))  %>% 
  ggplot(aes(source, score)) + 
  geom_boxplot() + 
  transition_states(
    gear,
    transition_length = 2,
    state_length = 1
  ) +
  transition_time(year) +
  enter_fade() +
  exit_shrink() +
  ease_aes('linear')+
animate(p2)

final_tf5 <- final_tf5 %>% 
  inner_join(get_sentiments("afinn"), by = "word")

peak <- final_tf5 %>% 
  nest(word) %>% 
  mutate(text = map(data, unlist),
         text = map_chr(text, paste, collapse = " "))

final_tf5 %>% 
  filter(sentiment == "economy") %>% 
ggplot(., aes(x = source)) + geom_histogram()

par(mfrow = c(1,2))


grid.arrange(s1, s2, ncol=2)
boxplot(final_tf5$tf.y, xlab = "PSQ")

