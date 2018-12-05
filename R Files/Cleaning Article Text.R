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

# Delete first unnecessary column
article_texts$X1 <- NULL

# Make year its own column
article_texts <- article_texts %>% 
  mutate(year = str_extract_all(date, "\\d{4}")) %>% 
  select(name, year, date, text)

articles_word <- article_texts %>% 
  unnest_tokens(word, text)

articles_sent <- article_texts %>% 
  unnest_tokens(sentence, text, token = "sentences")

# Take out stop words
tidy_words <- articles_word %>% 
  anti_join(stop_words)

# Remove other stop words the filter did not get
tidy_words <- tidy_words %>% 
  filter(!word == "e.g", !word == 0, !word == 1, !word == 2, !word == 3, !word == 4, !word == 5)

# Word Cloud
tidy_words %>% 
  count(word, sort = T) %>% 
  with(wordcloud(word, n, max.words = 100, scale = c(3, 0.5)), random.order = T)

# Pos/Neg Word Cloud
tidy_words %>% 
  inner_join(get_sentiments("bing"), by = c(word = "word")) %>% 
  count(word, sentiment, sort = T) %>% 
  acast(word ~ sentiment, value.var = "n", fill = 50) %>% 
  comparison.cloud(colors = c("blue", "red"), max.words = 100, scale = c(0.8, 0.1))

article_bing <- tidy_words %>% 
  inner_join(get_sentiments("bing"), by = c(word = "word"))

article_nrc <- tidy_words %>%   
  inner_join(get_sentiments("nrc"), by = c(word = "word"))

article_afinn <- tidy_words %>% 
  inner_join(get_sentiments("afinn"), by = c(word = "word"))

test <- article_bing %>% 
  mutate(linenumber = row_number()) %>% 
  count(word, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(test, aes(index, sentiment)) +
  geom_point(show.legend = F) +
  geom_text(aes(label = word))


# Shiny test
test <- function(input, output, session){
  term <- reactive({
    input$update
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
      })
    })
  })
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale = c(4, 0.5),
                  min.freq = input$freq, max.words = input$max,
                  colors = brewer.pal(8, "Dark2"))
  })
}


clean_speeches$context2 <- c("name")
speech_words <- clean_speeches%>% 
  unnest_tokens(word, text) %>% 
  count(country, year, context, word, sort = T) %>% 
  ungroup() %>% 
  inner_join(get_sentiments("afinn"), by = c(word = "word"))


total_speech_words <- speech_words %>% 
  group_by(country, year, context) %>% 
  summarize(total = sum(n))

country_words <- left_join(speech_words, total_speech_words)

clean_speeches %>% 
  spread(context, key = context1, value = context2)


speech_tf <- country_words  %>% 
  bind_tf_idf(word, country, n) %>% 
  spread(key = context, value = tf)

speech_tf$SOTU[is.na(speech_tf$SOTU)] <- 0
speech_tf$UNGD[is.na(speech_tf$UNGD)] <- 0
  

test <- speech_tf %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(country, year, context) %>% 
  top_n(-3)
  #ungroup() %>% 
  ggplot(test, aes(word, tf_idf, fill = word)) +
  geom_col(show.legend = F) +
  labs(x = NULL, y = "tf-idf") +
  #scale_y_discrete(break = word, by = 5)+
  facet_wrap(~country, ncol = 2, scales = "free")
  #coord_flip()
  
  
test <- speech_tf %>% 
  select(country, year, context, word, n) %>% 
  group_by(country, year, context, word) %>% 
  top_n(2)
  
  #Clustering
  t1 <- as.matrix(test)
  distMatrix <- dist(t1, method = "euclidean")

  groups <- hclust(distMatrix, method = "ward.D")
plot(groups, cex = 0.9, hang = -1)  
rect.hclust(groups, k = 10)


p1 <- speech_tf %>% 
  filter(country == "CHN") %>% 
  ggplot(., aes(y = SOTU, x = UNGD, color = country)) +
  #geom_text(aes(label = word), size = 3, hjust = -0.15) +
  geom_point(alpha = 0.5, show.legend = F) +
  geom_jitter() +
  facet_grid(context ~ country) +
  labs(title = 'Year: {frame_time}', x = 'count', y = 'total frequency') +
  transition_time(year) +
  ease_aes('linear')

animate(p1)

p2 <- speech_tf %>% 
  filter(country == "RUS", context == "UNGD") %>% 
  ggplot(., aes(y = tf, x = n, color = country)) 
  geom_text(aes(label = word), size = 3, hjust = -0.15) +
  geom_point(alpha = 0.5, show.legend = F) +
  geom_jitter() +
  facet_grid(context ~ country) +
  labs(title = 'Year: {frame_time}', x = 'count', y = 'total frequency') +
  transition_time(year) +
  ease_aes('linear')

animate(p2)

p3 <- speech_tf %>% 
  filter(country == "USA") %>% 
  ggplot(., aes(y = tf, x = n, color = country)) +
  geom_text(aes(label = word), size = 3, hjust = -0.15) +
  geom_point(alpha = 0.5, show.legend = F) +
  geom_jitter() +
  facet_grid(context ~ country) +
  labs(title = 'Year: {frame_time}', x = 'count', y = 'total frequency') +
  transition_time(year) +
  ease_aes('linear')

animate(p3)

p4 <- speech_tf %>% 
  filter(country == "ZAF") %>% 
  ggplot(., aes(y = tf, x = n, color = country)) +
  geom_text(aes(label = word), size = 3, hjust = -0.15) +
  geom_point(alpha = 0.5, show.legend = F) +
  geom_jitter() +
  facet_grid(context ~ country) +
  labs(title = 'Year: {frame_time}', x = 'count', y = 'total frequency') +
  transition_time(year) +
  ease_aes('linear')

animate(p4)

p5 <- speech_tf %>% 
  filter(country == "PHL") %>% 
  ggplot(., aes(y = tf, x = n, color = country)) +
  geom_text(aes(label = word), size = 3, hjust = -0.15) +
  geom_point(alpha = 0.5, show.legend = F) +
  geom_jitter() +
  facet_grid(context ~ country) +
  labs(title = 'Year: {frame_time}', x = 'count', y = 'total frequency') +
  transition_time(year) +
  ease_aes('linear')

animate(p5)

p6 <- speech_tf %>% 
  filter(country == "GHA") %>% 
  ggplot(., aes(y = tf, x = n, color = country)) +
  geom_text(aes(label = word), size = 3, hjust = -0.15) +
  geom_point(alpha = 0.5, show.legend = F) +
  geom_jitter() +
  facet_grid(context ~ country) +
  labs(title = 'Year: {frame_time}', x = 'count', y = 'total frequency') +
  transition_time(year) +
  ease_aes('linear')

animate(p6)