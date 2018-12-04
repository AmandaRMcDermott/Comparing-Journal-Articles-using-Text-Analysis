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
  
