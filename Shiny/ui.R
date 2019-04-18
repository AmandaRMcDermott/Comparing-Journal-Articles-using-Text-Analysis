library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(tidyverse)
library(tidytext)
library(shinythemes)
library(readr)

navbarPage(
  "Journal Articles Wordcloud",
  theme = shinytheme("superhero"),
  tabPanel(
    "Wordcloud",
    sidebarPanel(
      selectInput("selection", "Choose a Journal:",
                  choices = sources),
      actionButton("update", "Change"),
      hr(),
      
      offset = 1,
      sliderInput("yrs", "Years", 2013, 2018, value = c(2013, 2014)),
      sliderInput(
        "freq",
        "Min Freq:",
        min = 1,
        max = 50,
        value = 15),
      
      sliderInput(
        "max",
        "Max Number of Words:",
        min = 1,
        max = 300,
        value = 100)
    ),
    mainPanel(
      plotOutput('plot', width = "auto", height = "auto")
    )),
  tabPanel("Comparison Wordclouds",
           sidebarPanel(
             selectInput("selection3", "Choose Journal 1",
                         choices = sources),
             hr(),
             selectInput("selection4", "Choose Journal 2",
                         choices = sources2),
             hr(),
             offset = 1,
             sliderInput("yrs2", "Years", 2013, 2018, value = c(2013, 2014)),
             sliderInput(
               "freq2",
               "Min Freq:",
               min = 1,
               max = 50,
               value = 15
             ),
             sliderInput(
               "max2",
               "Max Number of Words:",
               min = 1,
               max = 300,
               value = 100
             )
           ),
           mainPanel(
             plotOutput('plot2', width = "auto", height = "600px"))
  )
)