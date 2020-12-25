#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

suppressWarnings(library(shiny))
suppressWarnings(library(tidyverse))
suppressWarnings(library(stringr))
suppressWarnings(library(dplyr))
suppressWarnings(library(ngram))
suppressWarnings(library(tidyr))

bi_words <- readRDS("bi_words_fast.rds")
tri_words  <- readRDS("tri_words_fast.rds")
quad_words <- readRDS("quad_words_fast.rds")
quint_words <- readRDS("quint_words_fast.rds")
sext_words <- readRDS("sext_words_fast.rds")

bigram <- function(input_words){
  num <- length(input_words)
  dplyr::filter(bi_words, 
                word1==input_words[num]) %>% 
    add_count(word2, sort=TRUE) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 2)) %>%
    as.character() -> out
  ifelse(out =="character(0)", "?", return(out))
}

trigram <- function(input_words){
  num <- length(input_words)
  dplyr::filter(tri_words, 
                word1==input_words[num-1], 
                word2==input_words[num])  %>% 
    add_count(word3, sort=TRUE) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 3)) %>%
    as.character() -> out
  ifelse(out=="character(0)", bigram(input_words), return(out))
}

quadgram <- function(input_words){
  num <- length(input_words)
  dplyr::filter(quad_words, 
                word1==input_words[num-2], 
                word2==input_words[num-1], 
                word3==input_words[num])  %>% 
    add_count(word4, sort=TRUE) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 4)) %>%
    as.character() -> out
  ifelse(out=="character(0)", trigram(input_words), return(out))
}

quintgram <- function(input_words){
  num <- length(input_words)
  dplyr::filter(quint_words, 
         word1==input_words[num-3], 
         word2==input_words[num-2], 
         word3==input_words[num-1],
         word4==input_words[num])  %>% 
    add_count(word5, sort=TRUE) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 5)) %>%
    as.character() -> out
  ifelse(out=="character(0)", quadgram(input_words), return(out))
}

sextgram <- function(input_words){
  num <- length(input_words)
  dplyr::filter(sext_words, 
         word1==input_words[num-4], 
         word2==input_words[num-3], 
         word3==input_words[num-2],
         word4==input_words[num-1],
         word5==input_words[num])  %>% 
    add_count(word6, sort=TRUE) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 6)) %>%
    as.character() -> out
  ifelse(out=="character(0)", quintgram(input_words), return(out))
}

ngrams <- function(input){
  # Create a dataframe
  input <- data_frame(text = input)
  # Clean the Input
  replace_reg <- "[^[:alpha:][:space:]]*"
  input <- input %>%
    mutate(text = str_replace_all(text, replace_reg, ""))
  # Find word count, separate words, lower case
  input_count <- str_count(input, boundary("word"))
  input_words <- unlist(str_split(input, boundary("word")))
  input_words <- tolower(input_words)
  # Call the matching functions
  out <- ifelse(input_count == 0, "Please enter your word or phrase in the given left text box.",
                ifelse(input_count == 1, bigram(input_words), 
                       ifelse (input_count == 2, trigram(input_words),
                               ifelse (input_count == 3, quadgram(input_words),
                                       ifelse (input_count == 4, quintgram(input_words),
                                               ifelse (input_count == 5,sextgram(input_words),sextgram(input_words) ))))))
  if(out == "?"){
    out = "The application not found the next expected word due to limited size of the training data" 
  }
  return(out)
}

shinyServer(function(input, output) {
  output$ngram_output <- renderText({
    ngrams(input$user_input)
  })
})


  
