#' Function to generate visual reports from QualtricsTools parsed data
#' using Jason Bryer's Likert package
#' 
#' 4/11/2018
#' Emma Morgan
#' 
#' By default, this function will remove n/a options from data and response options


library(QualtricsTools)
library(tidyverse)
library(likert)


generate_visual_responses <- function(question, remove_na = TRUE) {
  
  #Get our response table decoded
  
  response_table <- create_response_lookup_table(question)
  response_table <- as.tibble(lapply(response_table,unlist))
  response_table <- mutate(response_table, value = as.integer(var))
  response_table <- dplyr::select(response_table, value, text)
  response_table <- arrange(response_table, value)
  response_table <- filter(response_table, ! (value < 0))
  response_table <- response_table[order(response_table[['var']], decreasing = FALSE),]
  

  response_table <- as.tibble(create_response_lookup_table(question)) %>%
    select(var, text)# %>%
    as.tibble()# %>%
    dplyr::arrange(var)
  if (remove_na) {
    response_table <- filter(response_table )
  }
  
  
  
  factor_levels <- unlist(response_table[['text']])
  
  #Load text for the question
  
  title <- question[['Payload']][['QuestionText']]

  responses <- question[['Responses']]
  
  text_responses <- dplyr::mutate(responses, ROW_ID = 1:nrow(responses)) %>%
    tidyr::gather(key, value, -ROW_ID) %>%
    dplyr::left_join(response_table, by = c("value" = "var")) %>%
    select(-value) %>%
    spread(key=key,value=text, drop=FALSE) %>%
    select(-ROW_ID)
  
  #Convert to factor
  
  factor_responses <- as.data.frame(lapply(text_responses, function(x){
    factor(x, levels =factor_levels)}))

  lr <- likert::likert(factor_responses)
  return(lr)
}

