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
  response_table <- as.tibble(lapply(response_table,unlist)) %>%
    mutate(value = as.numeric(var)) %>%
    dplyr::select(value, text) %>%
    arrange(value)
  if (remove_na) {
    response_table <- filter(response_table, !(value < 0))
  }

  # response_table <- as.tibble(create_response_lookup_table(question)) %>%
  #   select(var, text)# %>%
  #   as.tibble()# %>%
  #   dplyr::arrange(var)
  # if (remove_na) {
  #   response_table <- filter(response_table )
  # }
  
  
  
  factor_levels <- unlist(response_table[['text']])
  
  #Load text for the question
  
  title <- question[['Payload']][['QuestionTextClean']]

  q_responses <- question[['Responses']]
  
  text_responses <- dplyr::mutate(q_responses, ROW_ID = 1:nrow(q_responses)) %>%
    tidyr::gather(key, value, -ROW_ID) %>%
    dplyr::mutate(int_value = as.numeric(value)) %>%
    dplyr::left_join(response_table, by = c("int_value" = "value")) %>%
    select(-value,-int_value) %>%
    spread(key=key,value=text, drop=FALSE) %>%
    select(-ROW_ID)
  
  #Convert to factor
  
  factor_responses <- as.data.frame(lapply(text_responses, function(x){
    factor(x, levels =factor_levels)}))

  lr <- likert::likert(factor_responses)
  return(lr)
}

