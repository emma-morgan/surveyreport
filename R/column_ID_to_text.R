colnames_to_item_text <- function(text_responses, question, original_first_rows) {
  
  #This assumes we have the responses that have their original rows
  
  question_id <- question[['Payload']][['QuestionID']]
  
  item_index <- names(question[['Payload']][['ChoiceDataExportTags']])
  item_choice <- sapply(question[['Payload']][['ChoiceDataExportTags']], function(x) x[[1]])
  item_text <- sapply(item_index, function(x) question[['Payload']][['Choices']] [[x]][['Display']])
  
  choice_lookup <- tibble(item_index = item_index, item_choice = item_choice, item_text = item_text)
  
  new_data <- text_responses
  names(new_data) <- lapply(names(text_responses), 
                            function(x) original_first_rows[2, x])

  new_data <- new_data %>%
    dplyr::mutate(ROW_ID = 1:nrow(new_data)) %>%
    tidyr::gather(key, value, -ROW_ID) %>%
    dplyr::mutate(key = gsub(paste0(question_id,"-"),"",key)) %>%
    dplyr::left_join(choice_lookup, by = c("key" = "item_index")) %>%
    dplyr::select(-key,-item_choice) %>%
    tidyr::spread(key=item_text,value=value, drop=FALSE) %>%
    dplyr::select(-ROW_ID)
    
  return(new_data)
  
}
  