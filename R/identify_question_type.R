#Function to identify the type of quesiton
#I would have thought this was already part of QualtricsTools...but I could have been wrong

library(QualtricsTools)

#Identify question type:

identify_question_type <- function(question) {
  
  if (is_text_entry(question)) {question_type <- "text_entry"}
  
  else if (is_mc_multiple_answer(question)) {question_type <- "mc_multiple_answer"}
  
  else if (is_mc_single_answer(question)) {question_type <- "mc_singel_answer"}
  
  else if (is_matrix_multiple_answer(question)) {question_type <- "matrix_multiple_answer"}
  
  else if (is_matrix_single_answer(question)) {question_type <- "matrix_single_answer"}
  
  else {question_type <- "unknown_question_type"}
  
  return(question_type)
}
