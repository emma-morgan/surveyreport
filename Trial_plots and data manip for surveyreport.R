#Attepting to do some visual work with Qualtrics surveys into R Markdown files...

devtools::install_github("emmamorgan-tufts/QualtricsTools")
library(QualtricsTools)
library(ggplot2)
library(dplyr)

QualtricsTools::get_setup()



lean_dict <- lean_responses(blocks,responses)
names(lean_dict) <- c("ResponseID","QuestionResponseColumn","RawResponse","CodedResponse")
qdict <- create_response_column_dictionary(blocks,original_first_rows[1,], flow_from_survey(survey))

q <- "Recommend" #Example for the diversity survey
response_subset <- dplyr::filter(lean_dict,startsWith(QuestionResponseColumn,q))


mytitle <- "This is going to be the title of my graph"
mylevels <- names(table(response_subset$CodedResponse))

#Basic stacked bar for a matrix
ggplot()+geom_bar(data=response_subset, aes(x=QuestionResponseColumn,fill=CodedResponse))

#Basic 100% tacked bar for a matrix
ggplot()+geom_bar(data=response_subset, 
                  aes(x=QuestionResponseColumn,fill=CodedResponse),
                  position="fill")


