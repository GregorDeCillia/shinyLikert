#' createTestData
#'
#' Create a test data set based on uniform distributions
#'
#' @param participants Either the number of participants or a vector of participant names
#' @param questions    Either the number of questions or a vector of question names
#' @param participant_factors A list containing all participant factors
#' @param question_factors    A list containing all question factors
#' @param response_levels     Vector with response levels
#'
#' @examples
#' createTestData( 100, 10 )
#'
#' td = createTestData( c( "John", "Tom", "Anna", "Gustavo" ),
#'                      c( "The new changes in my company improved my motivation to work",
#'                         "Our IT system needs improvement"
#'                          )  )
#' td$row_factors$gender = factor( c( "Male", "Male", "Female", "Male" ) )
#' summary( td )
#' td$row_factors
#'
#' @export
createTestData = function( participants,
                           questions,
                           input,
                           output,
                           participant_factors = list( country = c("US","AT","CH"),
                                                       gender = c("Male", "Female" ),
                                                       position = c("Programmer",
                                                                    "Project leader",
                                                                    "Salesman")  ),
                           question_factors = list( difficulty = c("easy", "hard"),
                                                    skill = c("logic", "compassion",
                                                              "leadership" ) ),
                           response_levels = c("Strongly disagree",
                                               "Disagree",
                                               "Neutral",
                                               "Agree",
                                               "Strongly Agree" )
){
  if( is.numeric(participants) )
    participants = paste( "Participant", 1:participants )
  if( is.numeric( questions ) )
    questions = paste( "Question", 1:questions )

  nparticipants = length( participants )
  nquestions    = length( questions )

  likert_data = data.frame( row.names = participants )
  for ( question in questions ){
    new = factor( sample( response_levels, nparticipants, TRUE ),
                  levels = response_levels,
                  ordered = TRUE )
    likert_data[[question]] = new
  }

  row_factors = data.frame( row.names = participants )
  i = 0
  for ( participant_factor in participant_factors ){
    i = i + 1
    new = sample( participant_factor, nparticipants, TRUE )
    new = factor( new, levels = participant_factor )
    row_factors[[ names(participant_factors)[i] ]] = new
  }

  column_factors = data.frame( row.names = questions )
  i = 0
  for ( question_factor in question_factors ){
    i = i+1
    new = sample( question_factor, nquestions, TRUE )
    new = factor( new, levels = question_factor )
    column_factors[[ names(question_factors)[i] ]] = new
  }

  out = list(likert_data = likert_data,
             row_factors = row_factors,
             column_factors = column_factors,
             input = input,
             output  = output )
  class( out ) = "likertData"

  return( out )
}
