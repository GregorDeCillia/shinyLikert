#' createTestData
#'
#' Create a test data set based on uniform distributions. For checking
#' statistical test using p values, this data set can confirm wether
#' the p-values are equally distributed if this data is used.
#'
#' @usage
#' createTestData( participants = 100,
#'                 questions    = 10,
#'                 participant_factors = list(
#'                   country  = c( "US", "AT", "CH" ),
#'                   gender   = c( "Male", "Female" ),
#'                   position = c( "Programmer",
#'                                 "Project leader",
#'                                 "Salesman" ) ),
#'                 question_factors = list(
#'                   difficulty = c( "easy", "hard" ),
#'                   skill      = c( "logic",
#'                                   "compassion",
#'                                   "leadership" ) ),
#'                 response_levels = c( "Strongly disagree",
#'                                      "Disagree",
#'                                      "Neutral",
#'                                      "Agree",
#'                                      "Strongly Agree" ))
#'
#' @param participants        either the number of participants or a vector of
#'                            participant names
#' @param questions           either the number of questions or a vector of
#'                            question names
#' @param participant_factors list containing all participant factors as names
#'                            and their possible levels as entries.
#' @param question_factors    list containing all question factors
#' @param response_levels     character vector with response levels
#'
#' @examples
#' td = createTestData()
#' td
#' summary( td )
#'
#' td = createTestData(
#'   c( "John", "Tom", "Anna", "Gustavo" ),
#'   c( "The new changes in my company improved my motivation to work",
#'      "Our IT system needs improvement" )
#' )
#'
#' # overwrite the randomly generated genders
#' td$row_factors$gender = factor( c( "Male", "Male", "Female", "Male" ) )
#' summary( td )
#' td$row_factors
#'
#' @export
createTestData = function( participants = 100,
                           questions = 10,
                           participant_factors = list(
                             country = c("US","AT","CH"),
                             gender = c("Male", "Female" ),
                             position = c("Programmer",
                                          "Project leader",
                                          "Salesman")  ),
                           question_factors = list(
                             difficulty = c("easy", "hard"),
                             skill = c("logic",
                                       "compassion",
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
             column_factors = column_factors )
  class( out ) = "likertData"

  return( out )
}
