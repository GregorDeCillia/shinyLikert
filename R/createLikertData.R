#' createLikertData
#'
#' Create data of class likertData from three dataframes. This function also
#' applies some sanity checks for the inputs.
#'
#' @param likert_scale_questions The data consisting of likert-scale items with the
#' response_levels supplied below. The columns should be either factorial or numeric,
#' containing only whole numbers (e.g. 0 - 5, 1 - 5, ... )
#' @param participant_factors Factors that can be used to classify the participants
#' @param question_factors    Factors that can be used to classify the questions
#' @param response_levels     The answer choices for the likert questions.
#'                            Often c( "strongly agree", "agree", ... )
#'
#' @export
#' @author Gregor de Cillia
#'
createLikertData = function( likert_scale_questions,
                             participant_factors,
                             question_factors,
                             response_levels ){
  nquestions = dim(likert_scale_questions)[2]
  likert_data = likert_scale_questions
  for ( i in 1:nquestions ){
    likert_data[ ,i ] = factor( likert_data[ , i ] )
    levels( likert_data[ , i ] ) = response_levels
  }

  row_factors = participant_factors
  for ( i in 1:length( row_factors ) )
    row_factors[ , i ] = factor( row_factors[ , i ] )

  out = list( likert_data = likert_data,
              row_factors = row_factors,
              column_factors = question_factors )

  class( out ) = "likertData"

  return( out )
}
