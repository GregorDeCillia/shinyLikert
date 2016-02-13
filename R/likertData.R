#' print likert data
#'
#' prints some information about the data
#'
#' @param x   An object of class likertData
#' @param ... further arguments. Will be ignored
#' @examples
#' testData2 = createTestData()
#' print( testData2 )
#' @export
#' @author Gregor de Cillia
print.likertData = function( x, ... ){
  cat("likert_data: \n    data frame with",
      dim(x$likert_data)[1],
      "observations of",
      dim(x$likert_data)[2],
      "questions\n"
      )
  cat("row_factors: \n    data frame with",
      dim(x$row_factors)[2],
      "factors for the",
      dim(x$row_factors )[1],
      "participants\n"
  )
  cat("column_factors: \n    data frame with",
      dim( x$column_factors )[2],
      "factors for the",
      dim(x$column_factors )[1],
      "questions\n"
  )
}

#' Summarize likert data
#'
#' prints a summary about the data
#'
#' @param object An object of class likertData
#' @param ... further arguments for the HH::likert call
#' @examples
#' testData2 = createTestData()
#' summary( testData2 )
#' @export
#' @author Gregor de Cillia
summary.likertData = function( object, ... ){
  cat("likert_data: \n data frame with",
      dim(object$likert_data)[1],
      "observations of",
      dim(object$likert_data)[2],
      "questions\n" )
  print(
    summary(
      data.frame(
        responseLevel = unlist( object$likert_data )
        )
      )
    )
  cat( "\n" )

  cat( "participant_factors:\n" )
  print( summary(object$row_factors) )

    cat( "\nquestion_factors:\n" )
  print( summary(object$column_factors) )
}

#' Plot likert data
#'
#' Create a simple likert plot from the data
#'
#' @param x An object of class likertData
#' @param ... further arguments passed down to the HH::likert call
#' @examples
#' testData2 = createTestData()
#' plot( testData2 )
#' plot( testData2, positive.order = TRUE )
#' @export
#' @author Gregor de Cillia
plot.likertData = function( x,
                            ... ){
  HH::likert( likert::likert( x$likert_data ),
              main = deparse(substitute( x ) ),
              ... )
}

