#' Shiny likert
#'
#' Creates a list of rendered shiny outputs to be used inside a call to
#' shinyApp() or inside an ShinyMarkdown document
#'
#' @usage
#' renderShinyLikert( id, data,
#'                    input, output,
#'                    dropdown_factors = NULL,
#'                    questions = names( data$likert_data ),
#'                    height = NULL,
#'                    env = parent.frame(),
#'                    quoted = FALSE,
#'                    expr= "NULL",
#'                    response_levels = levels( data$likert_data[,1] ),
#'                    split_factors = NULL,
#'                    ...
#'                  )
#'
#' @param id A string definig the id of the output. The id has to be
#' unique within your project
#' @param data An object of type likertData to be plotted
#' @param input,output options to be passed down from the following call \cr
#' shinyApp( ui=..., server = function( input,output, session )\{ ... \} )
#' @param dropdown_factors names ot the factors which should be used as for
#' the dropdown menus
#' @param questions the questions that should be displayed. (Currently broken)
#' @param height minimum and maximum height of the plot in px. Setting this option
#' will create a slider to control the height of the plot.
#' @param env argument to be used by the shiny runtime
#' @param quoted argument to be used by the shiny runtime
#' @param expr argument to be used by the shiny runtime
#' @param response_levels answer possibilities of interest
#' @param split_factors factors to be used in the HH plot
#' @param ... further arguments to be passed down to HH::likert
#' @return  A list of rendered shiny objects which can be used as outputs.
#' @examples
#' x <- 1
#' # shinyApp(
#' #     ui= fluidPage(
#' #         uiOutput( "selector" ),
#' #         plotOutput( "plot" )
#' #     ),
#' #     server = function( input, output, session ){
#' #         rendered = renderShinyLikert(
#' #             "ID 1", testData2 ,
#' #             input, output,
#' #             dropdown = "gender" )
#' #         output$plot     = rendered$plot
#' #         output$selector = rendered$selector
#' #     }
#' # )
#' @export
#' @author Gregor de Cillia
renderShinyLikert = function( id,
                              data,
                              input = data$input,
                              output = data$output,
                              dropdown_factors = NULL,
                              questions = names( data$likert_data ),
                              height = NULL,
                              env = parent.frame(),
                              quoted = FALSE,
                              expr= "NULL",
                              response_levels = levels( data$likert_data[,1] ),
                              split_factors = NULL,
                              group = NULL,
                              grouping = "likert",
                              ... ){
  valid_factors = c( names(data$row_factors), names(data$"column_factors") )

  for ( factor in c( dropdown_factors, split_factors ) )
    if( !( factor %in% valid_factors ) )
      stop( paste( "factor", factor, "invalid" ) )

  dataset        = data$likert_data#[ , questions ]
  row_factors    = data$row_factors
  column_factors = data$column_factors#[questions,]

  currentFactors = function(){
    out = NULL
    for ( factor in dropdown_factors ){
      if( is.null( input[[ paste0( id, factor ) ]] ) )
        return( NULL )
      out = c( out, input[[ paste0( id, factor ) ]] )
    }
    if ( is.null(out) )  ## bypass error messages by replacing NA with FALSE
      return ( NULL )
    return( out )
  }

  selector2 = reactive({
    out = create_dropdown_selector( id, dropdown_factors,
                              row_factors, column_factors,
                              currentFactors()#,
                              #input, output
    )
    out$heightSlider = renderHeightSlider( id, height )
    return( out )
  })

  # create ouputs
  outs = renderShinyPlot( data,
                          input,
                          dropdown_factors,
                          currentFactors,
                          questions,
                          height,
                          id,
                          response_levels,
                          split_factors,
                          group,
                          grouping,
                          output,
                          ... )

  plot = outs$plot
  cat( "TABLE SIZE\n"  )
  #cat( dim( outs$table ), "\n\n\n" )
  #output[[ paste0(id, ".factorTable" ) ]] = outs$table
  cat( paste0(id, ".factorTable" ) )
  table = outs$table

  output[[paste0(id,".plot")]] = plot

  selector3 = reactive({
    selector21 = selector2()
  if ( length( selector21 ) == 0 )
    return( NULL )
  if ( length( selector21 ) == 1 )
    return( selector3 = inputPanel(  selector21[[1]] ) )
  if( length( selector21 ) == 2 )
    return( inputPanel(  selector21[[1]], selector21[[2]] ) )
  if( length( selector21 ) == 3 )
    return( inputPanel(  selector21[[1]],
                             selector21[[2]],
                             selector21[[3]] )
    )
  if( length( selector21 ) == 4 )
    return( inputPanel(  selector21[[1]],
                             selector21[[2]],
                             selector21[[3]],
                             selector21[[4]] ) )
  })

  #selector4 = selector2
  #names( selector4 ) = NULL
  #selector3 = do.call( "inputPanel", c( selector2, cellArgs = list() ) )


  list(
        selector = renderUI( selector3() ),
        plot = renderUI({ tabsetPanel(
          tabPanel( "plot",
                    plot
          ),
          tabPanel( "table",
                    table
          )
        ) })
  )

}
