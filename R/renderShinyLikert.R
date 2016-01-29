#' Shiny likert
#'
#' Creates a list of rendered shiny outputs to be used inside a call to
#' shinyApp() or inside an ShinyMarkdown document
#'
#' @usage
#' renderShinyLikert( data,
#'                    dropdown_factors = NULL,
#'                    questions = names( data$likert_data ),
#'                    height = NULL,
#'                    env = parent.frame(),
#'                    quoted = FALSE,
#'                    expr = "NULL",
#'                    response_levels = levels( data$likert_data[,1] ),
#'                    split_factors = NULL,
#'                    group,
#'                    grouping,
#'                    id,
#'                    ...
#'                  )
#'
#' @param id A string definig the id of the output. The id has to be
#' unique within your project
#' @param data An object of type likertData to be plotted
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
#' @param group    variables to group the plot
#' @param grouping chooses wether HH or likert will be used to display
#'                 the result
#' @param ... further arguments to be passed down to HH::likert
#'
#' @return  A list of rendered shiny objects which can be used as outputs.
#' @examples
#' \dontrun{
#'   shinyApp(
#'       ui= fluidPage(
#'           uiOutput( "selector" ),
#'           uiOutput( "plot" )
#'       ),
#'       server = function( input, output, session ){
#'           testData2 = createTestData( 100, 10, input, output )
#'           rendered = renderShinyLikert(
#'               testData2 ,
#'               dropdown = "gender" )
#'           output$plot     = rendered$plot
#'           output$selector = rendered$selector
#'       }
#'   )
#' }
#' @export
#' @author Gregor de Cillia
renderShinyLikert = function( data,
                              dropdown_factors = NULL,
                              questions = names( data$likert_data ),
                              height = NULL,
                              env = parent.frame(),
                              quoted = FALSE,
                              expr = "NULL",
                              response_levels = levels( data$likert_data[,1] ),
                              split_factors = NULL,
                              group = NULL,
                              grouping = "likert",
                              id = toString(paste0("id",
                                                   sample(1:10000, 1))),
                              ... ){
  # break scoping here. always be careful with those calls
  input  = get( 'input',   envir=env )
  output = get( 'output',  envir=env )

  # append input and output to dataset
  data$input  = input
  data$output = output

  # getter function for inputs related to this instance of renderShinyLikert
  getInput = function( str, default = NULL ){
    input_object = input[[ paste0( id, str ) ]]
    if( is.null( input_object ) )   # shiny inputs get initialized as NULL
      input_object = default        # since they are implemented like a list
    return( input_object )
  }

  # sanity checks for inputs
  valid_factors = union(
    names(data$row_factors),
    names(data$column_factors)
    )

  for ( factor in c( dropdown_factors, split_factors ) )
    if( !( factor %in% valid_factors ) )
      stop( paste( "factor", factor, "invalid" ) )

  # getter function for dropdown selections
  currentFactors = function(){
    out = NULL
    for ( factor in dropdown_factors ){
      if( is.null( getInput( factor ) ) )
        return( NULL )
      out = c( out, getInput( factor ) )
    }
    return( out )
  }

  # get all input UIs as list
  selector = createInputs( id,
                           dropdown_factors,
                           split_factors,
                           data$row_factors,
                           data$column_factors,
                           currentFactors,
                           getInput,
                           height,
                           group )

  # create filtered version of dataset
  filtered_data = reactive({
    filterDataSet( data,
                   dropdown_factors,
                   currentFactors()
    )
  })

  # create plot and table ouputs
  outs = renderShinyPlot( dropdown_factors,
                          currentFactors,
                          height,
                          id,
                          response_levels,
                          split_factors,
                          group,
                          grouping,
                          filtered_data,
                          ... )
  plot  = outs$plot
  table = outs$table

  output[[paste0(id,".plot")]] = plot

  # use an inputPanel container for the inputs
  selectorUI = renderUI({
    selectorList = selector()
    names( selectorList ) = NULL
    do.call( "inputPanel", selectorList )
  })

  # return
  out =
  list(
    selector = selectorUI,
    plot = renderUI({ tabsetPanel(
      tabPanel( "plot",
                plot
      ),
      tabPanel( "table",
                table
      )
    ) })
  )
  class( out ) = "shinyLikertOutput"
  return( out )

}
