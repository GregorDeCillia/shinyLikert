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
  input  = get( 'input',   envir=env )
  output = get( 'output',  envir=env )
  data$input  = input
  data$output = output

  getInput = function( str, default = NULL ){
    input_object = input[[ paste0( id, str ) ]]
    if( is.null( input_object ) )   # shiny inputs get initialized as NULL
      input_object = default        # since they are implemented like a list
    input[[ paste0( id, str ) ]]
  }

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
      if( is.null( getInput( factor ) ) )
        return( NULL )
      out = c( out, getInput( factor ) )
    }
    if ( is.null(out) )  ## bypass error messages by replacing NA with FALSE
      return ( NULL )
    return( out )
  }

  selector = reactive({
    x = currentFactors()   # force reactivity
    out = create_dropdown_selector( id, dropdown_factors,
                              row_factors, column_factors,
                              currentFactors()
    )
    out$heightSlider = renderHeightSlider( id, height )
    if( ! is.null( split_factors )  ){
      selection = getInput( ".split_factors" )
      if( is.null(selection ) )
        selection = split_factors
      out$mulipanel = selectInput(
        inputId = paste0( id, ".split_factors" ),
        label   = "split factors",
        choices = setdiff(
          union(
            names( data$row_factors),
            names( data$column_factors )
          ),
          dropdown_factors
        ),
        selected = selection,
        multiple = TRUE )
    }
    return( out )
  })

  # create filtered version of dataset
  filtered_data = reactive({
    filterDataSet( data,
                   dropdown_factors,
                   currentFactors()
    )
  })

  # create ouputs
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

  plot = outs$plot
  table = outs$table

  output[[paste0(id,".plot")]] = plot

  # use an inputPanel container for the inputs
  selectorUI = renderUI({
    selectorList = selector()
    names( selectorList ) = NULL
    do.call( "inputPanel", selectorList )
  })

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

}
