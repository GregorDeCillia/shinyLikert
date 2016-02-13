#' Shiny likert
#'
#' Creates a list of rendered shiny outputs to be used in a shiny app or
#' a "shiny-markdown" document
#'
#' @usage
#' renderShinyLikert( data,
#'                    dropdown_factors = NULL,
#'                    questions = names( data$likert_data ),
#'                    height = NULL,
#'                    response_levels = levels( data$likert_data[ , 1 ] ),
#'                    split_factors = NULL,
#'                    group = NULL,
#'                    grouping = "likert",
#'                    id = toString( sample( 10^15, 1 ) ),
#'                    wrap = 30,
#'                    custom_tests = NULL,
#'                    ...
#'                  )
#'
#' @param data             an object of class likertData to be plotted
#' @param dropdown_factors a character vector containing names of the factors which
#'                         should be used for the dropdown menus
#' @param questions        the questions that should be displayed. (Currently broken)
#' @param height           minimum and maximum height of the plot in px. Setting this
#'                         option will create a slider to control the height of the
#'                         plot.
#' @param response_levels  answer possibilities to be plotted and tested
#' @param split_factors    factors to be used in the HH plot
#' @param group            variables to group the plot
#' @param grouping         decides wether the package HH or the package likert
#'                         will be used to display the result. Only relevant if
#'                         group has been set
#' @param id               a string definig the id of the output. The id has to be
#'                         unique within your project
#' @param wrap             apply wraping for text on the y-Axis (factor levels).
#' @param custom_tests     a list containing tests to be applied in the table
#'                         output
#' @param ...              further arguments to be passed down to HH::likert
#'
#' @return  A list of rendered shiny objects which can be used as outputs.
#' @examples
#' \dontrun{
#'   library( shiny )
#'   shinyApp(
#'       ui= fluidPage(
#'           uiOutput( "selector" ),
#'           uiOutput( "plot" )
#'       ),
#'       server = function( input, output, session ){
#'           testData2 = createTestData()
#'           rendered = renderShinyLikert(
#'               testData2 ,
#'               dropdown = "gender" )
#'           output$plot     = rendered$plot
#'           output$selector = rendered$selector
#'       }
#'   ) }
#' @export
#' @author Gregor de Cillia
renderShinyLikert = function( data,
                              dropdown_factors = NULL,
                              questions = names( data$likert_data ),
                              height = NULL,
                              response_levels = levels( data$likert_data[ , 1 ] ),
                              split_factors = NULL,
                              group = NULL,
                              grouping = "likert",
                              id = toString( sample( 10^15, 1 ) ),
                              wrap = 30,
                              custom_tests = NULL,
                              ... ){
  env = parent.frame()

  # break scoping here. always be careful with those calls
  input  = get( 'input',   envir=env )
  output = get( 'output',  envir=env )

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

  testMap = createTestMap( custom_tests )

  select_test = renderUI({
    selectInput( paste0(id,".test" ),
                 "Choose test method",
                 names( testMap )
    )
  })

  # create filtered version of dataset
  filtered_data = reactive({
    filterDataSet( data,
                   dropdown_factors,
                   currentFactors()
    )
  })

  # create plot and table ouputs
  plot = renderShinyPlot( dropdown_factors,
                          currentFactors,
                          height,
                          id,
                          response_levels,
                          split_factors,
                          group,
                          grouping,
                          filtered_data,
                          wrap,
                          ... )

  # create the rendered table
  table = renderTable(
    {
      test_method   = getInput( ".test",
                              "kruskal-wallis test" )
      test_function = testMap[[ test_method ]]

      renderTestTable( filtered_data(),
                       getInput( ".group" ),
                       getInput( ".split_factors"  ),
                       test_function )
    },
    include.rownames=FALSE
  )

  # use an inputPanel container for the inputs
  selectorUI = renderUI({
    selectorList = selector()
    names( selectorList ) = NULL
    do.call( "inputPanel", selectorList )
  })

  # create input panel for selecting a test in case it is needed
  if( is.null( split_factors ) && is.null( group ) )
    test_selector = NULL
  else
    test_selector = inputPanel( select_test )

  # return
  out =
  list(
    selector = selectorUI,
    plot = renderUI({
      tabsetPanel(
        tabPanel(
          "plot",
          # create height slider ( returns NULL if height is not supplied )
          renderHeightSlider( id, height ),
          plot
        ),
        tabPanel( "table",
                  test_selector,
                  table
        )
      ) })
  )
  class( out ) = "shinyLikertOutput"
  return( out )

}
