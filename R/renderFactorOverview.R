#' Create widgets to visualize the factor variables
#'
#' @param data   the datasetto be shown
#' @param id     a unique id of the output
#' @param env    argument for shiny session
#' @param quoted argument for shiny session
#' @param expr   argument for shiny session
#' @param ...    argument for shiny session
#'
#' @examples
#' \dontrun{
#' shinyApp(
#'     ui= fluidPage(
#'         uiOutput( "selector" ),
#'         uiOutput( "plot" )
#'     ),
#'     server = function(input,output,session){
#'         testData3 = createTestData( participants = 100,
#'                                     questions    = 10,
#'                                     input,
#'                                     output )
#'         rendered = renderFactorOverview(
#'             testData3
#'         )
#'         output$plot     = renderUI({rendered$plot})
#'         output$selector = renderUI({rendered$selector})
#'    }
#' )
#' }
#'
#' @export
#' @importFrom shiny selectInput reactive sliderInput renderUI tabPanel
#' tabsetPanel renderPlot renderTable
#' @import ggvis
#'
renderFactorOverview = function(
  data,
  id= toString(paste0("id",
                      sample(1:10000, 1))),
  env = parent.frame(),
  quoted = FALSE,
  expr= "NULL",
  ... )
{
  input  = get( 'input',   envir=env )
  output = get( 'output',  envir=env )
  factor = reactive({
    input[[paste0(id,"factor")]]
  })

  factor_vec <- reactive({
    factor = factor()
    if( is.null( factor ) ){
      factor = c(names(data$row_factors),
                 names(data$column_factors ) )[1]
    }

    if( factor %in% names( data$row_factors ) )
      factor_vec = data.frame( x = data$row_factors[, factor] )
    else
      factor_vec = data.frame( x = data$column_factors[, factor] )
    names( factor_vec ) = "x"
    return( factor_vec )
  })

  quantity = reactive({
    if( factor() %in% names( data$row_factors ) )
      return( "person" )
    else
      return( "question" )
  })

  #plot = reactive({
  factor_vec %>%
    ggvis( x = ~x ,
           width = "1800px" ) %>%
    layer_bars( fill               = "sky blue",
                fillOpacity       := 0.7,
                fillOpacity.hover := 0.9 ) %>%
    add_axis( "y", title = "count" ) %>%
    add_axis( "x", title = "" ) %>%
    add_tooltip( function( df ) {
      if ( df$stack_upr_ > 1 )
        paste0( df$stack_upr_, " ", quantity(), "s" )
      else
        paste0( df$stack_upr_, " ", quantity() )
    },

    "hover" ) %>%
    add_tooltip( function( df ) {
      paste0( 100*df$stack_upr_/length(factor_vec()$x), " %" )
    },

    "click" ) %>%
      hide_legend( "fill" ) %>%
      bind_shiny( paste0(id, "ggvis"),
                  paste0(id, "ggvis_ui" ) )

  selector = selectInput( paste0(id,"factor"), "choose factor",
                          c(names(data$row_factors),
                            names(data$column_factors ) )
  )

  return( list( selector = selector,
                plot = ggvisOutput( paste0(id, "ggvis") )
                ) )

}
