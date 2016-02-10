#' Create widgets to visualize the factor variables
#'
#' @param data   the datasetto be shown
#' @param id     a unique id of the output
#'
#' @examples
#' \dontrun{
#' shinyApp(
#'     ui= fluidPage(
#'         uiOutput( "selector" ),
#'         uiOutput( "plot" )
#'     ),
#'     server = function(input,output,session){
#'         testData3 = createTestData()
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
#' @importFrom shiny selectInput reactive sliderInput renderUI tabPanel inputPanel
#' tabsetPanel renderPlot renderTable
#' @import ggvis
#'
renderFactorOverview = function(
  data,
  id= toString(paste0("id",
                      sample(1:10000, 1)))
  )
{
  env = parent.frame()

  input  = get( 'input',   envir=env )
  output = get( 'output',  envir=env )

  selector = inputPanel(
    selectInput( paste0( id,
                         "factor" ),
                 "choose factor",
                 c(names(data$row_factors),
                   names(data$column_factors ) )
    ),
    sliderInput( paste0( id, "percent" ),
                 "choose percent",
                 0, 20, 4,
                 post = "%" )
  )

  # quick access to the inputs
  factor = reactive({
    input[[paste0(id,"factor")]]
  })
  percent = reactive({
    out = input[[paste0(id,"percent")]]
    if( is.null(out) )
      out = 4
    return( out )
  })

  factors = union(
    names( data$row_factors ),
    names( data$column_factors )
  )

  factor_vec <- reactive({
    factor = factor()
    if( is.null( factor ) ){
      factor = factors[1]
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

  counts = reactive({
    fv = factor_vec()
    counts = as.numeric( table( fv ) )
    df = data.frame( levels = levels( fv[,1] ),
                     counts,
                     treshold = sum( counts ) )
    return( df )
  })

  counts2 = reactive({
    cts = counts()
    cts = cts[ cts$counts/sum(cts$counts) >= percent()/100,  ]
    cts$levels   = base::factor( cts$levels ) # drop unused factor levels
    cts$treshold = cts$treshold*percent()/100
    return( cts )
  })

  counts2 %>%
    ggvis( ~levels, ~counts ) %>%
    layer_paths(~levels, ~treshold, stroke:="red") %>%
    layer_bars( fill               = "sky blue",
                fillOpacity       := 0.7,
                fillOpacity.hover := 0.9 ) %>%
    add_axis( "y", title = "count" ) %>%
    add_axis( "x", title = "" ) %>%
    add_tooltip( function( df ) {
      if ( df$stack_upr_ > 1 )
        paste0( df$stack_upr_,
                " ",
                quantity(),
                "s" )
      else
        paste0( df$stack_upr_,
                " ",
                quantity() )
    },

    "hover" ) %>%
    add_tooltip( function( df ) {
      paste0( 100*df$stack_upr_/length( factor_vec()$x ),
              " %" )
    },

    "click" ) %>%
      hide_legend( "fill" ) %>%
      bind_shiny( paste0(id, "ggvis"),
                  paste0(id, "ggvis_ui" ) )



  return( list( selector = selector,
                plot = ggvisOutput( paste0(id, "ggvis") )
                ) )

}
