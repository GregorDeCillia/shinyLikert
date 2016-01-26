data = createTestData(100, 10)
selected_data = subset( data$row_factors, select="country")

plotGgvis = selected_data %>%
  ggvis( x = ~country, width = "1800px" ) %>%
  layer_bars( fill = "sky blue",
              fillOpacity := 0.7,
              fillOpacity.hover := 0.9 ) %>%
  add_axis( "y", title = "persons" ) %>%
  add_axis( "x", title = "" ) %>%
  add_tooltip( function( df ) {
    if ( df$stack_upr_ > 1 )
      paste0( df$stack_upr_, " persons" )
    else
      paste0( df$stack_upr_, " person" )
  },
  "hover" ) %>%
  hide_legend( "fill" )

plotGgvis
