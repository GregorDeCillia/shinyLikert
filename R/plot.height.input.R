height.max              =  500
height.min              =  200
height.default          =  350

plot.height.input = function( label ){
  sliderInput( paste0( "plot.height.", label ),
               "plot height",
               height.min, height.max, height.default )
}
