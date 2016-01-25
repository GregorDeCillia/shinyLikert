renderHeightSlider = function( id, height )
{
    if ( is.null( height ) )
      return ( NULL )

    if ( length( height ) != 2 )
      return ( NULL )

    return (
        sliderInput(
          paste0( id,".height"),
          "height",
          height[1],
          height[2],
          ( height[1] + height[2] )/2
        )
    )
}
