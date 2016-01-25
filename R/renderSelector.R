renderSelector = function( id,
                           factor_id,
                           factor_levels,
                           selected = factor_levels[1] ){

    selectInput(
      paste0( id, factor_id ),
      label    = factor_id,
      choices  = factor_levels ,
      selected = selected,
      width    = '100%'
    )
}
