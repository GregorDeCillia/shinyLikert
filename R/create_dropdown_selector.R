create_dropdown_selector = function( id, factors,
                                     row_factors,
                                     column_factors,
                                     current_factors
                                     ){
  selector2 = list()
  for ( factor2 in factors ){
    input_id = paste0(id, ".", factor2)
    if( factor2 %in% names( row_factors ) ){
      choices = levels( row_factors[ , factor2 ] )
      selector2[[ input_id ]] = renderSelector( id,
                                                factor2,
                                                choices
      )

    }
    if( factor2 %in% names( column_factors ) ){
      choices = levels( column_factors[ , factor2 ] )
      selector2[[ input_id ]] = renderSelector( id,
                                                factor2,
                                                choices
       )
    }

  }
  return( selector2 )
}
