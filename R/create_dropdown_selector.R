create_dropdown_selector = function( id, factors,
                                     row_factors,
                                     column_factors,
                                     current_factors
                                     ){
  selector2 = list()
  i = 0
  for ( factor2 in factors ){
    i = i + 1
    input_id = paste0(id, ".", factor2)
    if( factor2 %in% names( row_factors ) ){
      choices = levels( row_factors[ , factor2 ] )
      inp = current_factors[i]
      selector2[[ input_id ]] = renderSelector( id,
                                                factor2,
                                                choices,
                                                inp )
      if( is.null( inp ) ){
        inp = choices[1]
      }
      row_factors = droplevels(
        row_factors[ inp == row_factors[ , factor2 ],
                     ]
      )

    }
    if( factor2 %in% names( column_factors ) ){
      choices = levels( column_factors[ , factor2 ] )
      inp = current_factors[i]

      selector2[[ input_id ]] = renderSelector( id,
                                                factor2,
                                                choices,
                                                inp
       )

      if( is.null( inp ) )
        inp = choices[1]

      column_factors = droplevels(
        column_factors[ inp == column_factors[ , factor2 ],
                     ]
      )
    }

  }
  return( selector2 )
}
