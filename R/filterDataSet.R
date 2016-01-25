filterDataSet = function( data_set,
  # likert_data,
                        #  row_factors,
                        #  column_factors  = NULL,
                          active_factors = NULL,
                          active_levels  = NULL )
{
  if ( length( active_factors ) == 0 ){
    return( data_set )
#    cat( "back to the sender!" )
#    out = list( likert_data, row_factors, column_factors )
#    names(out) = c( "likert_data", "row_factors", "column_factors" )

#    return( out )
    #return( likert_data )
  }
  for ( i in 1:length( active_factors ) ){
    if( active_factors[i] %in% names( data_set$row_factors ) )
    {
      subset = data_set$row_factors[[active_factors[i] ]] == active_levels[i]
      data_set$likert_data = subset( data_set$likert_data, subset )
      data_set$row_factors = subset( data_set$row_factors, subset )
    }
    if( active_factors[i] %in% names( data_set$column_factors ) ){
      subset = data_set$column_factors[[ active_factors[i] ]] == active_levels[i]
      subset = replace( subset, is.na( subset ), FALSE )
      if( sum( subset ) == 1 ){
        #name = names( likert_data )[subset]
        #likert_data = data.frame( likert_data[ , subset ] )
        #names( likert_data ) = name
        data_set$likert_data = subset( data_set$likert_data,
                                       select = subset )
      }
      if( sum(subset) != 1 )
        data_set$likert_data = data_set$likert_data[ , subset ]
      data_set$column_factors = data_set$column_factors[ subset,  ]
      #FALSE
    }
  }
  #out = list( likert_data, row_factors, column_factors )
  #names(out) = c( "likert_data", "row_factors", "column_factors" )

  #return( out )
  return( data_set )

  #return( likert_data )

}
