filterDataSet = function( data_set,
  # likert_data,
                        #  row_factors,
                        #  column_factors  = NULL,
                          active_factors = NULL,
                          active_levels  = NULL )
{
  if ( length( active_factors ) == 0 )
    return( data_set )
  for ( i in 1:length( active_factors ) ){
    dimens = dim( data_set$likert_data )
    if ( dimens[1] == 0 || dimens[2] == 0 )
      return( data_set )
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
        data_set$likert_data = subset( data_set$likert_data,
                                       select = subset )
      }
      if( sum(subset) != 1 )
        data_set$likert_data = data_set$likert_data[ , subset ]
      data_set$column_factors = data_set$column_factors[ subset,  ]
    }
  }

  return( data_set )
}
