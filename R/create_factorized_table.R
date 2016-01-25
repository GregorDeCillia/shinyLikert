create_factorized_table = function( data_set,
                                    split_factors,
                                    response_levels = levels(
                                      data_set$likert_data[,1]
                                      ),
                                    accumulate = TRUE
                                    )
{
  out = create_table( data_set$likert_data, response_levels )
  if( is.null(out) || dim( out )[1]==0 || dim(out)[2] == 0 )
    return( NULL )
  #if( accumulate ){
    out = colSums( out )
    out = data.frame( cbind( t( out ) ), "all", "", stringsAsFactors = FALSE )
  #}
  #if ( accumulate )
    names(out) = c( response_levels, "factor", "level" )
  #if  (!accumulate)
  #  names(out) = c( response_levels, "question", split_factors[1] )


  for ( factor in split_factors ){
    if ( factor %in% names( data_set$row_factors ) )
      current_levels = levels( data_set$row_factors[[factor]] )
    if ( factor %in% names( data_set$column_factors ) )
      current_levels = levels( data_set$column_factors[[factor]] )
    for ( level in current_levels ){
      newout = create_table( filterDataSet( data_set,
                                            factor,
                                            level
                                            )$likert_data,
                             response_levels )
      if( accumulate ){
      newout = colSums( newout )
      newout = data.frame( cbind( t(newout) ,
                                  factor,
                                  level ),
                           stringsAsFactors = FALSE )
      }
      if( !accumulate )
        newout = data.frame( cbind( newout ,
                                    rownames(newout),
                                    level ),
                             stringsAsFactors = FALSE )
      names(newout) = c( response_levels, "factor", "level" )
      out = rbind( out, newout )
    }

  }
  out$factor = as.factor( out$factor )
  out$level  = as.factor( out$level )

  for ( response in response_levels )
    out[,response] = as.numeric( out[ ,response ] )

  # HH::likert( level ~ . | factor , tabular,
  #             scales=list(y=list(relation="free")), layout=c(1,2) )

  return( out )
}
