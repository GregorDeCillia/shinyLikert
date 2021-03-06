create_table = function( dataset, responselevels = NULL ){
    if( is.null( dim( dataset ) ) || dim( dataset )[2] == 0 )
        return ( NULL )

    if ( is.null(responselevels)[1] )
      responselevels =  levels( dataset[, 1 ] )   # fixme: what if levels are missing?

    out = NULL
    for ( i in 1:dim(dataset)[2] ){
      tmp = rep( NA, length(responselevels) )
      for ( j in 1:length(responselevels) )
        tmp[j] = sum( dataset[,i]==responselevels[j] )
      out = rbind( out, tmp )
    }
    colnames(out) = responselevels
    rownames(out) = colnames(dataset)

    return(out)
  }
