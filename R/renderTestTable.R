renderTestTable = function( filtered,
                            group,
                            split_factors,
                            test_method,
                            custom_tests )
{
  if( test_method == "chisq test" )
    test_function = function( item, factor ){
      chisq.test( item, factor,
                  simulate.p.value = TRUE )$p.value
    }
  else{
    test_function = function( item, factor ){
      kruskal.test( item, factor )$p.value
    }
  }
  if( test_method %in% names(custom_tests) )
    test_function = custom_tests[[ test_method ]]

  if( !is.null( group ) ){
    out = likert::likert(
      filtered$likert_data,
      grouping = filtered$row_factors[ ,group[1] ] )
    out = cbind( out$results, p.value = NA )
    for ( question in out$Item ){
      p = test_function( filtered$likert_data[, question],
                         filtered$row_factors[ ,group[1] ] )
      out$p.value[ out$Item == question ] = p
    }
    out$Group[ out$Group == "NA" ] = NA
    return( out )
  }
  if( !is.null( split_factors ) ){
    if( dim(filtered$likert_data)[2]==0 )
      return( NULL )
    out = create_factorized_table( filtered,
                                   split_factors )
    out = cbind( out, p.value = NA )
    for ( factor in split_factors )
      if( factor %in% names( filtered$row_factors ) ){
        p = test_function(
          unlist( filtered$likert_data ),
          rep( filtered$row_factors[,factor],
               ncol( filtered$likert_data ) )
        )
        out$p.value[ out$factor == factor ] = p
      } else{
        fac  = filtered$column_factors[,factor]
        fac2 = NULL
        for ( i in 1:length( fac ) )
          fac2 = c( fac2,
                    rep( fac[i],
                         nrow( filtered$likert_data ) )
          )
        p = test_function(
          unlist( filtered$likert_data ),
          fac2
        )
        out$p.value[ out$factor == factor ] = p
      }
    return( out )
  }

  create_table( filtered$likert_data )
}
