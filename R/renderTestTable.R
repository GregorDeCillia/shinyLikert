renderTestTable = function( filtered,
                            likert_split,
                            split_factors,
                            id,
                            test_method )
{
    if( !is.null( likert_split ) ){
      out = likert::likert(
        filtered$likert_data,
        grouping = filtered$row_factors[ ,likert_split[1] ] )
      out = cbind( out$results, p.value = NA )
      for ( question in out$Item ){
        if( test_method == "chisq test" )
          p = chisq.test( filtered$likert_data[, question],
                          filtered$row_factors[ ,likert_split[1] ],
                          simulate.p.value = TRUE )$p.value
        else
          p = kruskal.test(filtered$likert_data[, question],
                           filtered$row_factors[ ,likert_split[1] ],
                           simulate.p.value = TRUE )$p.value
        out$p.value[ out$Item == question ] = p
      }
      out$Group[ out$Group == "NA" ] = NA
      return( out )
    }
    if( !is.null( split_factors ) ){
      out = create_factorized_table( filtered,
                                     split_factors )
      out = cbind( out, p.value = NA )
      for ( factor in split_factors )
        if( factor %in% names( filtered$row_factors ) ){
          # calculate p value from chi squared test
          if( test_method == "chisq test" )
            p = chisq.test( filtered$likert_data[,1],
                            filtered$row_factors[,factor],
                            simulate.p.value = TRUE )$p.value
          else
            p = kruskal.test( filtered$likert_data[,1],
                              filtered$row_factors[,factor] )$p.value
          out$p.value[ out$factor == factor ] = p
        }
      return( out )
    }

    create_table( filtered$likert_data )
}
