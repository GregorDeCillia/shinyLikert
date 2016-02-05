renderTestTable = function( filtered,
                            likert_split,
                            split_factors,
                            id,
                            test_method )
{
    if( !is.null( likert_split ) ){
      out = create_factorized_table( filtered,
                                     likert_split,
                                     accumulate = FALSE )
      out = data.frame( out, p.value = NA )
      for ( question in out$factor[2:nrow(out)] ){
        if( test_method == "chisq test" )
          p = chisq.test( filtered$likert_data[, question],
                          filtered$row_factors[ ,likert_split[1] ],
                          simulate.p.value = TRUE )$p.value
        else
          p = kruskal.test(filtered$likert_data[, question],
                           filtered$row_factors[ ,likert_split[1] ],
                           simulate.p.value = TRUE )$p.value
        out$p.value[ out$factor == question ] = p
      }

      row.names( out ) = NULL
      return( out )
    }
    if( !is.null( split_factors ) ){
      split_factors = filtered$input[[paste0(id,".split_factors")]]
      out = create_factorized_table( filtered,
                                     split_factors )
      out = data.frame( out, p.value = NA )
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
