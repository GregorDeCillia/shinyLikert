renderTestTable = function( dataset, factors, currentFactors,
                            likert_split, split_factors )
{
  #renderTable({
    filtered = filterDataSet( dataset,
                              factors,
                              currentFactors() )

    if( !is.null( likert_split ) ){
      out = create_factorized_table( filtered,
                                     likert_split,
                                     accumulate = FALSE )
      out = data.frame( out, p.value = NA )
      for ( question in out$factor[2:nrow(out)] ){
        p = chisq.test( filtered$likert_data[, question],
                        filtered$row_factors[ ,likert_split[1] ],
                        simulate.p.value = TRUE )$p.value
        out$p.value[ out$factor == question ] = p#p.adjust( p, nquestions )
      }

      row.names( out ) = NULL
      return( out )
    }
    if( !is.null( split_factors ) ){
      out = create_factorized_table( filtered,
                                     split_factors )
      out = data.frame( out, p.value = NA )
      for ( factor in split_factors )
        if( factor %in% names( dataset$row_factors ) ){
          # calculate p value from chi squared test
          p = chisq.test( filtered$likert_data[,1],
                          filtered$row_factors[,factor],
                          simulate.p.value = TRUE )$p.value
          out$p.value[ out$factor == factor ] = p
        }
      return( out )
    }

    create_table( filtered$likert_data )
  #})
}
