renderShinyPlot = function( factors,
                            currentFactors,
                            height,
                            id,
                            response_levels,
                            split_factors,
                            likert_split,
                            grouping,
                            filtered_data,
                            env = parent.frame(),
                            ...
){
  getInput  = get( 'getInput',   envir=env )

  reactiveLikertTable = reactive({
    renderTestTable( filtered_data(),
                     getInput( ".group" ),
                     getInput( ".split_factors"  ),
                     getInput( ".test",
                               "kruskal-wallis test" ) )

  })

  out = renderPlot({
    if( is.null( currentFactors() ) && length( factors ) != 0 )
      return( NULL )

    # filter the data according to the dropbox inputs
    filtered = filtered_data()

    # in case of an ordinary plot
    likert_table = create_table( filtered$likert_data,
                                 response_levels )

    # set default arguments
    defaults <- list( main = currentFactors() )
    args <- modifyList( defaults,
                        list( x = likert_table,
                              ... )
                        )

    if( !is.null( likert_split ) ){
      td = likert::likert(
        filtered$likert_data,
        grouping = filtered$row_factors[ , getInput(".group") ] )
      if( grouping == "HH" )
        return( HH::plot.likert(
          x = Group ~.| Item,
          data = td$results,
          layout = c( 1, length( levels( td$results$Item ) ) ),
          main = currentFactors(),
          ylab = getInput(".group"),
          strip = lattice::strip.custom( bg = "gray92" )
        ) )
      return( likert::likert.bar.plot( td ) )
    }

    if( is.null( split_factors ) ){
      if( is.null(likert_table)  )
        return( NULL )
      return( do.call( getFromNamespace("likert","HH"), args ) )
    }

    if( !is.null( split_factors ) ){
      split_factors = getInput( ".split_factors" )
      # in case of a split-plot
      likert_table2 = create_factorized_table( filtered,
                                               split_factors,
                                               response_levels
                                               )


      if( is.null( likert_table2 ) && length( currentFactors() ) != 0 ) # triggers when impossible factor
        # combinations are chosen
        return( NULL )
      likert_table2$factor = as.character( likert_table2$factor )
      HH::likert( level ~ . | factor, likert_table2,
              scales = list( cex = 1,
                             y = list( relation = "free" )
              ),
              main = currentFactors(),
              strip.left = lattice::strip.custom( bg = "gray92" ),
              par.strip.text = list( cex = 1, lines = 5 ),
              layout = c( 1, length( split_factors ) + 1  ),
              strip = FALSE,
              ylab = NULL,
              positive.order=TRUE,
              rightAxis=TRUE,
              ...
      )
    }

  },
  height = function(){
    if( is.null( height ) )
      return( 450 )
    high = getInput( ".height", 450 )
    return ( high )
  }
  )
  return(
    list( plot = out,
          table = renderTable({
            likert_table = reactiveLikertTable()
          }, include.rownames=FALSE )

    )
  )
}
