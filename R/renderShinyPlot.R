renderShinyPlot = function( factors,
                            currentFactors,
                            height,
                            id,
                            response_levels,
                            split_factors,
                            likert_split,
                            grouping,
                            filtered_data,
                            wrap,
                            ...
){
  env = parent.frame()
  getInput  = get( 'getInput',   envir=env )

  renderPlot({
    ellipsis = list( ... )
    for( element in names( ellipsis ) ){
      if( is.reactive( ellipsis[[element]] ) )
        ellipsis[[element]] = ellipsis[[element]]()
    }

    if( is.null( currentFactors() ) && length( factors ) != 0 )
      return( NULL )

    # filter the data according to the dropbox inputs
    filtered = filtered_data()

    if( !is.null( likert_split ) ){
      td = likert::likert(
        filtered$likert_data,
        grouping = filtered$row_factors[ , getInput(".group") ] )
      if( grouping == "HH" ){
        defaults = list(
          x = Group ~.| Item,
          data = td$results,
          layout = c( 1, length( levels( td$results$Item ) ) ),
          main = currentFactors(),
          ylab = getInput(".group"),
          strip = lattice::strip.custom( bg = "gray92" )
        )
        args = modifyList( defaults, ellipsis )
        return(
          do.call( getFromNamespace( "plot.likert", "HH"), args )
        )
      }
      args = modifyList( list(l = td), ellipsis )
      return(
        do.call( getFromNamespace("likert.bar.plot","likert"), args )
      )
    }

    if( is.null( split_factors ) ){
      # in case of an ordinary plot
      likert_table = create_table( filtered$likert_data,
                                   response_levels )

      # set default arguments
      defaults <- list( main = currentFactors() )

      ellipsis$x = likert_table

      args = modifyList( defaults,
                         ellipsis )

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
      likert_table2$level  = as.character( likert_table2$level  )
      for ( i in 1:nrow( likert_table2 ) )
        likert_table2$level[i] = paste(
          strwrap(
            likert_table2$level[i],
            wrap
          ),
          collapse = "\n"
        )

      defaults = list(
        x = level ~ . | factor,
        data = likert_table2,
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
        rightAxis=TRUE
      )

      args = modifyList( defaults,
                          ellipsis )

      return( do.call( getFromNamespace("likert","HH"), args ) )
    }

  },
  height = function(){
    getInput( ".height", 450 ) }
  )
}
