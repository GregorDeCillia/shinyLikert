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
  reactiveLikertTable = reactive({
    renderTestTable( filtered_data(),
                     likert_split, split_factors )

  })

  out = renderPlot({
    if( is.null( currentFactors() ) && length( factors ) != 0 )
      return( NULL )

    # filter the data according to the dropbox inputs
    filtered = filtered_data()

    # in case of an ordinary plot
    likert_table = create_table( filtered$likert_data,
                                 response_levels )

    filtered$output[[ paste0(id, ".factorTable" ) ]] =
      renderTable( likert_table )

    # in case of a split-plot
    likert_table2 = create_factorized_table( filtered,
                                             split_factors,
                                             response_levels )


    if( is.null( likert_table2 ) && length( currentFactors() ) != 0 ) # triggers when impossible factor
      # combinations are chosen
      return( NULL )

    # set default arguments
    defaults <- list( main = currentFactors() )
    args <- modifyList( defaults,
                        list( x = likert_table,
                              ... )
                        )

    #cat( is.null( split_factors ) )
    if( !is.null( likert_split ) ){
      td = likert::likert( filtered$likert_data,
                           grouping = filtered$row_factors[ , likert_split ] )
      if( grouping == "HH" )
        return( HH::likert( td ) )
      return( likert.bar.plot( td ) )
    }

    if( is.null( split_factors ) ){
      print( likert_table )
      return( do.call( getFromNamespace("likert","HH"), args ) )
    }

    if( !is.null( split_factors ) ){
      print( likert_table2 )
      HH::likert( level ~ . | factor, likert_table2,
              scales = list( cex = 1,
                             y = list( relation = "free" )
              ),
              main = currentFactors(),
              strip.left = strip.custom( bg = "gray85" ),
              par.strip.text = list( cex = 1, lines = 5 ),
              layout = c( 1, length( split_factors ) + 1  ),
              strip = FALSE,
              ylab = NULL,
              ...
      )
    }

  },
  height = function(){
    if( is.null( height ) )
      return( 450 )
    high = filtered_data()$input[[ paste0( id, ".height" ) ]]
    if( is.null( high ) )
      return( 450 )
    return ( high )
  }
  )
  return( list( plot = out,
                table = renderTable({
                  likert_table = reactiveLikertTable()
                  })
                )
          )
}
