createTestMap = function( custom_tests = NULL ){
  default_tests = list(
    "kruskal-wallis test" = function( item, factor ){
      kruskal.test( item, factor )$p.value
    },
    "chisq test"          = function( item, factor ){
      chisq.test( item, factor,
                  simulate.p.value = TRUE )$p.value
    },
    "wilcox test"         = function( item, factor ){
      item = as.numeric( item )
      levels = levels( factor )
      p = rep( NA, length( levels ) )
      for ( i in 1:length( levels ) ){
        binary_factor = levels[i] == factor
        p[i] = wilcox.test( item[ binary_factor ],
                            item[!binary_factor ] )$p.value
      }
      return( p )
    },
    "levene test"        = function( item, factor ){
      car::leveneTest( as.numeric( item),
                       factor )$'Pr(>F)'[1]
    }
  )

  return( c( custom_tests, default_tests ) )
}
