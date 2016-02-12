createTestMap = function( custom_tests = NULL ){
  default_tests = list(
    "kruskal-wallis test" = function( item, factor ){
      kruskal.test( item, factor )$p.value
    },
    "chisq test"          = function( item, factor ){
      chisq.test( item, factor,
                  simulate.p.value = TRUE )$p.value
    }
  )

  return( c( custom_tests, default_tests ) )
}
