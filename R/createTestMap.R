# returns a list of tests that can be used for the table output
# custom tests will be appended to this list
createTestMap = function( custom_tests = NULL ){
  #create named list of default tests
  default_tests = list(
    # kruskal-wallis test (H-Test). Similar to the wilcox test(see below),
    # but for factors with more than one level
    "kruskal-wallis test" = function( item, factor ){
      kruskal.test( item,
                    factor
                    )$p.value
    },
    # chi-squared test of independence from the stats library
    "chisq test"          = function( item, factor ){
      chisq.test( item,
                  factor,
                  simulate.p.value = TRUE
                  )$p.value
    },
    # wilcoxon-mann-whitney-U-test. Compares the mean rank sums of two data
    # groups to test for independence. Since it only can be applied for factor
    # variables with two levels, the test is applied for each level seperately
    "wilcox test"         = function( item, factor ){
      item = as.numeric( item )
      levels = levels( factor )
      p = rep( NA, length( levels ) )
      for ( i in 1:length( levels ) ){
        # apply a pairwise test
        binary_factor = levels[i] == factor

        p[i] = wilcox.test( item[  binary_factor ],
                            item[ !binary_factor ]
                            )$p.value
      }
      return( p )
    },
    "levene test"        = function( item, factor ){
      car::leveneTest( as.numeric( item ),
                       factor
                       )$'Pr(>F)'[1]
    }
  )
  # append the custom tests to the defaults as output. Put the custom
  # test at the beginning to ensure those are selected when the widget
  # is created
  return( c( custom_tests, default_tests ) )
}
