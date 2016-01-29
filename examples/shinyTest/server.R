library(shinyLikert)
shinyServer(
  function(input, output) {
    # import dataset
    load("../cattellData/cattell_data.rda")

    # shorten dataset
    cattell_data$likert_data  = cattell_data$likert_data[1:2000,]
    cattell_data$row_factors  = cattell_data$row_factors[1:2000,]
    cattell_data$row_factors  = cattell_data$row_factors[,c( "source", "gender" )]

    # add question as column factor
    cattell_data$column_factors = cbind(
      cattell_data$column_factors,
      question = names( cattell_data$likert_data )
    )

    rendered = renderShinyLikert(
      cattell_data ,
      dropdown = c( "question_type", "question" ),
      split = "source",
      response_levels = c(
        #"missed",                      # do not plot the missings
                                        # this can be avoided by replacing
                                        # missed with NAs in likert_data
                                        # and dropping the unused factor
                                        # missed afterwards
        "strongly disagree",
        "disagree",
        "neither agree not disagree",
        "agree",
        "strongly agree" ),
      as.percent = TRUE
    )

    output$plot     = rendered$plot
    output$selector = rendered$selector
  }
)
