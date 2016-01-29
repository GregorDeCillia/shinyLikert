library(shinyLikert)
shinyUI(
  fluidPage(
    uiOutput( "selector" ),
    uiOutput( "plot" )
  )
)
