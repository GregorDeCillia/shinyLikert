test_shiny = function(){
  shinyApp(
    ui = fluidPage(
      sidebarLayout(
        sidebarPanel(sliderInput("n", "Bins", 5, 100, 20)),
        mainPanel(plotOutput("hist"))
      )
    ),
    server = function(input, output) {
      output$hist <- renderPlot(
        hist(faithful[[2]], breaks = input$n,
             col = "skyblue", border = "white")
      )
    }
  )
}

renderTimeSeries <- function(expr, env=parent.frame(), quoted=FALSE) {
  # Convert the expression + environment into a function
  func <- exprToFunction(expr, env, quoted)

  function() {
    val <- func()
    list(start = tsp(val)[1],
         end = tsp(val)[2],
         freq = tsp(val)[3],
         data = as.vector(val))
  }
}
