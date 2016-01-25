shiny_likert2 = function( dataset,
                          responselevels = NULL,
                          questions = 1:10,
                          factor = 11 ){
  shinyApp( ui = fluidPage(
      plot.height.input( paste0("main" ) ),
      selectInput( paste0("gender" ), label = names(x = data)[factor],
                   choices = levels(dataset[,factor]) ,
                   width='100%' ),
      plotOutput( "plot1" )
    ),
    server = function(input,output){
      gender = function(){
        gen = input$gender
        if ( is.null( gen ) )
          return (FALSE)
        return (gen)
      }
      out = reactive({ create_table( dataset[dataset[,factor]==gender(),
                                             questions], responselevels  )
      })
      output$plot1 = renderPlot({
        myout = out()
        HH::likert( myout ,
                    main = input[[paste0("gender")]]
        )
      }, height = function(){ input[[paste0("plot.height.main")]]} )
    }, options = list(height = 700)
  )}





