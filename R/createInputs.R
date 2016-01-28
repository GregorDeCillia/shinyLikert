createInputs = function( id,
                         dropdown_factors,
                         split_factors,
                         row_factors,
                         column_factors,
                         currentFactors,
                         getInput,
                         height
)
{
 reactive({
   # force reactivity with respect to dropdown choices
   x = currentFactors()

   # create dropdown menus
   out = create_dropdown_selector( id, dropdown_factors,
                                   row_factors, column_factors,
                                   currentFactors()
   )

   # create height slider ( returns NULL if height is not supplied )
   out$heightSlider = renderHeightSlider( id, height )

   # in case split_factors are given, create a multidropdown
   if( ! is.null( split_factors )  ){
     selection = getInput( ".split_factors", split_factors )
     out$mulipanel = selectInput(
       inputId = paste0( id, ".split_factors" ),
       label   = "split factors",
       # make all factors available, that are not used by dropdowns already
       choices = setdiff(
         union(
           names( row_factors),
           names( column_factors )
         ),
         dropdown_factors
       ),
       selected = selection,
       multiple = TRUE )
   }

   # return as list
   return( out )
 })
}
