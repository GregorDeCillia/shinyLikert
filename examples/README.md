# shinyLikert examples
Gregor de Cillia  

Here you will find some examples of the usage of shinyLikert. Each example uses one folder and is written in shiny-markdown format (.Rmd). To run the examples onine, visit 

http://gregor-de-cillia.xyz/shiny/shinyLikert/examples/.

If you want to run those examples locally on your computer, you will have to install shinyLikert. To install the library, type the following in your terminal.

```r
devtools::install_github( "gregorDeCillia/shinyLikert" )
```

Then open one of the the examples in RStudio and strart it using the "Run Document" button.

Example         | Description
----------------|------------------------------
[misc]          | Simple examples on how to use shinyLikert
[cattellData]   | Quick overview of [Cattell's 16 Personality Factors Test]. The raw data can be found [here].
[chisqTest]     | some applications of the Chi-Square Test of Homogeneity
[factorOverview]| Demonstration of the `renderFactorOverview` function
[ui-server]     | Shows how to use this package in a ui/server setup
[hfactors]      | Gives an overview how several dropdown factors interact
[reactiveInputs]| Demonstartes how to pass reactive expressions to renderShinyLikert


[misc]:           misc/
[cattellData]:    cattellData/
[Cattell's 16 Personality Factors Test]: http://personality-testing.info/tests/16PF.php
[here]:           http://personality-testing.info/_rawdata/
[chisqTest]:      chisqTest/ 
[factorOverview]: factorOverview/
[ui-server]:      ui-server/
[hfactors]:       hierachicalFactors/
[reactiveInputs]: reactive_inputs/



