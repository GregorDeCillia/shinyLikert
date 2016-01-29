# shinyLikert examples
Gregor de Cillia  

Here you will find some examples of the usage of shinyLikert. If you want to run those examples yourself, you will have to istall shinyLikert like so


```r
devtools::install_github("gregorDeCillia/shinyLikert")
```


Link            | Description
----------------|------------------------------
[misc]          | Simple examples on how to use shinyLikert
[cattellData]   | Quick overview of [Cattell's 16 Personality Factors Test]. The raw data can be found [here].
[chisqTest]     | some applications of the Chi-Square Test of Homogeneity
[factorOverview]| Demonstration of the `renderFactorOverview` function
[ui-server]     | Shows how to use this package in a ui/server setup
[hfactors]      | Gives an overview how several dropdown factors interact


[misc]:           misc/
[cattellData]:    cattellData/
[Cattell's 16 Personality Factors Test]: http://personality-testing.info/tests/16PF.php
[here]:           http://personality-testing.info/_rawdata/
[chisqTest]:      chisqTest/ 
[factorOverview]: factorOverview/
[ui-server]:      ui-server/
[hfactors]:       hierachicalFactors/

The example below shows some functionalities of the package and has been included from http://gregor-de-cillia.xyz/shiny/shinyLikert/examples/shinyTest/


```r
library(shiny)
tags$iframe(
  seamless="seamless",
  height=800, width=900, frameborder=0,
  src="./shinyTest/")
```

<!--html_preserve--><iframe seamless="seamless" height="800" width="900" frameborder="0" src="./shinyTest/"></iframe><!--/html_preserve-->

<p style='text-align:right;'>[$\Uparrow$](./../)</p>
