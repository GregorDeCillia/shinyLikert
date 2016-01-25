\name{testData2}
\alias{testData2}
\docType{data}
\title{
  Testdata in the shinyLikert format.
%%   ~~ data name/kind ... ~~
}
\description{
  It contains measurements of responses (e.g., "very good", "good", ... ) and factors to be used to partitionate the data later on
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("testData2")}
\format{
testData2$likert_data \cr
 $ 'data.frame':	100 obs. of  10 variables: \cr
  ..$ Question 1 : Factor w/ 5 levels "Agree","Disagree",..: 5 2 3 4 3 5 2 4 1 3 ...
  ..$ Question 2 : Factor w/ 5 levels "Agree","Disagree",..: 2 1 2 2 3 4 2 1 3 4 ...
  ..$ Question 3 : Factor w/ 5 levels "Agree","Disagree",..: 5 2 5 1 4 2 3 1 5 5 ...
  ..$ Question 4 : Factor w/ 5 levels "Agree","Disagree",..: 1 1 2 2 4 2 4 2 2 2 ...
  ..$ Question 5 : Factor w/ 5 levels "Agree","Disagree",..: 2 4 4 2 2 4 1 2 2 1 ...
  ..$ Question 6 : Factor w/ 5 levels "Agree","Disagree",..: 1 3 4 3 5 5 2 5 5 2 ...
  ..$ Question 7 : Factor w/ 5 levels "Agree","Disagree",..: 2 1 4 2 5 2 4 4 3 3 ...
  ..$ Question 8 : Factor w/ 5 levels "Agree","Disagree",..: 2 4 4 3 1 4 5 4 4 1 ...
  ..$ Question 9 : Factor w/ 5 levels "Agree","Disagree",..: 4 3 1 2 1 4 4 2 5 2 ...
  ..$ Question 10: Factor w/ 5 levels "Agree","Disagree",..: 3 4 5 1 1 4 4 1 1 3 ...
 $ row_factors   :'data.frame':	100 obs. of  2 variables:
  ..$ gender : Factor w/ 2 levels "Female","Male": 2 1 2 1 2 1 1 2 1 1 ...
  ..$ country: Factor w/ 3 levels "Austria","Germany",..: 2 2 1 3 3 3 2 1 3 1 ...
 $ column_factors:'data.frame':	10 obs. of  1 variable:
  ..$ difficulty: Factor w/ 2 levels "easy","hard": 2 1 1 1 1 2 2 2 2 1
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(testData2)
## maybe str(testData2) ; plot(testData2) ...
}
\keyword{datasets}
