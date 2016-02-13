require(shinyLikert)
require(likert)
testData = createTestData( 100, 10 )
td = likert::likert( testData$likert_data )
class( td )
likert.bar.plot( td )
likert.heat.plot( td )
td = likert::likert(
  testData$likert_data,
  grouping = testData$row_factors[ , 2 ] )
likert.bar.plot( td, include.histogram = TRUE )
HH::likert( td )
likert.bar.plot( td )
