library( shinyLikert )

nquestions = 500

data = createTestData(100, nquestions, "", "" )
chisq.test( data$likert_data[,3],
            data$row_factors[,1],
            simulate.p.value = TRUE )$p.value
x = 1:nquestions
for ( i in 1:nquestions )
  x[i] = chisq.test( data$likert_data[,i], data$row_factors[,1],
                     simulate.p.value = TRUE )$p.value
# Expect equal distribution
hist( x )

load("../cattellData/cattell_data.rda")
data = cattell_data
chisq.test( data$likert_data[1:1000,1],
            data$row_factors[1:1000,4],
            simulate.p.value = TRUE )$p.value
