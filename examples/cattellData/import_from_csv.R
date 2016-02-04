library( shinyLikert )
# load( "cattell_data.rda")

# those are the original import for the cattell_data.rda
# They might be outdated. The data.csv file was
# downloaded from the site http://personality-testing.info/_rawdata/
# The download comes in the form of a zip file containing one .csv
# dataset and a .html documentation
#
# The first 163 columns (A1, A2, ..., P9) are likert items. Those have been
# stored as integer.
#
# 0 = missing (NA)
# 1 = strongly disagree
# 2 = disagree
# 3 = neutral
# 4 = agree
# 5 = strongly agree
#
# The last six columns, namely
#
# "age", "gender", "accuracy", "country", "source", "elapsed"
#
# represent factor variables, which will later be adressed as
# column_factors. Accuracy and elapsed will not be imported since
# the current implementation only supports factor variables and
# likert items.
#
data <- read.delim("C:/Users/grogon/Downloads/data.csv")
question_columns = 1:163
factor_columns   = c( "age", "gender", "country", "source" )

# Use the first letter of the question as factor variable
# The levels are A, B, ..., P correspond to the 16 personality
# factors
question_factors = substr( names( data[,question_columns] ), 1, 1)

ld = createLikertData( data[ , question_columns ],
                       data[ , factor_columns ],
                       data.frame(  question_type = question_factors ),
                       c( "missed",
                          "strongly disagree",
                          "disagree",
                          "neither agree not disagree",
                          "agree",
                          "strongly agree" )
)

# use the first letter of the questions as a factor
question_factors = substr( names( ld$likert_data ), 1, 1)

# get the questions as string vector
source( "cattell_questions.R" )
names(ld$likert_data) = cattell_questions

# mark all NAs as unanswered
#
levels( ld$row_factors$country )[1] = "NA"
ld$row_factors$country[ is.na( ld$row_factors$country ) ] = "NA"

# give names to the genders
levels( ld$row_factors$gender ) = c( "NA",
                                     "Male",
                                     "Female",
                                     "Other"
)

# give names to the sources factor
levels( ld$row_factors$"source" ) = c( "from front page of website that hosted the survey",
                                       "from google",
                                       "facebook",
                                       "any url containing '.edu'",
                                       "wikipedia",
                                       "other source or empty source")
