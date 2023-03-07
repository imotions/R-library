library(testthat)
library(imotionsApi)

test_check("imotionsApi", reporter = JunitReporter$new(file = "test-results-tests.xml"))
