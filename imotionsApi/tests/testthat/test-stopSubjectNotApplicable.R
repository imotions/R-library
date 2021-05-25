library("imotionsApi");
library("stubthat");

context("stopSubjectNotApplicable()");


test_that("should work as expected", {
    error <- capture_error(stopSubjectNotApplicable())
    expect_identical(error$message, "imotionsApi subject not applicable", "should stop and throw an error")

    error <- capture_error(stopSubjectNotApplicable(message = "My custom error"))
    expect_identical(error$message, "My custom error", "should stop with a custom error message")
})
