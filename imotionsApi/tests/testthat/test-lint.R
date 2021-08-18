context("Linting")

test_that("code style", {
    lintr::expect_lint_free("imotionsApi")
})
