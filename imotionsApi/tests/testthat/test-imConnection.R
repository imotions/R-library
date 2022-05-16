context("imConnection()")

library("imotionsApi")
library("mockery")

test_that("should throw errors if token missing", {
    # in case of missing token
    error <- capture_error(imConnection())
    expect_identical(error$message,
                     "You need a token to connect. Please refer to https://my.imotions.com for instructions.",
                     "missing `token` param not handled properly")
})

test_that("check that local connection works as expected", {
    connection <- imConnection("xxxxxxxx")
    expect_true(inherits(connection, "imConnection"), "`connection` param should be an imConnection object")
    expect_identical(connection$baseUrl, "http://localhost:8086", info = "Wrong baseUrl for local connection")
    expect_true(connection$localIM, "should be tagged as local connection")
    expect_true(connection$imAnalysis, "should be tagged as analysis")

    # test message output
    tmp <- capture_message(imConnection("xxxxxxxx"))
    expect_identical(tmp$message, "Connecting to iMotions API... http://localhost:8086\n",
                     "wrong message output")
})

test_that("check that remote connection works as expected", {
    connection <- imConnection("xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")
    expect_true(inherits(connection, "imConnection"), "`connection` param should be an imConnection object")
    expect_identical(connection$baseUrl, "https://my.imotions.com/api", info = "Wrong baseUrl for remote connection")
    expect_false(connection$localIM, "should not be tagged as local connection")
    expect_false(connection$imAnalysis, "should not be tagged as analysis")

    # test message output
    tmp <- capture_message(imConnection("xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"))
    expect_identical(tmp$message, "Connecting to iMotions API... https://my.imotions.com/api\n",
                     "wrong message output")
})

test_that("check that custom baseUrl works as expected", {
    connection <- imConnection("xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx", baseUrl = "https://my.imotions.com/testcustom")
    expect_true(inherits(connection, "imConnection"), "`connection` param should be an imConnection object")
    expect_identical(connection$baseUrl, "https://my.imotions.com/testcustom",
                     info = "Wrong baseUrl for custom connection")
    expect_false(connection$localIM, "should not be tagged as local connection")
    expect_false(connection$imAnalysis, "should not be tagged as analysis")

    # test message output
    tmp <- capture_message(imConnection("xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx", "https://my.imotions.com/testcustom"))
    expect_identical(tmp$message, "Connecting to iMotions API... https://my.imotions.com/testcustom\n",
                     "wrong message output")
})

test_that("print function should work as expected", {
    connection <- imConnection("xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx", baseUrl = "https://my.imotions.com/testcustom")
    expect_output(print(connection), "Connected to iMotions at https://my.imotions.com/testcustom")
})
