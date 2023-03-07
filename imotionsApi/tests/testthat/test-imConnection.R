context("imConnection()")

library("imotionsApi")
library("mockery")

test_that("should throw errors if token missing", {
    # in case of missing token
    error <- capture_error(imConnection())
    expect_identical(error$message, "You need a token to connect.", "missing `token` param not handled properly")
})

test_that("check that local connection works as expected", {
    connection <- imConnection("xxxxxxxx")
    expect_true(inherits(connection, "imConnection"), "`connection` param should be an imConnection object")
    expect_identical(connection$baseUrl, "http://localhost:8086", info = "Wrong baseUrl for local connection")
    expect_true(connection$localIM, "should be tagged as local connection")
    expect_true(is.null(connection$s3BaseUrl), "should be null for local connection")

    # test message output
    tmp <- capture_message(imConnection("xxxxxxxx"))
    expect_identical(tmp$message, "Connecting to iMotions API... http://localhost:8086\n",
                     "wrong message output")
})

test_that("check that remote connection works as expected and throw warning if no baseUrl, s3BaseUrl", {
    # in case of missing baseUrl
    error <- capture_error(imConnection("remote_token"))
    expect_identical(error$message, "You need a baseUrl for remote connection.",
                     "missing `baseUrl` param not handled properly")

    # in case of missing s3BaseUrl
    error <- capture_error(imConnection("remote_token", baseUrl = "https://my.imotions.com/testcustom"))
    expect_identical(error$message, "You need a s3BaseUrl for remote connection.",
                     "missing `s3BaseUrl` param not handled properly")
})

test_that("check that remote connection works as expected", {
    connection <- imConnection("remote_token", baseUrl = "https://my.imotions.com/testcustom",
                               s3BaseUrl = "https://test/")

    expect_true(inherits(connection, "imConnection"), "`connection` param should be an imConnection object")
    expect_identical(connection$baseUrl, "https://my.imotions.com/testcustom",
                     info = "Wrong baseUrl for custom connection")

    expect_false(connection$localIM, "should not be tagged as local connection")
    expect_identical(connection$s3BaseUrl, "https://test/", info = "Wrong s3BaseUrl for remote connection")

    # test message output
    tmp <- capture_message(imConnection("remote_token", "https://my.imotions.com/testcustom",
                           s3BaseUrl = "https://test/"))

    expect_identical(tmp$message, "Connecting to iMotions API... https://my.imotions.com/testcustom\n",
                     "wrong message output")
})

test_that("print function should work as expected", {
    connection <- imConnection("remote_token", baseUrl = "https://my.imotions.com/testcustom",
                               s3BaseUrl = "https://test/")

    expect_output(print(connection), "Connected to iMotions at https://my.imotions.com/testcustom")
})
