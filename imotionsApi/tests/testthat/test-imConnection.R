# imConnection ========================================================================================================
context("imConnection()")

library(mockery)

test_that("error - token missing", {
    # in case of missing token
    expect_error(imConnection(), "You need a token to connect.", info = "missing `token` param not handled properly")
})

test_that("local return - imConnection object", {
    # test message output
    expect_message(connection <- imConnection("xxxxxxxx"), "Connecting to iMotions API... http://localhost:8086\n",
                   info = "wrong message output")

    expect_s3_class(connection, "imConnection")
    expect_identical(connection$baseUrl, "http://localhost:8086", info = "Wrong baseUrl for local connection")
    expect_true(connection$localIM, info = "should be tagged as local connection")
    expect_null(connection$s3BaseUrl, info = "should be null for local connection")
})

test_that("remote error - no baseUrl or s3BaseUrl", {
    # in case of missing baseUrl
    expect_error(imConnection("remote_token"), "You need a baseUrl for remote connection.",
                 info = "missing `baseUrl` param not handled properly")

    # in case of missing s3BaseUrl
    expect_error(imConnection("remote_token", baseUrl = "https://my.imotions.com/testcustom"),
                 "You need a s3BaseUrl for remote connection.",
                 info = "missing `s3BaseUrl` param not handled properly")
})

test_that("remote return - imConnection object", {
    # test message output
    expect_message(connection <- imConnection("remote_token", baseUrl = "https://my.imotions.com/testcustom",
                                              s3BaseUrl = "https://test/"),
                   "Connecting to iMotions API... https://my.imotions.com/testcustom\n",
                   info = "wrong message output")

    expect_s3_class(connection, "imConnection")
    expect_identical(connection$baseUrl, "https://my.imotions.com/testcustom",
                     info = "Wrong baseUrl for custom connection")

    expect_false(connection$localIM, info = "should not be tagged as local connection")
    expect_identical(connection$s3BaseUrl, "https://test/", info = "Wrong s3BaseUrl for remote connection")
})

test_that("check - print function", {
    connection <- imConnection("remote_token", baseUrl = "https://my.imotions.com/testcustom",
                               s3BaseUrl = "https://test/")

    expect_output(print(connection), "Connected to iMotions at https://my.imotions.com/testcustom")
})
