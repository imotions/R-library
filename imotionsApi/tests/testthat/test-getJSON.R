context("getJSON()")

library("imotionsApi")
library("mockery")

connection <- list(token = "whatever", baseUrl = "nonsense")
class(connection) <- c("imConnection", "list")

mockedGetJSON <- function(connection, url, response, mockResponse = NULL) {
    getHttr_Stub <- mock(mockResponse)
    getHttrStatusCode_Stub <- mock(eval(response), cycle = TRUE)

    mockr::with_mock(getHttr = getHttr_Stub,
                     getHttrStatusCode = getHttrStatusCode_Stub, {
                        getJSON(connection, url, "Test API")
                     })
}

test_that("should throw an error when token is not authorized", {
    error <- capture_error(mockedGetJSON(connection, "url", response = 401))

    expect_identical(error$message, "Test API - Token not authorized to access requested resource",
                     "error 401 is not handled properly")
})

test_that("should throw an error when resource is not found", {
    error <- capture_error(mockedGetJSON(connection, "url", response = 404))

    expect_identical(error$message, "Test API - Resource not found", "error 404 is not handled properly")
})

test_that("should throw an error if something unexpected happens", {
    error <- capture_error(mockedGetJSON(connection, "url", response = 135))

    expect_identical(error$message, "Test API - unexpected response status (135)",
                     "unexpected error is not handled properly")
})


test_that("should return a JSON file if a good request has been sent", {
    mockData <- '{"userId":1,"id":1,"title":"delectus aut autem"}'
    mockResponse <- list(status_code = 200, content = charToRaw(mockData))
    class(mockResponse) <- "response"

    expectedReturn <- list(userId = 1, id = 1, title = "delectus aut autem")

    #in case of good request - send back a JSON file
    data <- mockedGetJSON(connection, "url", response = 200, mockResponse)
    expect_equal(data, expectedReturn, info = "wrong object returned")
})
