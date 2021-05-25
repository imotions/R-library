library("imotionsApi");
library("stubthat");

context("getJSON()");

connection <- list(token = "whatever", baseUrl = "nonsense")
class(connection) <- c("imConnection", "list")

content_Stub <- stub(content)
fromJSON_Stub <- stub(fromJSON)
getHttr_Stub <- stub(getHttr)

mockedGetJSON <- function(connection, url, response) {
    getHttrStatusCode_Stub <- stub(getHttrStatusCode)
    getHttrStatusCode_Stub$returns(eval(response))

    mockr::with_mock(getHttr = getHttr_Stub$f,
                     getHttrStatusCode = getHttrStatusCode_Stub$f,
                     content = content_Stub$f,
                     fromJSON = fromJSON_Stub$f,
                     getJSON(connection, url, "Test API"))
}

test_that("should throw an error when token is not authorized", {
    error <- capture_error(mockedGetJSON(connection, "url", response = 401))

    expect_identical(error$message, "Test API - Token not authorized to access requested resource",
                     "error 401 is not handled properly")

    expect_equal(content_Stub$calledTimes(), 0, info = "content function should not be called")
    expect_equal(fromJSON_Stub$calledTimes(), 0, info = "fromJSON function should not be called")
})

test_that("should throw an error when resource is not found", {
    error <- capture_error(mockedGetJSON(connection, "url", response = 404))

    expect_identical(error$message, "Test API - Resource not found", "error 404 is not handled properly")
    expect_equal(content_Stub$calledTimes(), 0, info = "content function should not be called")
    expect_equal(fromJSON_Stub$calledTimes(), 0, info = "fromJSON function should not be called")
})

test_that("should throw an error if something unexpected happens", {
    error <- capture_error(mockedGetJSON(connection, "url", response = 135))

    expect_identical(error$message, "Test API - unexpected response status (135)",
                     "unexpected error is not handled properly")

    expect_equal(content_Stub$calledTimes(), 0, info = "content function should not be called")
    expect_equal(fromJSON_Stub$calledTimes(), 0, info = "fromJSON function should not be called")
})


test_that("should return a JSON file if a good request has been sent", {
    mockData <- '{"userId":1,"id":1,"title":"delectus aut autem"}'
    expectedReturn <- list(userId = 1, id = 1, title = "delectus aut autem")

    #in case of good request - send back a JSON file
    mockResponse <- list(status_code = 200, content = charToRaw(mockData))

    getHttr_Stub$returns(mockResponse)

    content_Stub$expects(x = mockResponse)
    content_Stub$returns(mockData)

    fromJSON_Stub$expects(txt = mockData)
    fromJSON_Stub$returns(expectedReturn)

    data <- mockedGetJSON(connection, "url", response = 200)

    expect_identical(data, expectedReturn, info = "wrong object returned")
    expect_equal(content_Stub$calledTimes(), 1, info = "content function should be called once")
    expect_equal(fromJSON_Stub$calledTimes(), 1, info = "fromJSON function should be called once")
})
