library("imotionsApi");
library("stubthat");

context("postJSON()");

connection <- list(token = "whatever", baseUrl = "nonsense")
class(connection) <- c("imConnection", "list")

content_Stub <- stub(content)
fromJSON_Stub <- stub(fromJSON)
postHttr_Stub <- stub(postHttr)

body <- '{"flowName":"flowName","sampleName":"TestSensor","fileName":"../data/testFile.csv"}'
class(body) <- "json"

mockedPostJSON <- function(connection, url, postData, response) {
    getHttrStatusCode_Stub <- stub(getHttrStatusCode)
    getHttrStatusCode_Stub$returns(eval(response))

    mockr::with_mock(postHttr = postHttr_Stub$f,
                     getHttrStatusCode = getHttrStatusCode_Stub$f,
                     content = content_Stub$f,
                     fromJSON = fromJSON_Stub$f,
                     {
                         postJSON(connection, url, postData, "Test Upload API")
                     })
}


test_that("should throw an error when token is not authorized", {
    error <- capture_error(mockedPostJSON(connection, "url", body, response = 401))

    expect_identical(error$message, "Test Upload API - Token not authorized to access requested resource",
                     "error 401 is not handled properly")

    expect_equal(content_Stub$calledTimes(), 0, info = "content function should not be called")
    expect_equal(fromJSON_Stub$calledTimes(), 0, info = "fromJSON function should not be called")
})

test_that("should throw an error when resource is not found", {
    error <- capture_error(mockedPostJSON(connection, "url", body, response = 404))

    expect_identical(error$message, "Test Upload API - Resource not found", "error 404 is not handled properly")
    expect_equal(content_Stub$calledTimes(), 0, info = "content function should not be called")
    expect_equal(fromJSON_Stub$calledTimes(), 0, info = "fromJSON function should not be called")
})

test_that("should throw an error if something unexpected happens", {
    error <- capture_error(mockedPostJSON(connection, "url", body, response = 135))

    expect_identical(error$message, "Test Upload API - unexpected response status (135)",
                     "unexpected error is not handled properly")

    expect_equal(content_Stub$calledTimes(), 0, info = "content function should not be called")
    expect_equal(fromJSON_Stub$calledTimes(), 0, info = "fromJSON function should not be called")
})

test_that("should upload a JSON file if a good request has been sent", {
    # prepare mock results
    mockData <- paste0('{"filePath":"C:\\Users\\exampleUrls"}')
    expectedReturn <- list(filePath = "C:\\Users\\exampleUrls")

    #in case of good request - send back a JSON file
    mockResponse <- list(status_code = 200, content = charToRaw(mockData))
    postHttr_Stub$returns(mockResponse)

    content_Stub$expects(x = mockResponse)
    content_Stub$returns(mockData)

    fromJSON_Stub$expects(txt = mockData)
    fromJSON_Stub$returns(expectedReturn)

    filePath <- mockedPostJSON(connection, "C:\\Users\\exampleUrls", body, response = 200)

    expect_identical(filePath, expectedReturn, info = "wrong object returned")
    expect_equal(content_Stub$calledTimes(), 1, info = "content function should be called once")
    expect_equal(fromJSON_Stub$calledTimes(), 1, info = "fromJSON function should be called once")
})
