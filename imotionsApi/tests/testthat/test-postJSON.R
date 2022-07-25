context("postJSON()")

library("imotionsApi")
library("mockery")

connection <- list(token = "whatever", baseUrl = "nonsense")
class(connection) <- c("imConnection", "list")

body <- '{"flowName":"flowName","sampleName":"TestSensor","fileName":"../data/testFile.csv"}'
class(body) <- "json"

mockedPostJSON <- function(connection, url, postData, response, mockResponse = NULL) {
    postHttr_Stub <- mock(mockResponse)
    getHttrStatusCode_Stub <- mock(eval(response), cycle = TRUE)

    filePath <- mockr::with_mock(postHttr = postHttr_Stub,
                     getHttrStatusCode = getHttrStatusCode_Stub, {
                         postJSON(connection, url, postData, "Test Upload API")
                     })

    expect_args(postHttr_Stub, 1, connection, url, postData)
    return(filePath)
}


test_that("should throw an error when token is not authorized", {
    error <- capture_error(mockedPostJSON(connection, "url", body, response = 401))

    expect_identical(error$message, "Test Upload API - Token not authorized to access requested resource",
                     "error 401 is not handled properly")
})

test_that("should throw an error when resource is not found", {
    error <- capture_error(mockedPostJSON(connection, "url", body, response = 404))

    expect_identical(error$message, "Test Upload API - Resource not found", "error 404 is not handled properly")
})

test_that("should throw an error if something unexpected happens", {
    error <- capture_error(mockedPostJSON(connection, "url", body, response = 135))

    expect_identical(error$message, "Test Upload API - unexpected response status (135)",
                     "unexpected error is not handled properly")
})

test_that("should upload a JSON file if a good request has been sent", {
    # prepare mock results
    mockData <- '{"filePath":"C:\\\\Users\\\\exampleUrls"}'
    mockResponse <- list(status_code = 200, content = charToRaw(mockData))
    class(mockResponse) <- "response"

    #in case of good request - send back the url
    expectedReturn <- list(filePath = "C:\\Users\\exampleUrls")
    filePath <- mockedPostJSON(connection, "C:\\Users\\exampleUrls", body, response = 200, mockResponse)

    expect_identical(filePath, expectedReturn, info = "wrong object returned")
})
