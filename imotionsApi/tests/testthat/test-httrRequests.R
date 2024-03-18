context("Testing `httrRequests` functions")
# stopOnHttpError =====================================================================================================
context("stopOnHttpError()")

library(mockery)

mockResponse <- list(status_code = 200)

mockedStopOnHttpError <- function(response) {
    status_code_Stub <- mock(response)

    mockr::with_mock(status_code = status_code_Stub, {
        stopOnHttpError(mockResponse, "Test API")
    })
}

test_that("error - token is not authorized", {
    expect_error(mockedStopOnHttpError(response = 401), "Test API - Token not authorized to access requested resource",
                 info = "error 401 is not handled properly")
})

test_that("error - resource is not found", {
    expect_error(mockedStopOnHttpError(response = 404), "Test API - Resource not found",
                 info = "error 404 is not handled properly")
})

test_that("error - something unexpected happened", {
    expect_error(mockedStopOnHttpError(response = 135), "Test API - unexpected response status (135)",
                 fixed = TRUE, info = "unexpected error is not handled properly")
})

test_that("remote/local check - should work correctly", {
    # Local connection
    error <- mockedStopOnHttpError(response = 200)
    expect_null(error, info = "should not produce an error")

    # Remote connection
    error <- mockedStopOnHttpError(response = 204)
    expect_null(error, info = "should not produce an error")
})

# retryHttr ===========================================================================================================
context("retryHttr()")

connection <- jsonlite::unserializeJSON(readLines("../data/imConnection.json"))
connection_cloud <- jsonlite::unserializeJSON(readLines("../data/imConnection_cloud.json"))
terminate_on <- 404
config <- tokenHeaders(connection$token)


mockedRetryHttr <- function(message, url, mockResponse) {
    if (mockResponse == "error") {
        RETRY_Stub <- mock(stop("Error"))
    } else {
        RETRY_Stub <- mock(mockResponse)
    }

    response <- mockr::with_mock(RETRY = RETRY_Stub, {
        retryHttr(message, "GET", url, config, terminate_on)
    })

    expect_args(RETRY_Stub, 1, "GET", url, config, terminate_on, 3, 2, 60, 1, FALSE)
    return(response)
}

test_that("return - a valid response in case of a valid request", {
    mockResponse$status_code <- 200
    class(mockResponse) <- "response"
    response <- mockedRetryHttr("Test Message", "testurl", mockResponse)
    expect_identical(response, mockResponse, "response should not have been modified")

})

test_that("error - non existing url should throw an error", {
    mockResponse <- "error"

    expect_error(mockedRetryHttr("Test Message", "testurl", mockResponse),
                 "Test Message - Cannot access: testurl",
                 info = "should handle error from the RETRY function")
})

test_that("error - invalid response", {
    mockResponse$status_code <- 404
    class(mockResponse) <- "response"

    expect_error(mockedRetryHttr("Test Message", "testurl", mockResponse),
                 "Test Message - url: testurl - Resource not found",
                 info = "should handle error from the stopOnHttpError function")
})

# getHttr =============================================================================================================
context("getHttr()")

mockedGetHttr <- function(connection, url, mockResponse, terminate_on) {
    config <- tokenHeaders(connection$token)
    retryHttr_Stub <- mock(mockResponse)

    response <- mockr::with_mock(retryHttr = retryHttr_Stub, {
        getHttr(connection, url, "Test API")
    })

    expect_args(retryHttr_Stub, 1, "Test API", "GET", url, config, terminate_on)
    return(response)
}

test_that("remote/local check - should work correctly", {
    # Local connection
    mockResponse$status_code <- 200
    class(mockResponse) <- "response"
    response <- mockedGetHttr(connection, "url", mockResponse, terminate_on)
    expect_identical(response, mockResponse, "response should not have been modified")

    # Remote connection
    mockResponse$status_code <- 204
    class(mockResponse) <- "response"
    response <- mockedGetHttr(connection_cloud, "url", mockResponse, terminate_on = NULL)
    expect_identical(response, mockResponse, "response should not have been modified")
})

# getJSON =============================================================================================================
context("getJSON()")

mockedGetJSON <- function(connection, url, mockResponse) {
    getHttr_Stub <- mock(mockResponse)

    data <- mockr::with_mock(getHttr = getHttr_Stub, {
        getJSON(connection, url, "Test API")
    })

    expect_args(getHttr_Stub, 1, connection, url, "Test API")
    return(data)
}

test_that("return - JSON file if a good request has been sent", {
    mockData <- '{"userId":1,"id":1,"title":"delectus aut autem"}'
    mockResponse <- list(status_code = 200, content = charToRaw(mockData))
    class(mockResponse) <- "response"

    expectedReturn <- list(userId = 1, id = 1, title = "delectus aut autem")

    #in case of good request - send back a JSON file
    data <- mockedGetJSON(connection, "url", mockResponse)
    expect_equal(data, expectedReturn, info = "wrong object returned")
})

# getFile =============================================================================================================
context("getFile()")

# Get the sensors through the cloud
sensors_cloud <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imSensorList_cloud.json")))

mockedGetFile <- function(connection, url, mockResponse, fileName) {
    getHttr_Stub <- mock(mockResponse)

    fileInfos <- mockr::with_mock(getHttr = getHttr_Stub, {
        getFile(connection, url, "Test API", fileName)
    })

    expect_args(getHttr_Stub, 1, connection, url, "Test API")
    return(fileInfos)
}

test_that("remote return - correct paths to the file", {
    # Case with eyetracking data (zip file)
    eyetracking_fileName <- sensors_cloud[2, ]$fileName
    url <- "file://../data/example_data_cloud.zip"
    mockResponse <- list(headers = list("content-type" = "application/zip"), url = url)
    class(mockResponse) <- "response"

    fileInfos <- mockedGetFile(connection_cloud, url, mockResponse, eyetracking_fileName)

    expected_path <- paste0("ProgramData/iMotions/Lab_NG/Data/RRRock The R/Signals/",
                            "20e73a6c-f2ae-4146-90f7-1430bbc9857b/ET_Eyetracker.csv")

    expect_identical(fileInfos$file_path, file.path(gsub("\\\\", "/", fileInfos$tmp_dir), expected_path),
                     "wrong file found")

    expect_true(file.exists(fileInfos$file_path), info = "file should exists")
    unlink(c(fileInfos$file_path, file.path(fileInfos$tmp_dir, "ProgramData")), recursive = TRUE)
    expect_false(file.exists(fileInfos$file_path), info = "file should have been deleted")

    # Case with slideEvents data (csv file)
    events_fileName <- sensors_cloud[2, ]$fileName
    url <- "file://../data/Native_SlideEvents_cloud.csv"
    mockResponse <- list(headers = list("content-type" = "application/octet-stream"), url = url)
    class(mockResponse) <- "response"

    fileInfos <- mockedGetFile(connection_cloud, url, mockResponse, events_fileName)

    expected_path <- "tmp_data.csv"
    expect_identical(fileInfos$file_path, file.path(fileInfos$tmp_dir, expected_path), "wrong file found")

    expect_true(file.exists(fileInfos$file_path), info = "file should exists")
    unlink(c(fileInfos$file_path, file.path(fileInfos$tmp_dir, "ProgramData")), recursive = TRUE)
    expect_false(file.exists(fileInfos$file_path), info = "file should have been deleted")
})

# postHttr ============================================================================================================
context("postHttr()")

body <- '{"flowName":"flowName","sampleName":"TestSensor","fileName":"../data/testFile.csv"}'
class(body) <- "json"

mockedPostHttr <- function(connection, url, mockResponse, body) {
    config <- tokenHeaders(connection$token)
    retryHttr_Stub <- mock(mockResponse)

    response <- mockr::with_mock(retryHttr = retryHttr_Stub, {
        postHttr(connection, url, body, "Test API")
    })

    expect_args(retryHttr_Stub, 1, "Test API", "POST", url, body, config, jsonHeaders(), "json")
    return(response)
}

test_that("remote/local check - should work correctly", {
    # Local connection
    mockResponse$status_code <- 200
    class(mockResponse) <- "response"
    response <- mockedPostHttr(connection, "url", mockResponse, body)
    expect_identical(response, mockResponse, "response should not have been modified")

    # Remote connection
    mockResponse$status_code <- 204
    class(mockResponse) <- "response"
    response <- mockedPostHttr(connection_cloud, "url", mockResponse, body)
    expect_identical(response, mockResponse, "response should not have been modified")
})

# postJSON ============================================================================================================
context("postJSON()")

mockedPostJSON <- function(connection, url, body, mockResponse = NULL) {
    postHttr_Stub <- mock(mockResponse)

    fileInfos <- mockr::with_mock(postHttr = postHttr_Stub, {
        postJSON(connection, url, body, "Test Upload API")
    })

    expect_args(postHttr_Stub, 1, connection, url, body, "Test Upload API")
    return(fileInfos)
}

test_that("local check - should upload a JSON file if a good request has been sent", {
    # prepare mock results
    mockData <- '{"filePath":"C:\\\\Users\\\\exampleUrls"}'
    mockResponse <- list(status_code = 200, content = charToRaw(mockData))
    class(mockResponse) <- "response"

    # in case of good request - send back the url
    expectedReturn <- list(filePath = "C:\\Users\\exampleUrls")
    fileInfos <- mockedPostJSON(connection, "url", body, mockResponse)
    expect_identical(fileInfos, expectedReturn, info = "wrong object returned")
})


test_that("remote check - should get back correct information if asking for credential", {
    # prepare mock results
    expectedReturn <- fromJSON("../data/uploadCredential_cloud.json")
    mockData <- as.character(toJSON(expectedReturn))
    mockResponse <- list(status_code = 200, content = charToRaw(mockData))
    class(mockResponse) <- "response"

    # in case of good request - send back the whole table
    fileInfos <- mockedPostJSON(connection_cloud, "url", body, mockResponse)
    expect_identical(fileInfos, expectedReturn, info = "wrong object returned")
})

# putHttr =============================================================================================================
context("putHttr()")

mockedPutHttr <- function(connection, url, mockResponse, fileName = NULL, reqBody = NULL, expectedBody = NULL,
                          expectedConfig = NULL) {

    retryHttr_Stub <- mock(mockResponse)

    mockr::with_mock(retryHttr = retryHttr_Stub, {
        putHttr(connection, url, fileName, reqBody, "Test Put API")
    })

    expect_args(retryHttr_Stub, 1, "Test Put API", "PUT", url, body = expectedBody, config = expectedConfig)
}

test_that("remote check - should work correctly", {
    # With a file pushed
    mockResponse$status_code <- 200
    class(mockResponse) <- "response"
    config <- csvHeaders()
    fileName <- "../data/testFile.csv"
    expectedBody <- httr::upload_file(fileName)
    mockedPutHttr(connection_cloud, "url", mockResponse, fileName, expectedBody = expectedBody, expectedConfig = config)

    #  With no file pushed
    mockResponse$status_code <- 204
    class(mockResponse) <- "response"
    config <- tokenHeaders(connection_cloud$token)
    mockedPutHttr(connection_cloud, "url", mockResponse, expectedConfig = config)

    #  With a json object pushed
    config <- c(tokenHeaders(connection_cloud$token), jsonHeaders())
    expectedBody <- "test"
    mockedPutHttr(connection_cloud, "url", mockResponse, reqBody = "test", expectedBody = expectedBody,
                  expectedConfig = config)
})
