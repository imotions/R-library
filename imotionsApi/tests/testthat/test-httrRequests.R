context("Testing `httrRequests` functions")
context("stopOnHttpError()")

library("imotionsApi")
library("mockery")

mockResponse <- list(status_code = 200)

mockedStopOnHttpError <- function(response) {
    getHttrStatusCode_Stub <- mock(response)

    mockr::with_mock(getHttrStatusCode = getHttrStatusCode_Stub, {
        stopOnHttpError(mockResponse, "Test API")
        })
}

test_that("should throw an error when token is not authorized", {
    error <- capture_error(mockedStopOnHttpError(response = 401))

    expect_identical(error$message, "Test API - Token not authorized to access requested resource",
                     "error 401 is not handled properly")
})

test_that("should throw an error when resource is not found", {
    error <- capture_error(mockedStopOnHttpError(response = 404))
    expect_identical(error$message, "Test API - Resource not found", "error 404 is not handled properly")
})

test_that("should throw an error if something unexpected happens", {
    error <- capture_error(mockedStopOnHttpError(response = 135))
    expect_identical(error$message, "Test API - unexpected response status (135)",
                     "unexpected error is not handled properly")
})

test_that("should work correctly in case of remote/local connection", {
    # Local connection
    error <- capture_error(mockedStopOnHttpError(response = 200))
    expect_null(error, info =  "should not produce an error")

    # Remote connection
    error <- capture_error(mockedStopOnHttpError(response = 204))
    expect_null(error, info =  "should not produce an error")
})

context("getHttr()")

connection <- list(token = "whatever", baseUrl = "nonsense", localIM = TRUE)
class(connection) <- c("imConnection", "list")
config <- tokenHeaders(connection$token)
terminate_on <- 404

mockedGetHttr <- function(connection, url, mockResponse, config, terminate_on) {
    RETRY_Stub <- mock(mockResponse)
    stopOnHttpError_Stub <- mock()

    response <- mockr::with_mock(RETRY = RETRY_Stub,
                                 stopOnHttpError = stopOnHttpError_Stub, {
                                     getHttr(connection, url, "Test API")
                                 })

    expect_args(RETRY_Stub, 1, "GET", url, config, terminate_on)
    expect_args(stopOnHttpError_Stub, 1, mockResponse, "Test API")
    return(response)
}

test_that("should work correctly in case of remote/local connection", {
    # Local connection
    mockResponse$status_code <- 200
    class(mockResponse) <- "response"
    response <- mockedGetHttr(connection, "url", mockResponse, config, terminate_on)
    expect_identical(response, mockResponse, "response should not have been modified")

    # Remote connection
    mockResponse$status_code <- 204
    class(mockResponse) <- "response"
    connection$localIM <- FALSE
    response <- mockedGetHttr(connection, "url", mockResponse, config, terminate_on = NULL)
    expect_identical(response, mockResponse, "response should not have been modified")
})

context("getJSON()")

mockedGetJSON <- function(connection, url, mockResponse) {
    getHttr_Stub <- mock(mockResponse)

    data <- mockr::with_mock(getHttr = getHttr_Stub, {
            getJSON(connection, url, "Test API")
        })

    expect_args(getHttr_Stub, 1, connection, url, "Test API")
    return(data)
}

test_that("should return a JSON file if a good request has been sent", {
    mockData <- '{"userId":1,"id":1,"title":"delectus aut autem"}'
    mockResponse <- list(status_code = 200, content = charToRaw(mockData))
    class(mockResponse) <- "response"

    expectedReturn <- list(userId = 1, id = 1, title = "delectus aut autem")

    #in case of good request - send back a JSON file
    data <- mockedGetJSON(connection, "url", mockResponse)
    expect_equal(data, expectedReturn, info = "wrong object returned")
})

context("getFile()")

# Get the sensors through the cloud
sensors_cloud <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imSensorListCloud.json")))

mockedGetFile <- function(connection, url, mockResponse, fileName) {
    getHttr_Stub <- mock(mockResponse)

    fileInfos <- mockr::with_mock(getHttr = getHttr_Stub, {
        getFile(connection, url, "Test API", fileName)
    })

    expect_args(getHttr_Stub, 1, connection, url, "Test API")
    return(fileInfos)
}

test_that("getFile() should return the correct paths during remote cloud connection", {
    # Case with eyetracking data (zip file)
    eyetracking_fileName <- sensors_cloud[2, ]$fileName
    url <- "file://../data/example_data_cloud.zip"
    mockResponse <- list(headers = list("content-type" = "application/zip"), url = url)
    class(mockResponse) <- "response"

    fileInfos <- mockedGetFile(connection, url, mockResponse, eyetracking_fileName)

    expected_path <- paste0("ProgramData/iMotions/Lab_NG/Data/RRRock The R/Signals/",
                            "20e73a6c-f2ae-4146-90f7-1430bbc9857b/ET_Eyetracker.csv")

    expect_identical(fileInfos$file_path, file.path(gsub("\\\\", "/", fileInfos$tmp_dir), expected_path),
                     "wrong file found")

    expect_true(file.exists(fileInfos$file_path), info = "file should exists")
    unlink(file.path(fileInfos$tmp_dir, "*"), recursive = TRUE)
    expect_false(file.exists(fileInfos$file_path), info = "file should have been deleted")

    # Case with slideEvents data (csv file)
    events_fileName <- sensors_cloud[2, ]$fileName
    url <- "file://../data/Native_SlideEvents_cloud.csv"
    mockResponse <- list(headers = list("content-type" = "application/octet-stream"), url = url)
    class(mockResponse) <- "response"

    fileInfos <- mockedGetFile(connection, url, mockResponse, events_fileName)

    expected_path <- "tmp_data.csv"
    expect_identical(fileInfos$file_path, file.path(fileInfos$tmp_dir, expected_path), "wrong file found")

    expect_true(file.exists(fileInfos$file_path), info = "file should exists")
    unlink(file.path(fileInfos$tmp_dir, "*"), recursive = TRUE)
    expect_false(file.exists(fileInfos$file_path), info = "file should have been deleted")
})


context("postHttr()")

body <- '{"flowName":"flowName","sampleName":"TestSensor","fileName":"../data/testFile.csv"}'
class(body) <- "json"

mockedPostHttr <- function(connection, url, mockResponse, body, config) {
    RETRY_Stub <- mock(mockResponse)
    stopOnHttpError_Stub <- mock()

    response <- mockr::with_mock(RETRY = RETRY_Stub,
                                 stopOnHttpError = stopOnHttpError_Stub, {
                                     postHttr(connection, url, body, "Test API")
                                 })

    expect_args(RETRY_Stub, 1, "POST", url, body, config, jsonHeaders(), "json")
    expect_args(stopOnHttpError_Stub, 1, mockResponse, "Test API")
    return(response)
}

test_that("should work correctly in case of remote/local connection", {
    # Local connection
    mockResponse$status_code <- 200
    class(mockResponse) <- "response"
    response <- mockedPostHttr(connection, "url", mockResponse, body, config)
    expect_identical(response, mockResponse, "response should not have been modified")

    # Remote connection
    mockResponse$status_code <- 204
    class(mockResponse) <- "response"
    connection$localIM <- FALSE
    response <- mockedPostHttr(connection, "url", mockResponse, body, config)
    expect_identical(response, mockResponse, "response should not have been modified")
})

context("postJSON()")

mockedPostJSON <- function(connection, url, body, mockResponse = NULL) {
    postHttr_Stub <- mock(mockResponse)

    fileInfos <- mockr::with_mock(postHttr = postHttr_Stub, {
                                     postJSON(connection, url, body, "Test Upload API")
                                 })

    expect_args(postHttr_Stub, 1, connection, url, body, "Test Upload API")
    return(fileInfos)
}

test_that("should upload a JSON file if a good request has been sent with local connection", {
    # prepare mock results
    mockData <- '{"filePath":"C:\\\\Users\\\\exampleUrls"}'
    mockResponse <- list(status_code = 200, content = charToRaw(mockData))
    class(mockResponse) <- "response"

    # in case of good request - send back the url
    expectedReturn <- list(filePath = "C:\\Users\\exampleUrls")
    fileInfos <- mockedPostJSON(connection, "url", body, mockResponse)

    expect_identical(fileInfos, expectedReturn, info = "wrong object returned")
})


test_that("should get back correct information if asking for credential using remote connection", {
    # prepare mock results
    expectedReturn <- fromJSON("../data/uploadCredentialCloud.json")
    mockData <- as.character(toJSON(expectedReturn))
    mockResponse <- list(status_code = 200, content = charToRaw(mockData))
    class(mockResponse) <- "response"

    # in case of good request - send back the whole table
    fileInfos <- mockedPostJSON(connection, "url", body, mockResponse)
    expect_identical(fileInfos, expectedReturn, info = "wrong object returned")
})


context("putHttr()")

mockedPutHttr <- function(connection, url, mockResponse, fileName = NULL, config = list(), expectedBody = NULL) {
    RETRY_Stub <- mock(mockResponse)
    stopOnHttpError_Stub <- mock()

    mockr::with_mock(RETRY = RETRY_Stub,
                     stopOnHttpError = stopOnHttpError_Stub, {
                        putHttr(connection, url, fileName, "Test Put API")
                     })

    expect_args(RETRY_Stub, 1, "PUT", url, expectedBody, config)
    expect_args(stopOnHttpError_Stub, 1, mockResponse, "Test Put API")
}

test_that("should work correctly in case of remote connection", {
    # With a file pushed
    mockResponse$status_code <- 200
    class(mockResponse) <- "response"
    config <- csvHeaders()
    fileName <- "../data/testFile.csv"
    expectedBody <- httr::upload_file(fileName)
    mockedPutHttr(connection, "url", mockResponse, fileName, config, expectedBody)

    #  With no file pushed
    mockResponse$status_code <- 204
    class(mockResponse) <- "response"
    config <- tokenHeaders(connection$token)
    mockedPutHttr(connection, "url", mockResponse, config = config)
})
