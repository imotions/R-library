context("uploadSensorData()")

library("imotionsApi")
library("mockery")
library("arrow")

# Params
params <- list(token = "token", iMotionsVersion = "iMotionsVersion", flowName = "flowName",
               studyId = "studyId", respondentId = "respondentId", segmentId = "segmentId",
               stimulusId = "stimulusId", extraParam = "fixationFilter")

# Load study, respondent and stimulus
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))
respondent <- getRespondent(study, "09bd22e6-29b6-4a8a-8cc1-4780a5163e63")
stimulus <- getStimulus(study, "1000")

# Create data to upload
data <- data.frame("Timestamp" = seq(1:100), "Thresholded value" = rep(0, 100), check.names = FALSE)

sensorName <- "TestSensor"
scriptName <- "TestScript"

test_that("should throw errors if arguments are missing or not from the good class", {
    # in case of missing params
    error <- capture_error(uploadSensorData())
    expect_identical(error$message, "Please specify parameters used for your script",
                     "missing `params` param not handled properly")

    # in case of missing study
    error <- capture_error(uploadSensorData(params))
    expect_identical(error$message, "Please specify a study loaded with `imStudy()`",
                     "missing `study` param not handled properly")

    # in case of missing data
    error <- capture_error(uploadSensorData(params, study))
    expect_identical(error$message, "Please specify a data.table with signals to upload",
                     "missing `data` param not handled properly")

    # in case of missing target
    error <- capture_error(uploadSensorData(params, study, data))
    expect_identical(error$message, "Please specify a target respondent loaded with `getRespondents()`",
                     "missing `target` param not handled properly")

    # in case of missing sensorName
    error <- capture_error(uploadSensorData(params, study, data, respondent))
    expect_identical(error$message, "Please specify a name for the new sensor to upload",
                     "missing `sensorName` param not handled properly")

    # in case of missing scriptName
    error <- capture_error(uploadSensorData(params, study, data, respondent, sensorName = sensorName))
    expect_identical(error$message, "Please specify the name of the script used to produce this data",
                     "missing `scriptName` param not handled properly")

    # in case of study that is not an imStudy object
    error <- capture_error(uploadSensorData(params, study = "whatever", data, respondent, sensorName, scriptName))

    expect_identical(error$message, "`study` argument is not an imStudy object",
                     "study not being an imStudy object should throw an error")

    # in case of target that is not an imRespondent object
    error <- capture_error(uploadSensorData(params, study, data, target = "whatever", sensorName, scriptName))
    expect_identical(error$message, "`target` argument is not an imRespondent object",
                     "target not being an imRespondent object should throw an error")

    # in case of stimulus that is not an imStimulus object
    error <- capture_error(uploadSensorData(params, study, data, respondent, sensorName, scriptName,
                                            stimulus = "whatever"))

    expect_identical(error$message, "`stimulus` argument is not an imStimulus object",
                     "stimulus not being an imStimulus object should throw an error")

    # in case of params required field not provided
    wrongParams <- list("FirstParam" = "blah")
    error <- capture_error(uploadSensorData(wrongParams, study, data, respondent, sensorName, scriptName))
    expect_identical(error$message, "Required `iMotionsVersion` field in params",
                     "missing `iMotionsVersion` field in params not handled properly")

    wrongParams <- list("iMotionsVersion" = "blah")
    error <- capture_error(uploadSensorData(wrongParams, study, data, respondent, sensorName, scriptName))
    expect_identical(error$message, "Required `flowName` field in params",
                     "missing `flowName` field in params not handled properly")

    # in case of wrong data format
    wrongData <- data.table(Timestamp = integer(), variableTest = numeric())
    error <- capture_error(uploadSensorData(params, study, wrongData, respondent, sensorName, scriptName))
    expect_identical(error$message, "Do not upload an empty dataset", "zero row dataset should not be uploaded")

    wrongData <- data[, 1, drop = FALSE]
    error <- capture_error(uploadSensorData(params, study, wrongData, respondent, sensorName, scriptName))
    expect_identical(error$message, "Dataset must contain at least two columns (Timestamp included)",
                     "Can't upload without data columns")

    wrongData <- data[, 2, drop = FALSE]
    error <- capture_error(uploadSensorData(params, study, wrongData, respondent, sensorName, scriptName))
    expect_identical(error$message, "Wrong data format for upload (must be imSignals, imMetrics or imEvents)",
                     "Timestamp column should be present")
})


test_that("should call privateUpload with the good parameters", {
    data <- checkDataFormat(data)
    additionalMetadata <- data.frame("Group" = c("", "Thresholded"), "Units" = c("ms", "binary"))
    privateUpload_Stub <- mock()

    mockr::with_mock(
        privateUpload = privateUpload_Stub, {
            uploadSensorData(params, study, data, respondent, sensorName, scriptName, metadata = additionalMetadata)
        })

    expect_called(privateUpload_Stub, 1)
    expect_args(privateUpload_Stub, 1, params = params, study = study, data = data, target = respondent,
                sampleName = sensorName, scriptName = scriptName, metadata = additionalMetadata,
                stimulus = NULL)

})


test_that("should not call privateUpload if data is of wrong format", {
    wrongData <- data.frame("NotTimestamp" = seq(1:100), "Thresholded value" = rep(0, 100))
    privateUpload_Stub <- mock()

    error <- capture_error(mockr::with_mock(
        privateUpload = privateUpload_Stub, {
            uploadSensorData(params, study, wrongData, respondent, sensorName = sensorName, scriptName = scriptName)
        }))

    expect_called(privateUpload_Stub, 0)
    expect_identical(error$message, "Wrong data format for upload (must be imSignals, imMetrics or imEvents)",
                     "Timestamp column should be present")

    # in case data is actually an event table
    wrongData <- data.frame("Timestamp" = seq(1:100), "EventName" = "Event 1", "Description" = "Description 1")
    warning <- capture_warning(mockr::with_mock(
        privateUpload = privateUpload_Stub, {
            uploadSensorData(params, study, wrongData, respondent, sensorName = sensorName, scriptName = scriptName)
        }))

    expect_called(privateUpload_Stub, 0)
    expect_identical(warning$message, "Data to upload should be a data.frame/data.table containing a Timestamp column",
                     "wrong data type detected")
})



context("uploadEvents()");

# Create data to upload
dataEvents <- data.table("Timestamp" = seq(1:100), "EventName" = rep("Event 1", 100), "Description" = rep("test", 100))

test_that("should throw errors if arguments are missing or not from the good class", {
    # in case of missing params
    error <- capture_error(uploadEvents())
    expect_identical(error$message, "Please specify parameters used for your script",
                     "missing `params` param not handled properly")

    # in case of missing study
    error <- capture_error(uploadEvents(params))
    expect_identical(error$message, "Please specify a study loaded with `imStudy()`",
                     "missing `study` param not handled properly")

    # in case of missing data
    error <- capture_error(uploadEvents(params, study))
    expect_identical(error$message, "Please specify a data.table with events to upload",
                     "missing `data` param not handled properly")

    # in case of missing target
    error <- capture_error(uploadEvents(params, study, dataEvents))
    expect_identical(error$message, "Please specify a target respondent loaded with `getRespondents()`",
                     "missing `target` param not handled properly")

    # in case of missing eventsName
    error <- capture_error(uploadEvents(params, study, data, respondent))
    expect_identical(error$message, "Please specify a name for the new events to upload",
                     "missing `eventsName` param not handled properly")

    # in case of missing scriptName
    error <- capture_error(uploadEvents(params, study, data, respondent, eventsName = sensorName))
    expect_identical(error$message, "Please specify the name of the script used to produce this data",
                     "missing `scriptName` param not handled properly")

    # in case of study that is not an imStudy object
    error <- capture_error(uploadEvents(params, study = "whatever", dataEvents, respondent, eventsName = sensorName,
                                        scriptName = scriptName))

    expect_identical(error$message, "`study` argument is not an imStudy object",
                     "study not being an imStudy object should throw an error")

    # in case of target that is not an imRespondent object
    error <- capture_error(uploadEvents(params, study, dataEvents, target = "whatever", eventsName = sensorName,
                                        scriptName = scriptName))

    expect_identical(error$message, "`target` argument is not an imRespondent object",
                     "target not being an imRespondent object should throw an error")

    # in case of params required field not provided
    wrongParams <- list("FirstParam" = "blah")
    error <- capture_error(uploadEvents(wrongParams, study, dataEvents, respondent, eventsName = sensorName,
                                        scriptName = scriptName))

    expect_identical(error$message, "Required `iMotionsVersion` field in params",
                     "missing `iMotionsVersion` field in params not handled properly")

    wrongParams <- list("iMotionsVersion" = "blah")
    error <- capture_error(uploadEvents(wrongParams, study, dataEvents, respondent, eventsName = sensorName,
                                        scriptName = scriptName))

    expect_identical(error$message, "Required `flowName` field in params",
                     "missing `flowName` field in params not handled properly")

    # in case of wrong data format
    wrongData <- data.table(Timestamp = integer(), variableTest = numeric())
    error <- capture_error(uploadEvents(params, study, wrongData, respondent, eventsName = sensorName,
                                        scriptName = scriptName))

    expect_identical(error$message, "Do not upload an empty dataset", "zero row dataset should not be uploaded")

    wrongData <- data[, 1, drop = FALSE]
    error <- capture_error(uploadEvents(params, study, wrongData, respondent, eventsName = sensorName,
                                        scriptName = scriptName))

    expect_identical(error$message, "Dataset must contain at least two columns (Timestamp included)",
                     "Can't upload without data columns")

    wrongData <- data[, 2, drop = FALSE]
    error <- capture_error(uploadEvents(params, study, wrongData, respondent, eventsName = sensorName,
                                        scriptName = scriptName))

    expect_identical(error$message, "Wrong data format for upload (must be imSignals, imMetrics or imEvents)",
                     "Timestamp column should be present")
})


test_that("should call privateUpload with the good parameters", {
    dataEvents <- checkDataFormat(dataEvents)
    additionalMetadata <- data.table("Units" = c("ms", "", ""), "Show" = c("FALSE", "TRUE", "TRUE"))
    privateUpload_Stub <- mock()

    mockr::with_mock(
        privateUpload = privateUpload_Stub, {
            uploadEvents(params, study, dataEvents, respondent, eventsName = sensorName,
                         scriptName = scriptName, metadata = additionalMetadata)
        })

    expect_called(privateUpload_Stub, 1)
    expect_args(privateUpload_Stub, 1, params = params, study = study, data = dataEvents, target = respondent,
                eventsName = sensorName, scriptName = scriptName, metadata = additionalMetadata)
})


test_that("should not call privateUpload if data is of wrong format", {
    wrongData <- data.frame("NotTimestamp" = seq(1:100), "Thresholded value" = rep(0, 100))
    privateUpload_Stub <- mock()

    error <- capture_error(mockr::with_mock(
        privateUpload = privateUpload_Stub, {
            uploadEvents(params, study, wrongData, respondent, eventsName = sensorName, scriptName = scriptName)
        }))

    expect_called(privateUpload_Stub, 0)
    expect_identical(error$message, "Wrong data format for upload (must be imSignals, imMetrics or imEvents)",
                     "Timestamp column should be present")

    # in case data is actually a signal table
    wrongData <- data.frame("Timestamp" = seq(1:100), "Thresholded value" = rep(0, 100))

    warning <- capture_warning(mockr::with_mock(
        privateUpload = privateUpload_Stub, {
            uploadEvents(params, study, wrongData, respondent, eventsName = sensorName, scriptName = scriptName)
        }))

    expect_called(privateUpload_Stub, 0)
    expect_identical(warning$message,
                     "Events should be a data.frame/data.table containing EventName, Timestamp and Description columns",
                     "wrong data type detected")
})



context("uploadMetrics()");

# Create data to upload
dataMetrics <- data.table("StimulusId" = c("1000", "1001", "1002"), "Timestamp" = c(1, 2, 3), "Metrics1" = c(1, 2, 3),
                          "Metrics2" = c(23, 45, 46))

test_that("should throw errors if arguments are missing or not from the good class", {
    # in case of missing params
    error <- capture_error(uploadMetrics())
    expect_identical(error$message, "Please specify parameters used for your script",
                     "missing `params` param not handled properly")

    # in case of missing study
    error <- capture_error(uploadMetrics(params))
    expect_identical(error$message, "Please specify a study loaded with `imStudy()`",
                     "missing `study` param not handled properly")

    # in case of missing data
    error <- capture_error(uploadMetrics(params, study))
    expect_identical(error$message, "Please specify a data.table with metrics to upload",
                     "missing `data` param not handled properly")

    # in case of missing target
    error <- capture_error(uploadMetrics(params, study, dataMetrics))
    expect_identical(error$message, "Please specify a target respondent loaded with `getRespondents()`",
                     "missing `target` param not handled properly")


    # in case of missing metricsName
    error <- capture_error(uploadMetrics(params, study, data, respondent))
    expect_identical(error$message, "Please specify a name for the new metrics to upload",
                     "missing `metricsName` param not handled properly")

    # in case of missing scriptName
    error <- capture_error(uploadMetrics(params, study, data, respondent, metricsName = sensorName))
    expect_identical(error$message, "Please specify the name of the script used to produce this data",
                     "missing `scriptName` param not handled properly")


    # in case of study that is not an imStudy object
    error <- capture_error(uploadMetrics(params, study = "whatever", dataMetrics, respondent, metricsName = sensorName,
                                         scriptName = scriptName))

    expect_identical(error$message, "`study` argument is not an imStudy object",
                     "study not being an imStudy object should throw an error")

    # in case of target that is not an imRespondent object
    error <- capture_error(uploadMetrics(params, study, dataMetrics, target = "whatever", metricsName = sensorName,
                                         scriptName = scriptName))

    expect_identical(error$message, "`target` argument is not an imRespondent object",
                     "target not being an imRespondent object should throw an error")

    # in case of params required field not provided
    wrongParams <- list("FirstParam" = "blah")
    error <- capture_error(uploadMetrics(wrongParams, study, dataMetrics, respondent, metricsName = sensorName,
                                         scriptName = scriptName))

    expect_identical(error$message, "Required `iMotionsVersion` field in params",
                     "missing `iMotionsVersion` field in params not handled properly")

    wrongParams <- list("iMotionsVersion" = "blah")
    error <- capture_error(uploadMetrics(wrongParams, study, dataMetrics, respondent, metricsName = sensorName,
                                         scriptName = scriptName))

    expect_identical(error$message, "Required `flowName` field in params",
                     "missing `flowName` field in params not handled properly")

    # in case of wrong data format
    wrongData <- data.table(Timestamp = integer(), variableTest = numeric())
    error <- capture_error(uploadMetrics(params, study, wrongData, respondent, metricsName = sensorName,
                                         scriptName = scriptName))

    expect_identical(error$message, "Do not upload an empty dataset", "zero row dataset should not be uploaded")

    wrongData <- data[, 1, drop = FALSE]
    error <- capture_error(uploadMetrics(params, study, wrongData, respondent, metricsName = sensorName,
                                         scriptName = scriptName))

    expect_identical(error$message, "Dataset must contain at least two columns (Timestamp included)",
                     "Can't upload without data columns")

    wrongData <- data[, 2, drop = FALSE]
    error <- capture_error(uploadMetrics(params, study, wrongData, respondent, metricsName = sensorName,
                                         scriptName = scriptName))

    expect_identical(error$message, "Wrong data format for upload (must be imSignals, imMetrics or imEvents)",
                     "Timestamp column should be present")
})


test_that("should call privateUpload with the good parameters", {
    dataMetrics <- checkDataFormat(dataMetrics)
    additionalMetadata <- data.table("Units" = c("", "ms", "", ""), "Show" = c("FALSE", "FALSE", "TRUE", "TRUE"))
    privateUpload_Stub <- mock()

    mockr::with_mock(
        privateUpload = privateUpload_Stub, {
            uploadMetrics(params, study, dataMetrics, respondent, metricsName = sensorName,
                          scriptName = scriptName, metadata = additionalMetadata)
        })

    expect_called(privateUpload_Stub, 1)
    expect_args(privateUpload_Stub, 1, params = params, study = study, data = dataMetrics, target = respondent,
                sampleName = sensorName, scriptName = scriptName, metadata = additionalMetadata)
})


test_that("should not call privateUpload if data is of wrong format", {
    wrongData <- data.frame("NotTimestamp" = seq(1:100), "Thresholded value" = rep(0, 100))
    privateUpload_Stub <- mock()

    error <- capture_error(mockr::with_mock(
        privateUpload = privateUpload_Stub, {
            uploadMetrics(params, study, wrongData, respondent, metricsName = sensorName, scriptName = scriptName)
        }))

    expect_called(privateUpload_Stub, 0)
    expect_identical(error$message, "Wrong data format for upload (must be imSignals, imMetrics or imEvents)",
                     "Timestamp column should be present")

    # in case data is actually a signal table
    wrongData <- data.frame("Timestamp" = seq(1:100), "Thresholded value" = rep(0, 100))

    warning <- capture_warning(mockr::with_mock(
        privateUpload = privateUpload_Stub, {
            uploadMetrics(params, study, wrongData, respondent, metricsName = sensorName, scriptName = scriptName)
        }))

    expect_called(privateUpload_Stub, 0)
    expect_identical(warning$message,
                     paste("Metrics should be a data.frame/data.table containing a StimulusId column, a Timestamp",
                           "column and at least one other column containing metrics"), "wrong data type detected")
})



context("privateUpload()");

uploadUrlStudyPath <- "uploadUrlStudy"
uploadUrlStimulusPath <- "uploadUrlStimulus"
uploadUrlEventPath <- "uploadUrlEvent"
uploadUrlMetricsPath <- "uploadUrlMetrics"

mockedPrivateUpload <- function(params, study, data, respondent, expectedBody, expectedEndpoint, sensorName = NULL,
                                scriptName = NULL, metadata = NULL, stimulus = NULL) {

    class(expectedBody) <- "json"

    privateSaveToFile_Stub <- mock("../data/testFile.csv")

    # Replace url to load test data
    mockUrl <- function(url) {
         if (grepl("stimuli", url)) {
            return(uploadUrlStimulusPath)
        } else {
            return(uploadUrlStudyPath)
        }
    }

    if (inherits(data, "imSignals")) {
        expectedUrl <- mockUrl(getUploadSensorsUrl(study = study, imObject = respondent, stimulus = stimulus))
    } else if (inherits(data, "imEvents")) {
        expectedUrl <- uploadUrlEventPath
    } else if (inherits(data, "imMetrics")) {
        expectedUrl <- uploadUrlMetricsPath
    }

    getUploadSensorsUrl_Stub <- mock(expectedUrl)
    getUploadEventsUrl_Stub <- mock(expectedUrl)
    getUploadMetricsUrl_Stub <- mock(expectedUrl)
    postJSON_Stub <- mock(list(filePath = expectedUrl))

    res <- mockr::with_mock(privateSaveToFile = privateSaveToFile_Stub,
                            getUploadSensorsUrl = getUploadSensorsUrl_Stub,
                            getUploadEventsUrl = getUploadEventsUrl_Stub,
                            getUploadMetricsUrl = getUploadMetricsUrl_Stub,
                            postJSON = postJSON_Stub, {
                                privateUpload(params, study, data, respondent, sensorName, scriptName, metadata,
                                              stimulus)
                            })

    if (inherits(data, "imSignals")) {
        expect_args(getUploadSensorsUrl_Stub, 1, study = study, imObject = respondent, stimulus = stimulus)
    } else if (inherits(data, "imEvents")) {
        expect_args(getUploadEventsUrl_Stub, 1, study = study, imObject = respondent)
    } else if (inherits(data, "imMetrics")) {
        expect_args(getUploadMetricsUrl_Stub, 1, study = study, imObject = respondent)
    }

    expect_args(privateSaveToFile_Stub, 1, params = params, data = data, sampleName = sensorName,
                scriptName = scriptName, metadata = metadata)

    expect_args(postJSON_Stub, 1, connection = study$connection, expectedUrl, postData = expectedBody,
                message = expectedEndpoint)

    return(res)
}


test_that("should upload signals to a given respondent/segment if a good request has been sent", {
    data <- checkDataFormat(data)
    expectedBody <- '{"flowName":"flowName","sampleName":"TestSensor","fileName":"../data/testFile.csv"}'
    expectedEndpoint <- "Uploading sensor data for target: Wendy"

    res <- mockedPrivateUpload(params, study, data, respondent, expectedBody, expectedEndpoint, sensorName, scriptName)
    expect_identical(res$filePath, uploadUrlStudyPath, info = "wrong path returned")

    # Also when a stimulus is provided
    expectedEndpoint <- "Uploading sensor data for target: Wendy, stimulus: AntiSmoking40Sec"
    res <- mockedPrivateUpload(params, study, data, respondent, expectedBody, expectedEndpoint, sensorName, scriptName,
                               stimulus = stimulus)

    expect_identical(res$filePath, uploadUrlStimulusPath, info = "wrong path returned")
})


test_that("should upload events to a given respondent/segment if a good request has been sent", {
    dataEvents <- checkDataFormat(dataEvents)
    expectedBody <- '{"flowName":"flowName","sampleName":"ET_REventApi","fileName":"../data/testFile.csv"}'
    expectedEndpoint <- "Uploading events for target: Wendy"

    res <- mockedPrivateUpload(params, study, dataEvents, respondent, expectedBody, expectedEndpoint)
    expect_identical(res$filePath, uploadUrlEventPath, info = "wrong path returned")
})


test_that("should upload metrics to a given respondent/segment if a good request has been sent", {
    dataMetrics <- checkDataFormat(dataMetrics)
    expectedBody <- '{"flowName":"flowName","sampleName":"ET_RMetricsApi","fileName":"../data/testFile.csv"}'
    expectedEndpoint <- "Uploading metrics for target: Wendy"

    res <- mockedPrivateUpload(params, study, dataMetrics, respondent, expectedBody, expectedEndpoint)
    expect_identical(res$filePath, uploadUrlMetricsPath, info = "wrong path returned")
})


context("privateSaveToFile()");

test_that("Data should get stored as a temporary file", {
    tmpDir <- tempdir(check = TRUE)
    data <- checkDataFormat(data)
    additionalMetadata <- data.frame("Group" = c("", "Thresholded"), "Units" = c("ms", "binary"))
    dataFileName <- suppressWarnings(privateSaveToFile(params, data, sensorName, scriptName, additionalMetadata))

    # Check that file exists at the good location
    expect_true(file.exists(dataFileName), "temporary data file should have been created")
    expect_identical(dataFileName, file.path(tmpDir, "result.csv"), "wrong file path")
    dataWritten <- fread(dataFileName)
    testData <- fread("../data/testFile.csv")
    expect_identical(dataWritten, testData, "files should be identical")

    # Re-calling the function should overwrite the file (not append it below)
    dataFileName2 <- suppressWarnings(privateSaveToFile(params, data, sensorName, scriptName, additionalMetadata))

    expect_identical(dataFileName, dataFileName2, "same file should be used")
    dataWritten <- fread(dataFileName)
    expect_identical(dataWritten, testData, "files should still be identical")

    # params with a "scratchFolder" path provided should use this path directly without warnings
    params <- append(params, list("scratchFolder" = tmpDir))
    dataFileName <- privateSaveToFile(params, data, sensorName, scriptName, additionalMetadata)
    dataWritten <- fread(dataFileName)
    expect_identical(dataWritten, testData, "files should still be identical")

    # NA in metrics should be output as "NA"
    dataMetricsNA <- data.table("StimulusId" = c("1000", "1001", "1002"), "Timestamp" = c(1, 2, 3),
                                "Metrics1" = c(1, NA_real_, 3), "Metrics2" = c(23, 45, NA_real_))

    dataMetricsNA <- checkDataFormat(dataMetricsNA)
    expectedFile <- fread("../data/metricsFile.csv")
    dataFileName <- privateSaveToFile(params, dataMetricsNA, sensorName, scriptName)
    dataWritten <- fread(dataFileName)
    expect_identical(dataWritten, expectedFile, "files should still be identical")

    # Cleaning file created for testing
    file.remove(dataFileName)
})

context("privateGetFileHeader()");

test_that("General file header should be as expected", {
    # Signal data
    data <- checkDataFormat(data)
    headers <- privateGetFileHeader(data, params, sensorName, scriptName)

    expect_equal(length(headers), 11, info = "should be composed of 11 lines")
    expect_identical(headers[1], paste0("\ufeff", params$iMotionsVersion, ",,"), "BOM should be added")
    expect_identical(headers[8], "#METADATA,,", "wrong number of comma added")
    expect_identical(headers[9], "FieldName,Timestamp,Thresholded value", "wrong FieldName")
    expect_identical(headers[10], "DataType,Double,Double", "wrong DataType")
    expect_identical(headers[11], "#DATA,,", "data line should be added")

    # Event data
    dataEvents <- checkDataFormat(dataEvents)
    headers <- privateGetFileHeader(dataEvents, params, sensorName, scriptName)

    expect_equal(length(headers), 11, info = "should be composed of 11 lines")
    expect_identical(headers[1], paste0("\ufeff", params$iMotionsVersion, ",,,"), "BOM should be added")
    expect_identical(headers[8], "#METADATA,,,", "wrong number of comma added")
    expect_identical(headers[9], "FieldName,Timestamp,EventName,Description", "wrong FieldName")
    expect_identical(headers[10], "DataType,Double,Character,Character", "wrong DataType")
    expect_identical(headers[11], "#DATA,,,", "data line should be added")

    # Metrics data
    dataMetrics <- checkDataFormat(dataMetrics)
    headers <- privateGetFileHeader(dataMetrics, params, sensorName, scriptName)

    expect_equal(length(headers), 12, info = "should be composed of 11 lines")
    expect_identical(headers[1], paste0("\ufeff", params$iMotionsVersion, ",,,,"), "BOM should be added")
    expect_identical(headers[8], "#METADATA,,,,", "wrong number of comma added")
    expect_identical(headers[9], "FieldName,StimulusId,Timestamp,Metrics1,Metrics2", "wrong FieldName")
    expect_identical(headers[10], "DataType,Character,Double,Double,Double", "wrong DataType")
    expect_identical(headers[11], "ImotionsInternal,NoGraphDisplay,,Metrics,Metrics", "wrong internal informations")
    expect_identical(headers[12], "#DATA,,,,", "data line should be added")

    # Export data no metadata
    dataExport <- data.table("Respondent Name" = "Respondent 1", "Metrics1" = seq(1:100),
                             "Thresholded value" = rep(0, 100), check.names = FALSE)

    dataExport <- checkDataFormat(dataExport)
    headers <- privateGetFileHeader(dataExport)

    expect_equal(length(headers), 2, info = "should be composed of 2 lines")
    expect_identical(headers[1], "\ufeff#METADATA,,,", "BOM should be added")
    expect_identical(headers[2], "#DATA,,,", "wrong number of comma added")

    # Export data with metadata
    additionalMetadata <- data.table("Group" = c("", "numeric", "Thresholded"), "Units" = c("", "ms", "binary"))

    headers <- privateGetFileHeader(dataExport, metadata = additionalMetadata)

    expect_equal(length(headers), 4, info = "should be composed of 2 lines")
    expect_identical(headers[1], "\ufeff#METADATA,,,", "BOM should be added")
    expect_identical(headers[2], "#Group,,numeric,Thresholded", "should start correctly")
    expect_identical(headers[3], "#Units,,ms,binary", "should start correctly")
    expect_identical(headers[4], "#DATA,,,", "wrong number of comma added")
})

context("privateCreateHeader()");

test_that("Data header should be as expected for signals", {
    data <- checkDataFormat(data)
    dataHeader <- privateCreateHeader(params, data, sensorName, scriptName)

    # Most of the params should have been removed from metadata
    expectedMetadataUrl <- paste0("%7B%22sampleName%22%3A%22TestSensor%22%2C%22script%22%3A%22TestScript%22%2C%22",
                                  "fileDependency%22%3A%7B%7D%2C%22parameters%22%3A%7B%22extraParam%22%3A%22",
                                  "fixationFilter%22%7D%7D")

    expect_equal(length(dataHeader), 7, info = "should be composed of 7 lines")
    expect_identical(dataHeader[1], params$iMotionsVersion, "wrong imotions version")
    expect_identical(dataHeader[2], "#HEADER", "wrong #HEADER")
    expect_identical(dataHeader[3], "iMotions.RAPIData", "wrong iMotions.RAPIData")
    expect_identical(dataHeader[4], params$flowName, "wrong flowName")
    expect_identical(dataHeader[5], "ET_RExtAPI", "wrong ET_RExtAPI")
    expect_identical(dataHeader[6], "", "should be empty")
    expect_identical(dataHeader[7], expectedMetadataUrl, "wrong metadata")
})

test_that("Data header should be as expected for events", {
    dataEvents <- checkDataFormat(dataEvents)
    dataHeader <- privateCreateHeader(params, dataEvents, sampleName = NULL, scriptName = NULL)

    # Most of the params should have been removed from metadata
    expectedMetadataUrl <- paste0("%7B%22sampleName%22%3A%7B%7D%2C%22script%22%3A%7B%7D%2C%22parameters%22%3A%7B%22",
                                  "extraParam%22%3A%22fixationFilter%22%7D%7D")

    expect_equal(length(dataHeader), 7, info = "should be composed of 7 lines")
    expect_identical(dataHeader[1], params$iMotionsVersion, "wrong imotions version")
    expect_identical(dataHeader[2], "#HEADER", "wrong #HEADER")
    expect_identical(dataHeader[3], "iMotions.RAPIData", "wrong iMotions.RAPIData")
    expect_identical(dataHeader[4], params$flowName, "wrong flowName")
    expect_identical(dataHeader[5], "ET_REventAPI", "wrong ET_REventAPI")
    expect_identical(dataHeader[6], "", "should be empty")
    expect_identical(dataHeader[7], expectedMetadataUrl, "wrong metadata")
})

test_that("Data header should be as expected for metrics", {
    dataMetrics <- checkDataFormat(dataMetrics)
    dataHeader <- privateCreateHeader(params, dataMetrics, sampleName = NULL, scriptName = NULL)

    # Most of the params should have been removed from metadata
    expectedMetadataUrl <- paste0("%7B%22sampleName%22%3A%7B%7D%2C%22script%22%3A%7B%7D%2C%22parameters%22%3A%7B%22",
                                  "extraParam%22%3A%22fixationFilter%22%7D%7D")

    expect_equal(length(dataHeader), 7, info = "should be composed of 7 lines")
    expect_identical(dataHeader[1], params$iMotionsVersion, "wrong imotions version")
    expect_identical(dataHeader[2], "#HEADER", "wrong #HEADER")
    expect_identical(dataHeader[3], "iMotions.RAPIData", "wrong iMotions.RAPIData")
    expect_identical(dataHeader[4], params$flowName, "wrong flowName")
    expect_identical(dataHeader[5], "ET_RMetricsAPI", "wrong ET_RMetricsAPI")
    expect_identical(dataHeader[6], "", "should be empty")
    expect_identical(dataHeader[7], expectedMetadataUrl, "wrong metadata")
})

context("privateCreateMetadata()")

checkMandatoryMetadata <- function(metadata) {
    expect_identical(metadata[1], "#METADATA", "wrong #METADATA")
    expect_identical(metadata[2], "FieldName,Timestamp,Thresholded value", "wrong FieldName")
    expect_identical(metadata[3], "DataType,Double,Double", "wrong DataType") #DataType should not show Integer values
}

test_that("Mandatory metadata should be as expected", {
    data <- checkDataFormat(data)
    metadata <- privateCreateMetadata(data)

    expect_equal(length(metadata), 3, info = "should be composed of 3 lines")
    checkMandatoryMetadata(metadata)

})

test_that("Adding custom metadata should work as expected", {
    data <- checkDataFormat(data)
    additionalMetadata <- data.frame("Group" = c("", "Thresholded"), "Units" = c("ms", "binary"))
    metadata <- privateCreateMetadata(data, additionalMetadata)

    expect_equal(length(metadata), 5, info = "should be composed of 5 lines (mandatory + additional)")
    checkMandatoryMetadata(metadata)
    expect_identical(metadata[4], "Group,,Thresholded", "wrong additional metadata")
    expect_identical(metadata[5], "Units,ms,binary", "wrong additional metadata")

    # More or less metadata rows than data columns should throw a warning and not append metadata
    additionalMetadata <- data.frame("Group" = "", "Units" = "ms")
    metadata <- suppressWarnings(privateCreateMetadata(data, additionalMetadata))
    expect_equal(length(metadata), 3, info = "should be composed of 3 lines (mandatory only)")
    warning <- capture_warning(privateCreateMetadata(data, additionalMetadata))
    expect_identical(warning$message, "Wrong additional metadata format - ignoring it...", "should throw a warning")

})
