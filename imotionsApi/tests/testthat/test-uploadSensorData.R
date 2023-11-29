# privateCreateMetadata ===============================================================================================
context("privateCreateMetadata()")

library(mockery)

# Create data to upload
data <- data.frame("Timestamp" = seq(1:100), "Thresholded value" = rep(0, 100), check.names = FALSE)

checkMandatoryMetadata <- function(metadata) {
    testthat::expect_identical(metadata[1], "#METADATA", "wrong #METADATA")
    testthat::expect_identical(metadata[2], "FieldName,Timestamp,Thresholded value", "wrong FieldName")
    testthat::expect_identical(metadata[3], "DataType,Double,Double", "wrong DataType")
}

test_that("return - mandatory metadata", {
    data <- checkDataFormat(data)
    metadata <- privateCreateMetadata(data)

    expect_equal(length(metadata), 3, info = "should be composed of 3 lines")
    checkMandatoryMetadata(metadata)

})

test_that("return - custom metadata", {
    data <- checkDataFormat(data)
    additionalMetadata <- data.frame("Group" = c("", "Thresholded"), "Units" = c("ms", "binary"))
    metadata <- privateCreateMetadata(data, additionalMetadata)

    expect_equal(length(metadata), 5, info = "should be composed of 5 lines (mandatory + additional)")
    checkMandatoryMetadata(metadata)
    expect_identical(metadata[4], "Group,,Thresholded", "wrong additional metadata")
    expect_identical(metadata[5], "Units,ms,binary", "wrong additional metadata")

    # More or less metadata rows than data columns should throw a warning and not append metadata
    additionalMetadata <- data.frame("Group" = "", "Units" = "ms")

    expect_warning(metadata <- privateCreateMetadata(data, additionalMetadata),
                   "Wrong additional metadata format - ignoring it...",
                   info = "should throw a warning")

    expect_equal(length(metadata), 3, info = "should be composed of 3 lines (mandatory only)")
})

# privateCreateHeader =================================================================================================
context("privateCreateHeader()")

# Params
params <- list(token = "token", iMotionsVersion = "iMotionsVersion", flowName = "flowName",
               studyId = "studyId", respondentId = "respondentId", segmentId = "segmentId",
               stimulusId = "stimulusId", extraParam = "fixationFilter")

sampleName <- "TestSensor"
scriptName <- "TestScript"

test_that("return - headers for signals", {
    data <- checkDataFormat(data)
    dataHeader <- privateCreateHeader(params, data, sampleName, scriptName)

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

dataEvents <- data.table("Timestamp" = seq(1:100), "EventName" = rep("Event 1", 100), "Description" = rep("test", 100))

test_that("return - headers for events", {
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

dataMetrics <- data.table("StimulusId" = c("1000", "1001", "1002"), "Timestamp" = c(1, 2, 3), "Metrics1" = c(1, 2, 3),
                          "Metrics2" = c(23, 45, 46))

test_that("return - headers for metrics", {
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

# privateGetFileHeader ================================================================================================
context("privateGetFileHeader()")

dataExport <- data.table("Respondent Name" = "Respondent 1", "Metrics1" = seq(1:100),
                         "Thresholded value" = rep(0, 100), check.names = FALSE)

test_that("return - correct file headers based on the data type", {
    # Signal data
    data <- checkDataFormat(data)
    headers <- privateGetFileHeader(data, params, sampleName, scriptName)

    expect_equal(length(headers), 11, info = "should be composed of 11 lines")
    expect_identical(headers[1], paste0("\ufeff", params$iMotionsVersion, ",,"), "BOM should be added")
    expect_identical(headers[8], "#METADATA,,", "wrong number of comma added")
    expect_identical(headers[9], "FieldName,Timestamp,Thresholded value", "wrong FieldName")
    expect_identical(headers[10], "DataType,Double,Double", "wrong DataType")
    expect_identical(headers[11], "#DATA,,", "data line should be added")

    # Event data
    dataEvents <- checkDataFormat(dataEvents)
    headers <- privateGetFileHeader(dataEvents, params, sampleName, scriptName)

    expect_equal(length(headers), 11, info = "should be composed of 11 lines")
    expect_identical(headers[1], paste0("\ufeff", params$iMotionsVersion, ",,,"), "BOM should be added")
    expect_identical(headers[8], "#METADATA,,,", "wrong number of comma added")
    expect_identical(headers[9], "FieldName,Timestamp,EventName,Description", "wrong FieldName")
    expect_identical(headers[10], "DataType,Double,Character,Character", "wrong DataType")
    expect_identical(headers[11], "#DATA,,,", "data line should be added")

    # Metrics data
    dataMetrics <- checkDataFormat(dataMetrics)
    headers <- privateGetFileHeader(dataMetrics, params, sampleName, scriptName)

    expect_equal(length(headers), 12, info = "should be composed of 11 lines")
    expect_identical(headers[1], paste0("\ufeff", params$iMotionsVersion, ",,,,"), "BOM should be added")
    expect_identical(headers[8], "#METADATA,,,,", "wrong number of comma added")
    expect_identical(headers[9], "FieldName,StimulusId,Timestamp,Metrics1,Metrics2", "wrong FieldName")
    expect_identical(headers[10], "DataType,Character,Double,Double,Double", "wrong DataType")
    expect_identical(headers[11], "ImotionsInternal,NoGraphDisplay,,Metrics,Metrics", "wrong internal informations")
    expect_identical(headers[12], "#DATA,,,,", "data line should be added")

    # Export data no metadata
    dataExport <- checkDataFormat(dataExport)
    headers <- privateGetFileHeader(dataExport, params, sampleName, scriptName)

    expect_equal(length(headers), 2, info = "should be composed of 2 lines")
    expect_identical(headers[1], "\ufeff#METADATA,,,", "BOM should be added")
    expect_identical(headers[2], "#DATA,,,", "wrong number of comma added")

    # Export data with metadata
    additionalMetadata <- data.table("Group" = c("", "numeric", "Thresholded"), "Units" = c("", "ms", "binary"))
    headers <- privateGetFileHeader(dataExport, params, sampleName, scriptName, metadata = additionalMetadata)

    expect_equal(length(headers), 4, info = "should be composed of 2 lines")
    expect_identical(headers[1], "\ufeff#METADATA,,,", "BOM should be added")
    expect_identical(headers[2], "#Group,,numeric,Thresholded", "should start correctly")
    expect_identical(headers[3], "#Units,,ms,binary", "should start correctly")
    expect_identical(headers[4], "#DATA,,,", "wrong number of comma added")
})

# privateSaveToFile ===================================================================================================
context("privateSaveToFile()")

# Load studies
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))
study_cloud <- jsonlite::unserializeJSON(readLines("../data/imStudy_cloud.json"))

test_that("check - data should get stored as a temporary file", {
    tmpDir <- tempdir(check = TRUE)
    data <- checkDataFormat(data)
    additionalMetadata <- data.frame("Group" = c("", "Thresholded"), "Units" = c("ms", "binary"))

    dataFileName <- privateSaveToFile(params, study, data, sampleName, scriptName, additionalMetadata)

    # Check that file exists at the good location
    expect_true(file.exists(dataFileName), "temporary data file should have been created")
    expect_identical(dataFileName, file.path(tmpDir, "result.csv"), "wrong file path")

    dataWritten <- fread(dataFileName)
    testData <- fread("../data/testFile.csv")

    expect_identical(dataWritten, testData, "files should be identical")

    # Re-calling the function should overwrite the file (not append it below)
    dataFileName2 <- privateSaveToFile(params, study, data, sampleName, scriptName, additionalMetadata)
    expect_identical(dataFileName, dataFileName2, "same file should be used")

    dataWritten <- fread(dataFileName)

    expect_identical(dataWritten, testData, "files should still be identical")

    # params with a "scratchFolder" path provided should use this path directly without warnings
    params <- append(params, list("scratchFolder" = tmpDir))
    dataFileName <- privateSaveToFile(params, study, data, sampleName, scriptName, additionalMetadata)
    dataWritten <- fread(dataFileName)

    expect_identical(dataWritten, testData, "files should still be identical")

    # NA in metrics should be output as "NA" and reordered
    dataMetricsNA <- data.table("StimulusId" = c("1000", "1001", "1002"), "Timestamp" = c(30, 20, 10),
                                "Metrics1" = c(1, NA_real_, 3), "Metrics2" = c(23, 45, NA_real_))

    dataMetricsNA <- checkDataFormat(dataMetricsNA)
    expectedFile <- fread("../data/metricsFile.csv")
    dataFileName <- privateSaveToFile(params, study, dataMetricsNA, sampleName, scriptName)
    dataWritten <- fread(dataFileName)
    expect_identical(dataWritten, expectedFile, "files should still be identical")

    # export should be saved directly by using the sampleName as path
    dataExport <- data.table("Respondent Name" = "Respondent 1", "Metrics1" = seq(1:100),
                             "Thresholded value" = rep(0, 100), check.names = FALSE)

    sampleName <- file.path(tmpDir, "export.csv")
    additionalMetadata <- data.table("Group" = c("", "numeric", "Thresholded"), "Units" = c("", "ms", "binary"))

    dataExport <- checkDataFormat(dataExport)
    expectedFile <- fread("../data/exportData.csv")
    dataFileName <- privateSaveToFile(params, study, dataExport, sampleName, scriptName)
    dataWritten <- fread(dataFileName)

    expect_identical(dataWritten, expectedFile, "files should still be identical")

    # Cleaning file created for testing
    unlink(file.path(tmpDir, "*"))
})

# privateCreatePostRequest ============================================================================================
context("privateCreatePostRequest()")

test_that("local/remote return - post request data", {
    # In case of local connection
    expectedPostData <- '{"flowName":"flowName","sampleName":"TestSensor","fileName":"../data/testFile.csv"}'
    class(expectedPostData) <- "json"
    postData <- privateCreatePostRequest(params, study, "TestSensor", "../data/testFile.csv")

    expect_identical(postData, expectedPostData, info = "wrong post body for local connection")

    # In case of remote connection
    expectedPostData <- '{"instance":"flowName","name":"TestSensor","fileName":"../data/testFile.csv"}'
    class(expectedPostData) <- "json"
    postData <- privateCreatePostRequest(params, study_cloud, "TestSensor", "../data/testFile.csv")

    expect_identical(postData, expectedPostData, info = "wrong post body for remote connection")
})

# privateUpload =======================================================================================================
context("privateUpload()")

# Load respondent, segment and stimulus
respondent <- getRespondent(study, "09bd22e6-29b6-4a8a-8cc1-4780a5163e63")
stimulus <- getStimulus(study, "1000")
segment <- getSegment(study, "1010")

uploadUrlRespondentPath <- "uploadUrlRespondent"
uploadUrlStimulusPath <- "uploadUrlStimulus"
uploadUrlSegmentPath <- "uploadUrlSegment"
uploadUrlEventPath <- "uploadUrlEvent"
uploadUrlMetricsPath <- "uploadUrlMetrics"

# Replace url to load test data
mockUrl <- function(url) {
    if (grepl("report", url)) {
        return(url)
    } else if (grepl("respondent", url)) {
        if (grepl("stimuli", url)) {
            return(uploadUrlStimulusPath)
        } else {
            return(uploadUrlRespondentPath)
        }
    } else {
        return(uploadUrlSegmentPath)
    }
}

mockedPrivateUpload <- function(params, study, data, target, expectedBody, expectedEndpoint, sampleName = NULL,
                                scriptName = NULL, metadata = NULL, stimulus = NULL) {

    class(expectedBody) <- "json"
    privateSaveToFile_Stub <- mock("../data/testFile.csv")

    if (inherits(data, "imSignals")) {
        expectedUrl <- mockUrl(getUploadSensorDataUrl(study = study, imObject = target, stimulus = stimulus))
    } else if (inherits(data, "imEvents")) {
        expectedUrl <- uploadUrlEventPath
    } else if (inherits(data, "imMetrics")) {
        expectedUrl <- uploadUrlMetricsPath
    } else if (inherits(data, "imExport")) {
        expectedUrl <- mockUrl(getUploadSensorDataUrl(study = study, imObject = target, stimulus = stimulus))
    }

    getUploadSensorDataUrl_Stub <- mock(expectedUrl)
    getUploadEventsUrl_Stub <- mock(expectedUrl)
    getUploadMetricsUrl_Stub <- mock(expectedUrl)

    if (study$connection$localIM) {
        expectedPostData <- list(filePath = expectedUrl)
    } else {
        expectedPostData <- fromJSON("../data/uploadCredential_cloud.json")
    }

    postJSON_Stub <- mock(expectedPostData)
    putHttr_Stub <- mock()

    res <- mockr::with_mock(privateSaveToFile = privateSaveToFile_Stub,
                            getUploadSensorDataUrl = getUploadSensorDataUrl_Stub,
                            getUploadEventsUrl = getUploadEventsUrl_Stub,
                            getUploadMetricsUrl = getUploadMetricsUrl_Stub,
                            postJSON = postJSON_Stub,
                            putHttr = putHttr_Stub, {
                                privateUpload(params, study, data, target, sampleName, scriptName, metadata,
                                              stimulus)
                            })

    if (!study$connection$localIM) {
        baseUrl <- "https://test/api"
        if (inherits(target, "imRespondent")) {
            expectedUrl <- paste0(baseUrl, "/reportruns/1234/respondents/09bd22e6-29b6-4a8a-8cc1-4780a5163e63")
            expectedUrlConfirm <- paste0(expectedUrl, "/samples/1e0c8d99-4aa1-4916-adf8-b6950db40d67")
            expectedEndpointUpload <- "Uploading sensor data for respondent: Wendy"
            expectedEndpointConfirm <- "Upload of sensor data for respondent: Wendy confirmed"
        } else {
            expectedUrl <- expectedUrlConfirm <- paste0(baseUrl, "/reportruns/1234/segments/1010")
            expectedEndpointUpload <- "Uploading export for segment: 2 GSR 81-1"
            expectedEndpointConfirm <- "Upload of export for segment: 2 GSR 81-1 confirmed"
        }
    }

    if (inherits(data, "imSignals")) {
        expect_args(getUploadSensorDataUrl_Stub, 1, study = study, imObject = target, stimulus = stimulus)
    } else if (inherits(data, "imEvents")) {
        expect_args(getUploadEventsUrl_Stub, 1, study = study, imObject = target)
    } else if (inherits(data, "imMetrics")) {
        expect_args(getUploadMetricsUrl_Stub, 1, study = study, imObject = target)
    } else if (inherits(data, "imExport")) {
        expect_args(getUploadSensorDataUrl_Stub, 1, study = study, imObject = target)
    }

    expect_args(privateSaveToFile_Stub, 1, params = params, study = study, data = data, sampleName = sampleName,
                scriptName = scriptName, metadata = metadata)

    expect_args(postJSON_Stub, 1, connection = study$connection, expectedUrl, postData = expectedBody,
                message = expectedEndpoint)

    if (!study$connection$localIM) {
        presignedUrl <- paste0("https://my-dev-imotions-blah.s3.us-east-1.amazonaws.com/",
                               "verylongqueryparamshere?Z-Amz-Credential=ADFFKAHDKFH")

        expect_args(putHttr_Stub, 1, connection = study$connection, presignedUrl, fileName = "../data/testFile.csv",
                    message = expectedEndpointUpload)

        expect_args(putHttr_Stub, 2, connection = study$connection, expectedUrlConfirm,
                    message = expectedEndpointConfirm)
    }

    return(res)
}

test_that("remote error - missing reportRunId parameter", {
    data <- checkDataFormat(data)
    expectedBody <- '{"instance":"flowName","name":"TestSensor","fileName":"../data/testFile.csv"}'
    expectedEndpoint <- "Getting presignedUrl to upload data"

    expect_error(mockedPrivateUpload(params, study_cloud, data, respondent, expectedBody, expectedEndpoint, sampleName,
                                     scriptName),
                 "Required `reportRunId` field in params for remote connection",
                 info = "missing `reportRunId` field in params not handled properly")
})

test_that("local check - upload signals to a given respondent/segment", {
    data <- checkDataFormat(data)
    expectedBody <- '{"flowName":"flowName","sampleName":"TestSensor","fileName":"../data/testFile.csv"}'
    expectedEndpoint <- "Uploading sensor data for respondent: Wendy"

    res <- mockedPrivateUpload(params, study, data, respondent, expectedBody, expectedEndpoint, sampleName, scriptName)
    expect_identical(res$filePath, uploadUrlRespondentPath, info = "wrong path returned")

    # Also when a stimulus is provided
    expectedEndpoint <- "Uploading sensor data for respondent: Wendy, stimulus: AntiSmoking40Sec"
    res <- mockedPrivateUpload(params, study, data, respondent, expectedBody, expectedEndpoint, sampleName, scriptName,
                               stimulus = stimulus)

    expect_identical(res$filePath, uploadUrlStimulusPath, info = "wrong path returned")

    # Also when a segment is provided
    expectedEndpoint <- "Uploading sensor data for segment: 2 GSR 81-1, stimulus: AntiSmoking40Sec"
    res <- mockedPrivateUpload(params, study, data, segment, expectedBody, expectedEndpoint, sampleName, scriptName,
                               stimulus = stimulus)

    expect_identical(res$filePath, uploadUrlSegmentPath, info = "wrong path returned")
})

test_that("remote check - upload signals to a given respondent/segment", {
    # upload to a respondent
    data <- checkDataFormat(data)
    params$reportRunId <- "1234"
    expectedBody <- '{"instance":"flowName","name":"TestSensor","fileName":"../data/testFile.csv"}'
    expectedEndpoint <- "Getting presignedUrl to upload data"

    res <- mockedPrivateUpload(params, study_cloud, data, respondent, expectedBody, expectedEndpoint, sampleName,
                               scriptName)

    expect_equal(length(res), 6, info = "should return a lot of information")
})

test_that("local check - upload events to a given respondent/segment", {
    dataEvents <- checkDataFormat(dataEvents)
    expectedBody <- '{"flowName":"flowName","sampleName":"ET_REventApi","fileName":"../data/testFile.csv"}'
    expectedEndpoint <- "Uploading events for respondent: Wendy"

    res <- mockedPrivateUpload(params, study, dataEvents, respondent, expectedBody, expectedEndpoint)
    expect_identical(res$filePath, uploadUrlEventPath, info = "wrong path returned")
})

test_that("local check - upload metrics to a given respondent/segment", {
    dataMetrics <- checkDataFormat(dataMetrics)
    expectedBody <- '{"flowName":"flowName","sampleName":"ET_RMetricsApi","fileName":"../data/testFile.csv"}'
    expectedEndpoint <- "Uploading metrics for respondent: Wendy"

    res <- mockedPrivateUpload(params, study, dataMetrics, respondent, expectedBody, expectedEndpoint)
    expect_identical(res$filePath, uploadUrlMetricsPath, info = "wrong path returned")
})

test_that("remote check - upload export to a given segment", {
    # upload to a segment
    dataExport <- checkDataFormat(dataExport)
    params$reportRunId <- "1234"
    expectedBody <- '{"instance":"flowName","name":"TestSensor","fileName":"../data/testFile.csv"}'
    expectedEndpoint <- "Getting presignedUrl to upload data"

    res <- mockedPrivateUpload(params, study_cloud, dataExport, segment, expectedBody, expectedEndpoint, sampleName,
                               scriptName)

    expect_equal(length(res), 6, info = "should return a lot of information")
})

# uploadSensorData ====================================================================================================
context("uploadSensorData()")

sensorName <- "TestSensor"

test_that("error - arguments are missing or not from the good class", {
    # in case of missing params
    expect_error(uploadSensorData(), "Please specify parameters used for your script",
                 info = "missing `params` param not handled properly")

    # in case of missing study
    expect_error(uploadSensorData(params), "Please specify a study loaded with `imStudy()`", fixed = TRUE,
                 info = "missing `study` param not handled properly")

    # in case of missing data
    expect_error(uploadSensorData(params, study), "Please specify a data.table with signals to upload",
                 info = "missing `data` param not handled properly")

    # in case of missing target
    expect_error(uploadSensorData(params, study, data),
                 "Please specify a target respondent/segment loaded with `getRespondents()` or `getSegments()`",
                 fixed = TRUE, info = "missing `target` param not handled properly")

    # in case of missing sensorName
    expect_error(uploadSensorData(params, study, data, respondent),
                 "Please specify a name for the new sensor to upload",
                 info = "missing `sensorName` param not handled properly")

    # in case of missing scriptName
    expect_error(uploadSensorData(params, study, data, respondent, sensorName = sensorName),
                 "Please specify the name of the script used to produce this data",
                 info = "missing `scriptName` param not handled properly")

    # in case of study that is not an imStudy object
    expect_error(uploadSensorData(params, study = "whatever", data, respondent, sensorName, scriptName),
                 "`study` argument is not an imStudy object",
                 info = "study not being an imStudy object should throw an error")

    # in case of target that is not an imRespondent/imSegment object
    expect_error(uploadSensorData(params, study, data, target = "whatever", sensorName, scriptName),
                 "`target` argument is not an imRespondent or imSegment object",
                 info = "target not being an imRespondent/imSegment object should throw an error")

    # in case of stimulus that is not an imStimulus object
    expect_error(uploadSensorData(params, study, data, respondent, sensorName, scriptName, stimulus = "whatever"),
                 "`stimulus` argument is not an imStimulus object",
                 info = "stimulus not being an imStimulus object should throw an error")

    # in case of params required field not provided
    wrongParams <- list("FirstParam" = "blah")
    expect_error(uploadSensorData(wrongParams, study, data, respondent, sensorName, scriptName),
                 "Required `iMotionsVersion` field in params",
                 info = "missing `iMotionsVersion` field in params not handled properly")

    wrongParams <- list("iMotionsVersion" = "blah")
    expect_error(uploadSensorData(wrongParams, study, data, respondent, sensorName, scriptName),
                 "Required `flowName` field in params",
                 info = "missing `flowName` field in params not handled properly")

    # in case of stimulus missing for a segment target
    expect_error(uploadSensorData(params, study, data, segment, sensorName, scriptName),
                 "Please specify a stimulus to upload data to a segment target",
                 info = "missing `stimulus` param for segment target not handled properly")

    # in case of wrong data format
    wrongData <- data.table(Timestamp = integer(), variableTest = numeric())
    expect_error(uploadSensorData(params, study, wrongData, respondent, sensorName, scriptName),
                 "Do not upload an empty dataset", info =  "zero row dataset should not be uploaded")

    wrongData <- data[, 1, drop = FALSE]
    expect_error(uploadSensorData(params, study, wrongData, respondent, sensorName, scriptName),
                 "Dataset must contain at least two columns (Timestamp included)", fixed = TRUE,
                 info = "Can't upload without data columns")

    wrongData <- data[, 2, drop = FALSE]
    expect_error(uploadSensorData(params, study, wrongData, respondent, sensorName, scriptName),
                 "Wrong data format for upload (must be imSignals, imMetrics or imEvents)", fixed = TRUE,
                 info = "Timestamp column should be present")
})


test_that("check - should call privateUpload with the good parameters", {
    data <- checkDataFormat(data)
    additionalMetadata <- data.frame("Group" = c("", "Thresholded"), "Units" = c("ms", "binary"))
    privateUpload_Stub <- mock()

    mockr::with_mock(
        privateUpload = privateUpload_Stub, {
            uploadSensorData(params, study, data, respondent, sensorName, scriptName, additionalMetadata)
        })

    expect_called(privateUpload_Stub, 1)
    expect_args(privateUpload_Stub, 1, params = params, study = study, data = data, target = respondent,
                sampleName = sensorName, scriptName = scriptName, metadata = additionalMetadata,
                stimulus = NULL)

})


test_that("check - should not call privateUpload if data is of wrong format", {
    wrongData <- data.frame("NotTimestamp" = seq(1:100), "Thresholded value" = rep(0, 100))
    privateUpload_Stub <- mock()

    expect_error(mockr::with_mock(
        privateUpload = privateUpload_Stub, {
            uploadSensorData(params, study, wrongData, respondent, sensorName, scriptName)
        }),
        "Wrong data format for upload (must be imSignals, imMetrics or imEvents)", fixed = TRUE,
        info = "Timestamp column should be present")

    expect_called(privateUpload_Stub, 0)

    # in case data is actually an event table
    wrongData <- data.frame("Timestamp" = seq(1:100), "EventName" = "Event 1", "Description" = "Description 1")

    expect_warning(mockr::with_mock(
        privateUpload = privateUpload_Stub, {
            uploadSensorData(params, study, wrongData, respondent, sensorName, scriptName)
        }),
        "Data to upload should be a data.frame/data.table containing a Timestamp column",
        info = "wrong data type detected")

    expect_called(privateUpload_Stub, 0)
})


# uploadEvents ========================================================================================================
context("uploadEvents()")

# Create data to upload
dataEvents <- data.table("Timestamp" = seq(1:100), "EventName" = rep("Event 1", 100), "Description" = rep("test", 100))
eventsName <- "TestEvent"

test_that("error - arguments are missing or not from the good class", {
    # in case of missing params
    expect_error(uploadEvents(), "Please specify parameters used for your script",
                 info = "missing `params` param not handled properly")

    # in case of missing study
    expect_error(uploadEvents(params), "Please specify a study loaded with `imStudy()`", fixed = TRUE,
                 info = "missing `study` param not handled properly")

    # in case of missing data
    expect_error(uploadEvents(params, study), "Please specify a data.table with events to upload",
                 info = "missing `data` param not handled properly")

    # in case of missing target
    expect_error(uploadEvents(params, study, dataEvents),
                 "Please specify a target respondent loaded with `getRespondents()`", fixed = TRUE,
                 info = "missing `target` param not handled properly")

    # in case of missing eventsName
    expect_error(uploadEvents(params, study, data, respondent), "Please specify a name for the new events to upload",
                 info = "missing `eventsName` param not handled properly")

    # in case of missing scriptName
    expect_error(uploadEvents(params, study, data, respondent, eventsName = eventsName),
                 "Please specify the name of the script used to produce this data",
                 info = "missing `scriptName` param not handled properly")

    # in case of study that is not an imStudy object
    expect_error(uploadEvents(params, study = "whatever", dataEvents, respondent, eventsName, scriptName),
                 "`study` argument is not an imStudy object",
                 info = "study not being an imStudy object should throw an error")

    # in case of target that is not an imRespondent object
    expect_error(uploadEvents(params, study, dataEvents, target = "whatever", eventsName, scriptName),
                 "`target` argument is not an imRespondent object",
                 info = "target not being an imRespondent object should throw an error")

    # in case of params required field not provided
    wrongParams <- list("FirstParam" = "blah")
    expect_error(uploadEvents(wrongParams, study, dataEvents, respondent, eventsName, scriptName),
                 "Required `iMotionsVersion` field in params",
                 info = "missing `iMotionsVersion` field in params not handled properly")

    wrongParams <- list("iMotionsVersion" = "blah")
    expect_error(uploadEvents(wrongParams, study, dataEvents, respondent, eventsName, scriptName),
                 "Required `flowName` field in params",
                 info = "missing `flowName` field in params not handled properly")

    # in case of wrong data format
    wrongData <- data.table(Timestamp = integer(), variableTest = numeric())
    expect_error(uploadEvents(params, study, wrongData, respondent, eventsName, scriptName),
                 "Do not upload an empty dataset",
                 info = "zero row dataset should not be uploaded")

    wrongData <- data[, 1, drop = FALSE]
    expect_error(uploadEvents(params, study, wrongData, respondent, eventsName, scriptName),
                 "Dataset must contain at least two columns (Timestamp included)", fixed = TRUE,
                 info = "Can't upload without data columns")

    wrongData <- data[, 2, drop = FALSE]
    expect_error(uploadEvents(params, study, wrongData, respondent, eventsName, scriptName),
                 "Wrong data format for upload (must be imSignals, imMetrics or imEvents)", fixed = TRUE,
                 info = "Timestamp column should be present")
})


test_that("check - should call privateUpload with the good parameters", {
    dataEvents <- checkDataFormat(dataEvents)
    additionalMetadata <- data.table("Units" = c("ms", "", ""), "Show" = c("FALSE", "TRUE", "TRUE"))
    privateUpload_Stub <- mock()

    mockr::with_mock(
        privateUpload = privateUpload_Stub, {
            uploadEvents(params, study, dataEvents, respondent, eventsName, scriptName, additionalMetadata)
        })

    expect_called(privateUpload_Stub, 1)
    expect_args(privateUpload_Stub, 1, params = params, study = study, data = dataEvents, target = respondent,
                eventsName = eventsName, scriptName = scriptName, metadata = additionalMetadata)
})


test_that("check - should not call privateUpload if data is of wrong format", {
    wrongData <- data.frame("NotTimestamp" = seq(1:100), "Thresholded value" = rep(0, 100))
    privateUpload_Stub <- mock()

    expect_error(mockr::with_mock(
        privateUpload = privateUpload_Stub, {
            uploadEvents(params, study, wrongData, respondent, eventsName, scriptName)
        }),
        "Wrong data format for upload (must be imSignals, imMetrics or imEvents)", fixed = TRUE,
        info = "Timestamp column should be present")

    expect_called(privateUpload_Stub, 0)

    # in case data is actually a signal table
    wrongData <- data.frame("Timestamp" = seq(1:100), "Thresholded value" = rep(0, 100))

    expect_warning(mockr::with_mock(
        privateUpload = privateUpload_Stub, {
            uploadEvents(params, study, wrongData, respondent, eventsName, scriptName)
        }),
        "Events should be a data.frame/data.table containing EventName, Timestamp and Description columns",
        info = "wrong data type detected")

    expect_called(privateUpload_Stub, 0)
})


# uploadMetrics =======================================================================================================
context("uploadMetrics()")

metricsName <- "TestMetrics"

test_that("error - arguments are missing or not from the good class", {
    # in case of missing params
    expect_error(uploadMetrics(), "Please specify parameters used for your script",
                 info = "missing `params` param not handled properly")

    # in case of missing study
    expect_error(uploadMetrics(params), "Please specify a study loaded with `imStudy()`", fixed = TRUE,
                 info = "missing `study` param not handled properly")

    # in case of missing data
    expect_error(uploadMetrics(params, study), "Please specify a data.table with metrics to upload",
                 info = "missing `data` param not handled properly")

    # in case of missing target
    expect_error(uploadMetrics(params, study, dataMetrics),
                 "Please specify a target respondent loaded with `getRespondents()`", fixed = TRUE,
                 info = "missing `target` param not handled properly")


    # in case of missing metricsName
    expect_error(uploadMetrics(params, study, data, respondent), "Please specify a name for the new metrics to upload",
                 info = "missing `metricsName` param not handled properly")

    # in case of missing scriptName
    expect_error(uploadMetrics(params, study, data, respondent, metricsName = metricsName),
                 "Please specify the name of the script used to produce this data",
                 info = "missing `scriptName` param not handled properly")


    # in case of study that is not an imStudy object
    expect_error(uploadMetrics(params, study = "whatever", dataMetrics, respondent, metricsName, scriptName),
                 "`study` argument is not an imStudy object",
                 info = "study not being an imStudy object should throw an error")

    # in case of target that is not an imRespondent object
    expect_error(uploadMetrics(params, study, dataMetrics, target = "whatever", metricsName, scriptName),
                 "`target` argument is not an imRespondent object",
                 info = "target not being an imRespondent object should throw an error")

    # in case of params required field not provided
    wrongParams <- list("FirstParam" = "blah")
    expect_error(uploadMetrics(wrongParams, study, dataMetrics, respondent, metricsName, scriptName),
                 "Required `iMotionsVersion` field in params",
                 info = "missing `iMotionsVersion` field in params not handled properly")

    wrongParams <- list("iMotionsVersion" = "blah")
    expect_error(uploadMetrics(wrongParams, study, dataMetrics, respondent, metricsName, scriptName),
                 "Required `flowName` field in params",
                 info = "missing `flowName` field in params not handled properly")

    # in case of wrong data format
    wrongData <- data.table(Timestamp = integer(), variableTest = numeric())
    expect_error(uploadMetrics(params, study, wrongData, respondent, metricsName, scriptName),
                 "Do not upload an empty dataset",
                 info = "zero row dataset should not be uploaded")

    wrongData <- data[, 1, drop = FALSE]
    expect_error(uploadMetrics(params, study, wrongData, respondent, metricsName, scriptName),
                 "Dataset must contain at least two columns (Timestamp included)", fixed = TRUE,
                 info = "Can't upload without data columns")

    wrongData <- data[, 2, drop = FALSE]
    expect_error(uploadMetrics(params, study, wrongData, respondent, metricsName, scriptName),
                 "Wrong data format for upload (must be imSignals, imMetrics or imEvents)", fixed = TRUE,
                 info = "Timestamp column should be present")
})


test_that("check - should call privateUpload with the good parameters", {
    dataMetrics <- checkDataFormat(dataMetrics)
    additionalMetadata <- data.table("Units" = c("", "ms", "", ""), "Show" = c("FALSE", "FALSE", "TRUE", "TRUE"))
    privateUpload_Stub <- mock()

    mockr::with_mock(
        privateUpload = privateUpload_Stub, {
            uploadMetrics(params, study, dataMetrics, respondent, metricsName, scriptName, additionalMetadata)
        })

    expect_called(privateUpload_Stub, 1)
    expect_args(privateUpload_Stub, 1, params = params, study = study, data = dataMetrics, target = respondent,
                sampleName = metricsName, scriptName = scriptName, metadata = additionalMetadata)
})


test_that("check - should not call privateUpload if data is of wrong format", {
    wrongData <- data.frame("NotTimestamp" = seq(1:100), "Thresholded value" = rep(0, 100))
    privateUpload_Stub <- mock()

    expect_error(mockr::with_mock(
        privateUpload = privateUpload_Stub, {
            uploadMetrics(params, study, wrongData, respondent, metricsName, scriptName)
        }),
        "Wrong data format for upload (must be imSignals, imMetrics or imEvents)", fixed = TRUE,
        info = "Timestamp column should be present")

    expect_called(privateUpload_Stub, 0)

    # in case data is actually a signal table
    wrongData <- data.frame("Timestamp" = seq(1:100), "Thresholded value" = rep(0, 100))

    expect_warning(mockr::with_mock(
        privateUpload = privateUpload_Stub, {
            uploadMetrics(params, study, wrongData, respondent, metricsName, scriptName)
        }),
        paste("Metrics should be a data.frame/data.table containing a StimulusId column, a Timestamp",
              "column and at least one other column containing metrics"),
        info = "wrong data type detected")

    expect_called(privateUpload_Stub, 0)
})

# createExport ========================================================================================================
context("createExport()")

# Create data to export
data <- data.table("Respondent Name" = "Respondent 1", "Metrics1" = seq(1:100), "Thresholded value" = rep(0, 100),
                   check.names = FALSE)

additionalMetadata <- data.table("Group" = c("", "numeric", "Thresholded"), "Units" = c("", "ms", "binary"))

test_that("local/remote error - arguments are missing or not from the good class", {
    # in case of missing params
    expect_error(createExport(), "Please specify parameters used for your script",
                 info = "missing `params` param not handled properly")

    # in case of missing study
    expect_error(createExport(params), "Please specify a study loaded with `imStudy()`", fixed = TRUE,
                 info = "missing `study` param not handled properly")

    # in case of missing data
    expect_error(createExport(params, study), "Please specify a data.table to export",
                 info = "missing `data` param not handled properly")

    # in case of missing outputDirectory
    expect_error(createExport(params, study, data), "Please specify an outputDirectory filepath to export the file",
                 info = "missing `outputDirectory` param not handled properly")

    # in case of missing fileName
    expect_error(createExport(params, study, data, "outputDirectoryPath"),
                 "Please specify the name of the file to create",
                 info = "missing `filename` param not handled properly")

    # in case of wrong data format
    wrongData <- data.table(Timestamp = rep(2, 2), variableTest = 2)
    expect_error(createExport(params, study, wrongData, "outputDirectoryPath", "export.csv"),
                 "Wrong data format for export (must be imMetrics or imExport)", fixed = TRUE,
                 info = "signals data should not be exported")

    # in case of missing segment for remote studies
    expect_error(createExport(params, study_cloud, data, "outputDirectoryPath", "export.csv"),
                 "Please specify a segment to upload export for remote connection",
                 info = "missing `segment` param not handled properly")
})

outputDirectory <- "outputDirectoryPath"
fileName <- "export.csv"
expectedData <- fread("../data/exportData.csv", skip = 2)

mockedCreateExport <- function(params, study, data, outputDirectory, fileName, expectedfilePath, expectCallUpload,
                               expectCallWrite, additionalMetadata = NULL, segment = NULL) {

    privateUpload_Stub <- mock()
    privateSaveToFile_Stub <- mock()
    dir.create_Stub <- mock()

    mockr::with_mock(
        privateUpload = privateUpload_Stub,
        privateSaveToFile = privateSaveToFile_Stub,
        dir.create = dir.create_Stub, {
            createExport(params, study, data, outputDirectory, fileName, additionalMetadata, segment)
        })

    expect_called(privateUpload_Stub, expectCallUpload)
    expect_called(privateSaveToFile_Stub, expectCallWrite)
    expect_args(dir.create_Stub, 1, path = outputDirectory)

    if (expectCallUpload > 0) {
        expect_args(privateUpload_Stub, 1, params, study, data, segment, sampleName = expectedfilePath,
                    scriptName = NULL, metadata = additionalMetadata)
    }

    if (expectCallWrite > 0) {
        expect_args(privateSaveToFile_Stub, 1, params, study, data, sampleName = expectedfilePath, scriptName = NULL,
                    metadata = additionalMetadata)
    }
}


test_that("local check - work with good data format", {
    # should call privateSaveToFile with the good parameters without metadata
    data <- checkDataFormat(data)
    expectedfilePath <- "outputDirectoryPath/export.csv"

    mockedCreateExport(params, study, data, outputDirectory, fileName, expectedfilePath = expectedfilePath,
                       expectCallUpload = 0, expectCallWrite = 1)


    # should call privateSaveToFile with the good parameters with metadata
    mockedCreateExport(params, study, data, outputDirectory, fileName, expectedfilePath = expectedfilePath,
                       expectCallUpload = 0, expectCallWrite = 1, additionalMetadata = additionalMetadata)
})

test_that("remote check - work with good data format", {
    # should call privateUpload with the good parameters without metadata
    data <- checkDataFormat(data)
    expectedfilePath <- "outputDirectoryPath/export.csv"

    mockedCreateExport(params, study_cloud, data, outputDirectory, fileName, expectedfilePath = expectedfilePath,
                       expectCallUpload = 1, expectCallWrite = 0, segment = segment)


    # should call privateUpload with the good parameters with metadata
    mockedCreateExport(params, study_cloud, data, outputDirectory, fileName, expectedfilePath = expectedfilePath,
                       expectCallUpload = 1, expectCallWrite = 0, additionalMetadata = additionalMetadata,
                       segment = segment)
})

test_that("local/remote error - wrong data format", {
    # local - should not call dir.create, privateSaveToFile and privateUpload if wrong data format
    wrongData <- data.frame("Timestamp" = seq(1:100), "Thresholded value" = rep(0, 100))

    expect_error(mockedCreateExport(params, study, wrongData, outputDirectory, fileName,
                                    expectedfilePath = expectedfilePath, expectCallUpload = 0, expectCallWrite = 0),
                 "Wrong data format for export (must be imMetrics or imExport)", fixed = TRUE,
                 info = "Timestamp column should not be present")

    # remote - should not call dir.create, privateSaveToFile and privateUpload if wrong data format
    expect_error(mockedCreateExport(params, study_cloud, wrongData, outputDirectory, fileName,
                                    expectedfilePath = expectedfilePath, expectCallUpload = 0, expectCallWrite = 0,
                                    segment = segment),
                 "Wrong data format for export (must be imMetrics or imExport)", fixed = TRUE,
                 info = "Timestamp column should not be present")
})
