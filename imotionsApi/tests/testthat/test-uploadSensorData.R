library("imotionsApi");
library("stubthat");
library("arrow");

context("uploadSensorData()");

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

additionalMetadata <- data.frame("Group" = c("", "Thresholded"), "Units" = c("ms", "binary"))

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
    expect_identical(error$message, "Please specify a data.table with signals/metrics to upload",
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
    expect_identical(error$message, "Wrong data format for upload (must be imSignals or imMetrics)",
                     "Timestamp column should be present")
})


test_that("should call privateUploadSignals with the good parameters", {
    data <- checkDataFormat(data)
    privateUploadSignals_Stub <- stub(privateUploadSignals)

    privateUploadSignals_Stub$expects(params = params, study = study, data = data, target = respondent,
                                      sensorName = sensorName, scriptName = scriptName,
                                      metadata = NULL, stimulus = NULL)

    mockr::with_mock(
        privateUploadSignals = privateUploadSignals_Stub$f, {
            uploadSensorData(params, study, data, respondent, sensorName, scriptName)
        })

    expect_equal(privateUploadSignals_Stub$calledTimes(), 1, info = "privateUploadSignals() should be called")
})


test_that("should not call privateUploadSignals if data is of wrong format", {
    wrongData <- data.frame("NotTimestamp" = seq(1:100), "Thresholded value" = rep(0, 100))
    privateUploadSignals_Stub <- stub(privateUploadSignals)

    error <- capture_error(mockr::with_mock(
        privateUploadSignals = privateUploadSignals_Stub$f, {
            uploadSensorData(params, study, wrongData, respondent, sensorName = sensorName, scriptName = scriptName)
        }))

    expect_equal(privateUploadSignals_Stub$calledTimes(), 0, info = "privateUploadSignals() should not be called")
    expect_identical(error$message, "Wrong data format for upload (must be imSignals or imMetrics)",
                     "Timestamp column should be present")
})




context("privateUploadSignals()");

expectedBody <- '{"flowName":"flowName","sampleName":"TestSensor","fileName":"../data/testFile.csv"}'
class(expectedBody) <- "json"
uploadUrlStudyPath <- "uploadUrlStudy"
uploadUrlStimulusPath <- "uploadUrlStimulus"

mockedPrivateUploadSignals <- function(params, study, data, respondent, sensorName, scriptName, metadata = NULL,
                                       stimulus = NULL) {

    privateSaveSignalsToFile_Stub <- stub(privateSaveSignalsToFile)
    privateSaveSignalsToFile_Stub$expects(params = params, data = data, sensorName = sensorName,
                                          scriptName = scriptName, metadata = metadata)

    privateSaveSignalsToFile_Stub$return("../data/testFile.csv")

    getUploadSensorsUrl_Stub <- stub(getUploadSensorsUrl)
    getUploadSensorsUrl_Stub$expects(study = study, imObject = respondent, stimulus = stimulus)
    getUploadSensorsUrl_Stub$withArgs(stimulus = stimulus)$returns(uploadUrlStimulusPath)
    getUploadSensorsUrl_Stub$withArgs(stimulus = NULL)$returns(uploadUrlStudyPath)

    if (is.null(stimulus)) {
        endpoint <- paste("target:", respondent$name)
    } else {
        endpoint <- paste0("target: ", respondent$name, ", stimulus: ", stimulus$name)
    }

    postJSON_Stub <- stub(postJSON)
    postJSON_Stub$expects(connection = study$connection, postData = expectedBody,
                          message = paste("Uploading sensor data for", endpoint))

    postJSON_Stub$withArgs(url = uploadUrlStimulusPath)$returns(list(filePath = uploadUrlStimulusPath))
    postJSON_Stub$withArgs(url = uploadUrlStudyPath)$returns(list(filePath = uploadUrlStudyPath))

    res <- mockr::with_mock(privateSaveSignalsToFile = privateSaveSignalsToFile_Stub$f,
                            getUploadSensorsUrl = getUploadSensorsUrl_Stub$f,
                            postJSON = postJSON_Stub$f, {
                                privateUploadSignals(params, study, data, respondent, sensorName, scriptName, metadata,
                                                     stimulus)
                            })

    return(res)
}


test_that("should upload signals to a given respondent/segment if a good request has been sent", {
    res <- mockedPrivateUploadSignals(params, study, data, respondent, sensorName, scriptName)
    expect_identical(res$filePath, uploadUrlStudyPath, info = "wrong path returned")

    # Also when a stimulus is provided
    res <- mockedPrivateUploadSignals(params, study, data, respondent, sensorName, scriptName, stimulus = stimulus)
    expect_identical(res$filePath, uploadUrlStimulusPath, info = "wrong path returned")
})




context("privateSaveSignalsToFile()");

test_that("Data should get stored as a temporary file", {
    tmpDir <- tempdir(check = TRUE)
    data <- checkDataFormat(data)
    dataFileName <- suppressWarnings(privateSaveSignalsToFile(params, data, sensorName, scriptName, additionalMetadata))

    # Check that file exists at the good location
    expect_true(file.exists(dataFileName), "temporary data file should have been created")
    expect_identical(dataFileName, file.path(tmpDir, "result.csv"), "wrong file path")
    dataWritten <- read.csv(dataFileName)
    testData <- read.csv("../data/testFile.csv")
    expect_identical(dataWritten, testData, "files should be identical")

    # Re-calling the function should overwrite the file (not append it below)
    dataFileName2 <- suppressWarnings(privateSaveSignalsToFile(params, data, sensorName, scriptName,
                                                               additionalMetadata))

    expect_identical(dataFileName, dataFileName2, "same file should be used")
    dataWritten <- read.csv(dataFileName)
    expect_identical(dataWritten, testData, "files should still be identical")

    # params without "scratchFolder" path provided should throw a warning
    warning <- capture_warning(privateSaveSignalsToFile(params, data, sensorName, scriptName, additionalMetadata))
    expect_identical(warning$message, "params$scratchFolder not provided - using user tempdir location",
                     "should throw a warning")

    # params with a "scratchFolder" path provided should use this path directly without warnings
    params <- append(params, list("scratchFolder" = tmpDir))
    dataFileName <- privateSaveSignalsToFile(params, data, sensorName, scriptName, additionalMetadata)
    dataWritten <- read.csv(dataFileName)
    expect_identical(dataWritten, testData, "files should still be identical")

    # Cleaning file created for testing
    file.remove(dataFileName)
})


context("privateCreateDataHeader()");

test_that("Data header should be as expected", {
    dataHeader <- privateCreateDataHeader(params, sensorName, scriptName)

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
