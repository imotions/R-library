# privateDownloadData =================================================================================================
context("privateDownloadData()")

library(mockery)

# Load study
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))

# Load sensor
sensors <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imSensorList.json")))
sensor <- sensors[3, ]

expectedMessage <- "Retrieving data for sensor: Eyetracker"

mockedPrivateDownloadData <- function(study, sensor, fileInfos, expectedMessage, expectedFileName = NULL,
                                      signalsName = NULL) {

    expectedUrl <- getSensorDataUrl(study = study, sensor = sensor)
    getJSON_Stub <- mock(fileInfos)
    getFile_Stub <- mock(fileInfos)

    signals <- mockr::with_mock(getJSON = getJSON_Stub,
                                getFile = getFile_Stub, {
                                    privateDownloadData(study, sensor, signalsName)
                                })

    if (study$connection$localIM) {
        expect_args(getJSON_Stub, 1, study$connection, expectedUrl, expectedMessage)
    } else {
        expect_args(getFile_Stub, 1, study$connection, expectedUrl, expectedMessage, expectedFileName)
    }

    return(signals)
}

test_that("local return - correct signals file", {
    # Case where timestamps are not changed
    fileInfos <- list(binFile = "../data/ET_Eyetracker.csv.pbin", timestampBinFile = "")
    signals <- mockedPrivateDownloadData(study, sensor, fileInfos, expectedMessage)

    expect_gt(nrow(signals), 100, "data points should be present.")
    expect_equal(ncol(signals), 14, info = "should have 14 different columns")

    # Case where timestamps are changed
    fileInfos <- list(binFile = "../data/ET_Eyetracker.csv.pbin",
                      timestampBinFile = "../data/ET_Eyetracker.csv.ts.pbin")

    signalsWithLatency <- mockedPrivateDownloadData(study, sensor, fileInfos, expectedMessage)

    expect_equal(nrow(signalsWithLatency), nrow(signals), info = "data points should be preserved")
    expect_equal(ncol(signalsWithLatency), ncol(signals), info = "columns should be preserved")
    diffTimestamp <- unique(signals$Timestamp - signalsWithLatency$Timestamp)
    expect_equal(diffTimestamp, 100, info = "100 ms delay should have been added")

    # Case where columns are changed
    signalToKeep <- c("ET_PupilLeft")
    signal <- mockedPrivateDownloadData(study, sensor, fileInfos, expectedMessage, signalsName = signalToKeep)

    expect_equal(nrow(signal), nrow(signals), info = "data points should be preserved")
    expect_named(signal, c("Timestamp", signalToKeep), info = "incorrect columns name")
})

# Get the study/sensors through the cloud
study_cloud <- jsonlite::unserializeJSON(readLines("../data/imStudy_cloud.json"))
sensors_cloud <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imSensorList_cloud.json")))

test_that("remote return - correct signals file", {
    fileInfos <- list(file_path = "../data/Native_SlideEvents_cloud.csv")

    # Case with slide events
    expectedFileName <- sensors_cloud[3, ]$fileName
    expectedMessage <- "Retrieving data for sensor: SlideEvents"

    signals <- mockedPrivateDownloadData(study_cloud, sensors_cloud[3, ], fileInfos, expectedMessage, expectedFileName)
    expect_gt(nrow(signals), 100, "slide events should be retrieved")
    expect_equal(ncol(signals), 7, info = "should have 7 different columns")

    # Case where columns are changed
    signalToKeep <- c("Duration")
    signal <- mockedPrivateDownloadData(study_cloud, sensors_cloud[3, ], fileInfos, expectedMessage, expectedFileName,
                                        signalToKeep)

    expect_equal(nrow(signal), nrow(signals), info = "data points should be preserved")
    expect_named(signal, c("Timestamp", signalToKeep), info = "incorrect columns name")
})

# getSensorData =======================================================================================================
context("getSensorData()")

# Load signals
signals <- arrow::read_parquet("../data/ET_Eyetracker.csv.pbin")

mockedGetSensorData <- function(study, sensor, signalsName = NULL, intervals = NULL, signals) {
    privateDownloadData_Stub <- mock(signals)

    signals <- mockr::with_mock(privateDownloadData = privateDownloadData_Stub, {
                                    getSensorData(study, sensor, signalsName, intervals)
                                })

    expect_args(privateDownloadData_Stub, 1, study = study, sensor = sensor, signalsName = signalsName)
    return(signals)
}

test_that("error - arguments are missing or not from the good class", {
    # in case of missing study
    expect_error(getSensorData(), "Please specify a study loaded with `imStudy()`", fixed = TRUE,
                 info = "missing `study` param not handled properly")

    # in case of missing sensor
    expect_error(getSensorData(study), "Please specify a sensor loaded with `getSensors()`", fixed = TRUE,
                 info = "missing `sensor` param not handled properly")

    # in case of study that is not an imStudy object
    expect_error(getSensorData(study = "whatever", sensor), "`study` argument is not an imStudy object",
                 info = "study not being an imStudy object should throw an error")

    # in case of sensor that is not an imSensor object
    expect_error(getSensorData(study, sensor = "whatever"), "`sensor` argument is not an imSensor object",
                 info = "sensor not being an imSensor object should throw an error")
})

test_that("return - signals data for a specific sensor", {
    signals <- mockedGetSensorData(study, sensor, signals = signals)

    expect_gt(nrow(signals), 100, "data points should be present.")
    expect_equal(ncol(signals), 14, info = "should have 14 different columns")
    expect_s3_class(signals, "imSignals")
})

test_that("return - truncated data with good signalsName data for a specific sensor", {
    intervals <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imIntervalList.json")))
    truncSignals <- mockedGetSensorData(study, sensor, intervals = intervals, signals = signals)

    expect_lt(nrow(truncSignals), nrow(signals), "truncated signals", "original signals")
    expect_equal(ncol(truncSignals), 14, info = "should have 14 different columns")
    expect_s3_class(truncSignals, "imSignals")

    # Should throw an error if intervals come from a different respondent than the sensor
    sensor$respondent[[1]]$id <- "somethingElse"

    expect_error(mockedGetSensorData(study, sensor, intervals = intervals, signals = signals),
                 "sensor and intervals must correspond to the same respondent",
                 info = "intervals come from a different respondent than the sensor respondent")

    # Should throw an error if the sensor comes from a segment
    sensor_segment <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imSensorSegment.json")))

    expect_error(mockedGetSensorData(study, sensor_segment, intervals = intervals, signals = signals),
                 "truncating signals by intervals is not available for sensors at the segment level",
                 info = "sensor comes from a segment and not a respondent")

    # Should pass signalsName information to privateDownloadData (check inside mockedGetSensorData)
    signals <- mockedGetSensorData(study, sensor, signalsName = c("Signal1", "Signal2"), signals = signals)
})
