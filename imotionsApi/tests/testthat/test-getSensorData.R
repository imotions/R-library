context("privateDownloadData()")

library("imotionsApi")
library("stubthat")
library("arrow")

# Load study
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))

# Load sensor
sensors <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imSensorList.json")))
sensor <- sensors[1, ]


mockedPrivateDownloadData <- function(study, sensor, signals, filesPath) {
    getSensorDataUrl_Stub <- stub(getSensorDataUrl)
    getSensorDataUrl_Stub$expects(study = study, sensor = sensor)
    getJSON_Stub <- stub(getJSON)
    getJSON_Stub$returns(filesPath)

    signals <- mockr::with_mock(getJSON = getJSON_Stub$f,
                                getSensorDataUrl = getSensorDataUrl_Stub$f, {
                                    privateDownloadData(study, sensor, signals)
                                })

    return(signals)
}

test_that("privateDownloadData() should return the correct signals file during local connection", {
    # Case where timestamps are not changed
    filesPath <- list(binFile = "../data/ET_Eyetracker.csv.pbin", timestampBinFile = "")
    signals <- mockedPrivateDownloadData(study, sensor, signals = NULL, filesPath)

    expect_gt(NROW(signals), 100, "data points should be present.")
    expect_equal(NCOL(signals), 14, info = "should have 14 different columns")

    # Case where timestamps are changed
    filesPath <- list(binFile = "../data/ET_Eyetracker.csv.pbin",
                      timestampBinFile = "../data/ET_Eyetracker.csv.ts.pbin")

    signalsWithLatency <- mockedPrivateDownloadData(study, sensor, signals = NULL, filesPath)

    expect_equal(NROW(signalsWithLatency), NROW(signals), info = "data points should be preserved")
    expect_equal(NCOL(signalsWithLatency), NCOL(signals), info = "columns should be preserved")
    diffTimestamp <- unique(signals$Timestamp - signalsWithLatency$Timestamp)
    expect_equal(diffTimestamp, 100, info = "100 ms delay should have been added")

    # Case where columns are changed
    signalToKeep <- c("ET_PupilLeft")
    signal <- mockedPrivateDownloadData(study, sensor, signals = signalToKeep, filesPath)

    expect_equal(NROW(signal), NROW(signals), info = "data points should be preserved")
    expect_identical(colnames(signal), c("Timestamp", signalToKeep), "incorrect columns name")
})


context("getSensorData()");

# Load signals
signals <- arrow::read_parquet("../data/ET_Eyetracker.csv.pbin")

mockedGetSensorData <- function(study, sensor, signalsName = NULL, intervals = NULL, signals) {
    privateDownloadData_Stub <- stub(privateDownloadData)
    privateDownloadData_Stub$expects(study = study, sensor = sensor, signalsName = signalsName)
    privateDownloadData_Stub$returns(signals)

    signals <- mockr::with_mock(privateDownloadData = privateDownloadData_Stub$f, {
                                    getSensorData(study, sensor, signalsName, intervals)
                                })

    return(signals)
}

test_that("should throw errors if arguments are missing or not from the good class", {
    # in case of missing study
    error <- capture_error(getSensorData())
    expect_identical(error$message, "Please specify a study loaded with `imStudy()`",
                     "missing `study` param not handled properly")

    # in case of missing sensor
    error <- capture_error(getSensorData(study))
    expect_identical(error$message, "Please specify a sensor loaded with `getRespondentSensors()`",
                     "missing `sensor` param not handled properly")

    # in case of study that is not an imStudy object
    error <- capture_error(getSensorData(study = "whatever", sensor))
    expect_identical(error$message, "`study` argument is not an imStudy object",
                     "study not being an imStudy object should throw an error")

    # in case of sensor that is not an imSensor object
    error <- capture_error(getSensorData(study, sensor = "whatever"))
    expect_identical(error$message, "`sensor` argument is not an imSensor object",
                     "sensor not being an imSensor object should throw an error")
})

test_that("should return signals data for this sensor", {
    signals <- mockedGetSensorData(study, sensor, signals = signals)

    expect_gt(NROW(signals), 100, "data points should be present.")
    expect_equal(NCOL(signals), 14, info = "should have 14 different columns")
    expect_true(inherits(signals, "imSignals"), "signals should be an imSignals object")
})

test_that("should return truncated signals data for this sensor and pass signalsName information", {
    intervals <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imIntervalList.json")))
    truncSignals <- mockedGetSensorData(study, sensor, intervals = intervals, signals = signals)

    expect_lt(nrow(truncSignals), nrow(signals), "truncated signals", "original signals")
    expect_equal(NCOL(truncSignals), 14, info = "should have 14 different columns")
    expect_true(inherits(truncSignals, "imSignals"), "signals should be an imSignals object")

    # Should throw an error if intervals come from a different respondent than the sensor
    sensor$respondent[[1]]$id <- "somethingElse"
    error <- capture_error(mockedGetSensorData(study, sensor, intervals = intervals, signals = signals))
    expect_identical(error$message, "sensor and intervals must correspond to the same respondent",
                     "intervals come from a different respondent than the sensor respondent")

    # Should pass signalsName information to privateDownloadData (check inside mockedGetSensorData)
    signals <- mockedGetSensorData(study, sensor, signalsName = c("Signal1", "Signal2"), signals = signals)
})
