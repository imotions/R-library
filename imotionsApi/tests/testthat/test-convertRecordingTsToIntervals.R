library("imotionsApi");
library("stubthat");

context("convertRecordingTsToIntervals()");

# Load signals and intervals
signals <- arrow::read_parquet("../data/ET_Eyetracker.csv.pbin")
intervals <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imIntervalList.json")))

test_that("should throw errors if arguments are missing or not from the good class", {
    # in case of missing signals
    error <- capture_error(convertRecordingTsToIntervals())
    expect_identical(error$message, "Please specify an array, scalar or data.table with timestamps to modify",
                     "missing `recordingTs` param not handled properly")

    # in case of missing intervals
    error <- capture_error(convertRecordingTsToIntervals(recordingTs = 10))
    expect_identical(error$message, paste("Please specify intervals loaded with `getRespondentIntervals()`",
                                          "or `getAOIRespondentData()`"),
                     "missing `intervals` param not handled properly")

    # in case of intervals that is not an imInterval or imIntervalList object
    error <- capture_error(convertRecordingTsToIntervals(recordingTs = 10, intervals = "whatever"))
    expect_identical(error$message, "`intervals` argument is not an imInterval or imIntervalList object",
                     "intervals not being an imInterval or imIntervalList object object should throw an error")

    # in case of intervals that are from multiple stimuli
    error <- capture_error(convertRecordingTsToIntervals(recordingTs = 10, intervals))
    expect_identical(error$message, "`intervals` argument contain more than one stimulus/scene/aoi",
                     "intervals from multiple stimuli should throw an error")

    # in case of recordingTs not being numeric
    error <- capture_error(convertRecordingTsToIntervals(recordingTs = c("blah", "blah"), intervals[1, ]))
    expect_identical(error$message, "`recordingTs` array or scalar must be of numeric type",
                     "recordingTs from type string should throw an error")

    error <- capture_error(convertRecordingTsToIntervals(recordingTs = c(T, F), intervals[1, ]))
    expect_identical(error$message, "`recordingTs` array or scalar must be of numeric type",
                     "recordingTs from type boolean should throw an error")
})

test_that("Timestamps from signals should be converted as expected", {
    # Get only signals for the first stimulus (interval)
    interval <- intervals[1, ]
    convertedSignals <- convertRecordingTsToIntervals(signals, interval)
    expectedData <- truncateSignalsByIntervals(signals, interval)

    # Check that data got converted and truncated as expected
    expect_identical(convertedSignals[, -c("Timestamp")], expectedData[, -c("Timestamp")], "should be truncated")
    expect_equal(convertedSignals$Timestamp, expectedData$Timestamp - interval$fragments.start,
                 info = "timestamps should be converted")

    expect_lt(nrow(convertedSignals), nrow(signals), "converted signals", "original signals")
    expect_true(inherits(convertedSignals, "imSignals"), "`converted signals should still be an imSignals object")

    # Get signals for a scene with 2 intervals fragments
    intervals <- intervals[id == 1008, ]
    convertedSignals <- convertRecordingTsToIntervals(signals, intervals)
    expectedData <- truncateSignalsByIntervals(signals, intervals)
    expectedFirstFragment <- truncateSignalsByIntervals(signals, intervals[1, ])
    expectedSecondFragment <- truncateSignalsByIntervals(signals, intervals[2, ])

    # Check that data got converted and truncated as expected
    expect_identical(convertedSignals[, -c("Timestamp")], expectedData[, -c("Timestamp")], "should be truncated")
    expect_equal(convertedSignals[seq(nrow(expectedFirstFragment)), ]$Timestamp,
                 expectedFirstFragment$Timestamp - intervals[1, ]$fragments.start,
                 info = "timestamps should be converted")

    expect_equal(convertedSignals[seq(nrow(expectedFirstFragment) + 1, nrow(convertedSignals)), ]$Timestamp,
                 expectedSecondFragment$Timestamp - intervals[2, ]$fragments.start + intervals[1, ]$fragments.duration,
                 info = "timestamps should be converted")

    expect_lt(nrow(convertedSignals), nrow(signals), "converted signals", "original signals")
    expect_true(inherits(convertedSignals, "imSignals"), "`converted signals should still be an imSignals object")
})


test_that("Should also work on scalar or array", {
    # Converts timestamps using the first stimulus (interval)
    interval <- intervals[1, ]
    convertedScalar <- convertRecordingTsToIntervals(6073.691, interval)
    expect_equal(convertedScalar, 0, 1e-2, info = "wrong timestamp conversion")

    # Converts timestamps using a scene with 2 intervals fragments
    intervals <- intervals[id == 1008, ]
    convertedArray <- convertRecordingTsToIntervals(c(6075.691, 18440.691), intervals)
    expect_equal(convertedArray, c(0, 7403), 1e-2, info = "wrong timestamp conversion")
})
