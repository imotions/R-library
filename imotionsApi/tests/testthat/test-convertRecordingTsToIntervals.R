# convertRecordingTsToIntervals =======================================================================================
context("convertRecordingTsToIntervals()")

library(mockery)

# Load signals and intervals
signals <- arrow::read_parquet("../data/ET_Eyetracker.csv.pbin")
intervals <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imIntervalList.json")))

test_that("error - arguments are missing or not from the good class", {
    # in case of missing signals
    expect_error(convertRecordingTsToIntervals(),
                 "Please specify an array, scalar or data.table with timestamps to modify",
                 info = "missing `recordingTs` param not handled properly")

    # in case of missing intervals
    expect_error(convertRecordingTsToIntervals(recordingTs = 10),
                 "Please specify intervals loaded with `getRespondentIntervals()` or `getAOIRespondentData()`",
                 fixed = TRUE, info = "missing `intervals` param not handled properly")

    # in case of intervals that is not an imInterval or imIntervalList object
    expect_error(convertRecordingTsToIntervals(recordingTs = 10, intervals = "whatever"),
                 "`intervals` argument is not an imInterval or imIntervalList object",
                 info = "intervals not being an imInterval or imIntervalList object object should throw an error")

    # in case of intervals that are from multiple stimuli
    expect_error(convertRecordingTsToIntervals(recordingTs = 10, intervals),
                 "`intervals` argument contain more than one stimulus/scene/aoi",
                 info = "intervals from multiple stimuli should throw an error")

    # in case of recordingTs not being numeric
    expect_error(convertRecordingTsToIntervals(recordingTs = c("blah", "blah"), intervals[1, ]),
                 "`recordingTs` array or scalar must be of numeric type",
                 info = "recordingTs from type string should throw an error")

    expect_error(convertRecordingTsToIntervals(recordingTs = c(TRUE, FALSE), intervals[1, ]),
                 "`recordingTs` array or scalar must be of numeric type",
                 info = "recordingTs from type boolean should throw an error")
})


test_that("check - converts timestamps from signals", {
    # Get only signals for the first stimulus (interval)
    interval <- intervals[1, ]
    convertedSignals <- convertRecordingTsToIntervals(signals, interval)
    expectedData <- truncateSignalsByIntervals(signals, interval)

    expect_identical(convertedSignals[, -c("Timestamp")], expectedData[, -c("Timestamp")], "should be truncated")
    expect_equal(convertedSignals$Timestamp, expectedData$Timestamp - interval$fragments.start,
                 info = "timestamps should be converted")

    expect_lt(nrow(convertedSignals), nrow(signals), "converted signals", "original signals")
    expect_s3_class(convertedSignals, "imSignals")

    # Get signals for a scene with 2 intervals fragments
    intervals <- intervals[id == 1008, ]
    convertedSignals <- convertRecordingTsToIntervals(signals, intervals)
    expectedData <- truncateSignalsByIntervals(signals, intervals)
    expectedFirstFragment <- truncateSignalsByIntervals(signals, intervals[1, ])
    expectedSecondFragment <- truncateSignalsByIntervals(signals, intervals[2, ])

    expect_identical(convertedSignals[, -c("Timestamp")], expectedData[, -c("Timestamp")], "should be truncated")
    expect_equal(convertedSignals[seq_len(nrow(expectedFirstFragment)), ]$Timestamp,
                 expectedFirstFragment$Timestamp - intervals[1, ]$fragments.start,
                 info = "timestamps should be converted")

    expect_equal(convertedSignals[seq(nrow(expectedFirstFragment) + 1, nrow(convertedSignals)), ]$Timestamp,
                 expectedSecondFragment$Timestamp - intervals[2, ]$fragments.start + intervals[1, ]$fragments.duration,
                 info = "timestamps should be converted")

    expect_lt(nrow(convertedSignals), nrow(signals), "converted signals", "original signals")
    expect_s3_class(convertedSignals, "imSignals")
})


test_that("check - converts scalar or array", {
    # Converts timestamps using the first stimulus (interval)
    interval <- intervals[1, ]
    convertedScalar <- convertRecordingTsToIntervals(6073.691, interval)
    expect_equal(convertedScalar, 0, 1e-2, info = "wrong timestamp conversion")

    # Converts timestamps using a scene with 2 intervals fragments
    intervals <- intervals[id == 1008, ]
    convertedArray <- convertRecordingTsToIntervals(c(6075.691, 18440.691), intervals)
    expect_equal(convertedArray, c(0, 7403), 1e-2, info = "wrong timestamp conversion")

    # KeepTs set to true should keep timestamps outside of intervals unchanged
    convertedArray <- convertRecordingTsToIntervals(c(6075.691, 18440.691, 120402), intervals, keepTs = TRUE)
    expect_equal(convertedArray, c(0, 7403, 120402), 1e-2, info = "wrong timestamp conversion")

    # KeepTs set to NA should keep add NA for timestamps out of intervals
    convertedArray <- convertRecordingTsToIntervals(c(6075.691, 18440.691, 120402), intervals, keepTs = "NA")
    expect_equal(convertedArray, c(0, 7403, NA), 1e-2, info = "wrong timestamp conversion")
})
