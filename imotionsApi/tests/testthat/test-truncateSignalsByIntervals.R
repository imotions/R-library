# truncateSignalsByIntervals ==========================================================================================
context("truncateSignalsByIntervals()")

library(mockery)

# Load signals and intervals
signals <- arrow::read_parquet("../data/ET_Eyetracker.csv.pbin")
intervals <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imIntervalList.json")))

test_that("error - arguments are missing or not from the good class", {
    # in case of missing signals
    expect_error(truncateSignalsByIntervals(),
                 paste("Please specify an imSignals object as returned by `getSensorData()`",
                 "or a data.table including a `Timestamp` column"),
                 fixed = TRUE, info = "missing `signals` param not handled properly")

    # in case of missing intervals
    expect_error(truncateSignalsByIntervals(signals),
                 "Please specify intervals loaded with `getRespondentIntervals()` or `getAOIRespondentData()`",
                 fixed = TRUE, info = "missing `intervals` param not handled properly")

    # in case of signals that is not of the good format
    expect_error(truncateSignalsByIntervals(signals = "whatever", intervals),
                 "Signals / metrics / events object should be data.frame or data.table",
                 info = "signals not being of the good format should throw an error")

    # in case of signals that is a metric instead
    metrics <- data.frame("metric1" = 1, "metric2" = 2)
    class(metrics) <- append(class(metrics), c("imData", "imMetrics"))

    expect_error(truncateSignalsByIntervals(signals = metrics, intervals),
                 "`signals` argument is an imMetrics object, not an imSignals object",
                 info = "metrics should throw an error")

    # in case of intervals that is not an imInterval or imIntervalList object
    expect_error(truncateSignalsByIntervals(signals, intervals = "whatever"),
                 "`intervals` argument is not an imInterval or imIntervalList object",
                 info = "intervals not being an imInterval or imIntervalList object object should throw an error")
})


test_that("return - truncated signals", {
    # Get only signals for the first stimulus (interval)
    interval <- intervals[1, ]
    truncSignals <- truncateSignalsByIntervals(signals, interval)
    expected <- as.character(which(signals$Timestamp %inrange% interval[, c("fragments.start", "fragments.end")]))

    # Check that data got truncated as expected
    expect_equal(sum(truncSignals$Timestamp >= interval$fragments.end), 0, info = "timestamps should be removed")
    expect_equal(sum(truncSignals$Timestamp <= interval$fragments.start), 0, info = "timestamps should be removed")
    expect_equal(row.names(truncSignals), expected, info = "row names should be as expected")
    expect_lt(nrow(truncSignals), nrow(signals), "truncated signals", "original signals")
    expect_s3_class(truncSignals, "imSignals")

    # Get signals for 2 intervals and check that timestamps between intervals got removed correctly
    truncSignals2 <- truncateSignalsByIntervals(signals, intervals[1:2, ])
    expected <- as.character(which(signals$Timestamp %inrange% intervals[1:2, c("fragments.start", "fragments.end")]))

    expect_equal(sum(truncSignals2$Timestamp >= intervals$fragments.end[2]), 0, info = "timestamps should be removed")
    expect_equal(sum(truncSignals2$Timestamp <= intervals$fragments.start[1]), 0, info = "timestamps should be removed")
    expect_equal(sum(truncSignals2$Timestamp >= intervals$fragments.end[1] &
                     truncSignals2$Timestamp <= intervals$fragments.start[2]), 0, info = "timestamps should be removed")

    expect_equal(row.names(truncSignals2), expected, info = "row names should be as expected")
    expect_s3_class(truncSignals2, "imSignals")
})


test_that("return - signals should get drop as expected if dropIntervals is set to TRUE", {
    # Get signals for everything except the first stimulus (interval)
    interval <- intervals[1, ]
    truncSignals <- truncateSignalsByIntervals(signals, interval, dropIntervals = TRUE)
    expected <- as.character(which(!signals$Timestamp %inrange% interval[, c("fragments.start", "fragments.end")]))

    # Check that data got truncated as expected
    expect_equal(sum(truncSignals$Timestamp %inrange% interval[, c("fragments.start", "fragments.end")]), 0,
                 info = "timestamps should be removed")

    expect_equal(row.names(truncSignals), expected, info = "row names should be as expected")
    expect_lt(nrow(truncSignals), nrow(signals), "truncated signals", "original signals")
    expect_s3_class(truncSignals, "imSignals")

    # Get signals for everything except 2 intervals and check that timestamps got removed correctly
    truncSignals2 <- truncateSignalsByIntervals(signals, intervals[1:2, ], dropIntervals = TRUE)
    expected <- as.character(which(!signals$Timestamp %inrange% intervals[1:2, c("fragments.start", "fragments.end")]))

    expect_equal(sum(truncSignals2$Timestamp %inrange% intervals[1:2, c("fragments.start", "fragments.end")]), 0,
                 info = "timestamps should be removed")

    expect_equal(row.names(truncSignals2), expected, info = "row names should be as expected")
    expect_s3_class(truncSignals2, "imSignals")
})
