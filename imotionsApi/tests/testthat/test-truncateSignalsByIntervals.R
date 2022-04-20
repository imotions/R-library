context("truncateSignalsByIntervals()")

library("imotionsApi")
library("stubthat")

# Load signals and intervals
signals <- arrow::read_parquet("../data/ET_Eyetracker.csv.pbin")
intervals <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imIntervalList.json")))

test_that("should throw errors if arguments are missing or not from the good class", {
    # in case of missing signals
    error <- capture_error(truncateSignalsByIntervals())
    expect_identical(error$message, paste("Please specify an imSignals object as returned by `getSensorData()`",
                                          "or a data.table including a `Timestamp` column"),
                     "missing `signals` param not handled properly")

    # in case of missing intervals
    error <- capture_error(truncateSignalsByIntervals(signals))
    expect_identical(error$message, paste("Please specify intervals loaded with `getRespondentIntervals()`",
                                          "or `getAOIRespondentData()`"),
                     "missing `intervals` param not handled properly")

    # in case of signals that is not of the good format
    error <- capture_error(truncateSignalsByIntervals(signals = "whatever", intervals))
    expect_identical(error$message, "Signals / metrics / events object should be data.frame or data.table",
                     "signals not being of the good format should throw an error")

    # in case of signals that is a metric instead
    metrics <- data.frame("metric1" = 1, "metric2" = 2)
    class(metrics) <- append(class(metrics), c("imData", "imMetrics"))
    error <- capture_error(truncateSignalsByIntervals(signals = metrics, intervals))
    expect_identical(error$message, "`signals` argument is an imMetrics object, not an imSignals object",
                     "metrics should throw an error")

    # in case of intervals that is not an imInterval or imIntervalList object
    error <- capture_error(truncateSignalsByIntervals(signals, intervals = "whatever"))
    expect_identical(error$message, "`intervals` argument is not an imInterval or imIntervalList object",
                     "intervals not being an imInterval or imIntervalList object object should throw an error")
})


test_that("signals should get truncated as expected", {
    # Get only signals for the first stimulus (interval)
    interval <- intervals[1, ]
    truncSignals <- truncateSignalsByIntervals(signals, interval)
    expected <- as.character(which(signals$Timestamp %inrange% interval[, c("fragments.start", "fragments.end")]))

    # Check that data got truncated as expected
    expect_equal(sum(truncSignals$Timestamp >= interval$fragments.end), 0, info = "timestamps should be removed")
    expect_equal(sum(truncSignals$Timestamp <= interval$fragments.start), 0, info = "timestamps should be removed")
    expect_equal(row.names(truncSignals), expected, info = "row names should be as expected")
    expect_lt(nrow(truncSignals), nrow(signals), "truncated signals", "original signals")

    expect_true(inherits(truncSignals, "imSignals"), "`truncated signals should still be an imSignals object")

    # Get signals for 2 intervals and check that timestamps between intervals got removed correctly
    truncSignals2 <- truncateSignalsByIntervals(signals, intervals[1:2, ])
    expected <- as.character(which(signals$Timestamp %inrange% intervals[1:2, c("fragments.start", "fragments.end")]))
    expect_equal(sum(truncSignals2$Timestamp >= intervals$fragments.end[2]), 0, info = "timestamps should be removed")
    expect_equal(sum(truncSignals2$Timestamp <= intervals$fragments.start[1]), 0, info = "timestamps should be removed")
    expect_equal(sum(truncSignals2$Timestamp >= intervals$fragments.end[1] &
                     truncSignals2$Timestamp <= intervals$fragments.start[2]), 0, info = "timestamps should be removed")
    expect_equal(row.names(truncSignals2), expected, info = "row names should be as expected")

    expect_true(inherits(truncSignals2, "imSignals"), "`truncated signals should still be an imSignals object")
})


test_that("signals should get drop as expected if dropIntervals is set to TRUE", {
    # Get signals for everything except the first stimulus (interval)
    interval <- intervals[1, ]
    truncSignals <- truncateSignalsByIntervals(signals, interval, dropIntervals = TRUE)
    expected <- as.character(which(!signals$Timestamp %inrange% interval[, c("fragments.start", "fragments.end")]))

    # Check that data got truncated as expected
    expect_equal(sum(truncSignals$Timestamp %inrange% interval[, c("fragments.start", "fragments.end")]), 0,
                 info = "timestamps should be removed")

    expect_equal(row.names(truncSignals), expected, info = "row names should be as expected")
    expect_lt(nrow(truncSignals), nrow(signals), "truncated signals", "original signals")

    expect_true(inherits(truncSignals, "imSignals"), "`truncated signals should still be an imSignals object")

    # Get signals for everything except 2 intervals and check that timestamps got removed correctly
    truncSignals2 <- truncateSignalsByIntervals(signals, intervals[1:2, ], dropIntervals = TRUE)
    expected <- as.character(which(!signals$Timestamp %inrange% intervals[1:2, c("fragments.start", "fragments.end")]))
    expect_equal(sum(truncSignals2$Timestamp %inrange% intervals[1:2, c("fragments.start", "fragments.end")]), 0,
                 info = "timestamps should be removed")

    expect_equal(row.names(truncSignals2), expected, info = "row names should be as expected")
    expect_true(inherits(truncSignals2, "imSignals"), "`truncated signals should still be an imSignals object")
})
