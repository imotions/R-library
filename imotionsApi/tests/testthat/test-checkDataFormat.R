context("checkDataFormat()")

library("imotionsApi")
library("mockery")

test_that("should throw errors if data is of wrong format", {
    data <- "string"
    error <- capture_error(checkDataFormat(data))
    expect_identical(error$message, "Signals / metrics / events object should be data.frame or data.table",
                     "data should be data.frame or data.table")
})

test_that("should convert to imObject if from the good format", {
    # imSignals if a Timestamp column is found
    data <- data.frame("Timestamp" = seq(1:100), "Thresholded value" = rep(0, 100), check.names = FALSE)
    data <- checkDataFormat(data)
    assertClass(data, "imSignals", "should be of class imSignals")

    data <- data.table::data.table("Timestamp" = seq(1:100), "Thresholded value" = rep(0, 100), check.names = FALSE)
    data <- checkDataFormat(data)
    assertClass(data, "imSignals", "should be of class imSignals")

    # imEvents if Timestamps, EventName and Description are found
    events <- data.frame("Timestamp" = seq(1:100), "EventName" = "event 1", "Description" = "blah", check.names = FALSE)
    events <- checkDataFormat(events)
    assertClass(events, "imEvents", "should be of class imMetrics")

    # imMetrics if only composed of 1 row and no Timestamp columns
    metrics <- data.frame("Metric1" = 2, "Metric3" = 4)
    metrics <- checkDataFormat(metrics)
    assertClass(metrics, "imMetrics", "should be of class imMetrics")

    # imMetrics if only composed of 1 row of numeric values and no Timestamp columns
    metrics <- data.table("Metric1" = 2, "Metric3" = 4)
    metrics <- checkDataFormat(metrics)
    assertClass(metrics, "imMetrics", "should be of class imMetrics")

    # imMetrics if downloaded from the cloud (appended with a Timestamp column that has to be remove)
    metrics <- data.table::data.table("Timestamp" = 0, "Metric1(METRIC_SUMMARY=true)" = 2,
                                      "Metric3(METRIC_SUMMARY=true)" = 4)

    metrics <- checkDataFormat(metrics)
    assertClass(metrics, "imMetrics", "should be of class imMetrics")
    expect_identical(names(metrics), c("Metric1", "Metric3"), "wrong column names")

    # imExport if a data.table not fitting any of the cases above (non numeric or more than one row)
    metrics <- data.table::data.table("Respondent Name" = "blah", "Metric1" = 2, "Metric" = 4)
    metrics <- checkDataFormat(metrics)
    assertClass(metrics, "imExport", "should be of class imExport")

    metrics <- data.table::data.table("Metric2" = rep(2, 2), "Metric1" = 2, "Metric" = 4)
    metrics <- checkDataFormat(metrics)
    assertClass(metrics, "imExport", "should be of class imExport")
})

test_that("should not change the object class if already an imData object", {
    # imSignals if a Timestamp column is found
    data <- data.frame("Timestamp" = seq(1:100), "Thresholded value" = rep(0, 100), check.names = FALSE)
    class(data) <- append(class(data), c("imData", "imTest"))
    data <- checkDataFormat(data)
    expect_true(inherits(data, "imTest"), "should not have change class")
})



context("assertUploadFormat()");

test_that("should throw errors if data is of wrong format", {
    # No data rows
    data <- data.frame("Timestamp" = integer(), "Thresholded value" = integer(), check.names = FALSE)
    data <- checkDataFormat(data)
    error <- capture_error(assertUploadFormat(data))
    expect_identical(error$message, "Do not upload an empty dataset", "data should not have 0 rows")

    # Only one column
    data <- data.frame("Timestamp" = seq(1:100), check.names = FALSE)
    data <- checkDataFormat(data)
    error <- capture_error(assertUploadFormat(data))
    expect_identical(error$message, "Dataset must contain at least two columns (Timestamp included)",
                     "signals should have 2 columns minimum")

    # Metrics have more than one row
    metrics <- data.table("Metric1" = c(2, 3), "Metric3" = c(4, 5))
    class(metrics) <- append(class(metrics), c("imData", "imMetrics"))
    error <- capture_error(assertUploadFormat(metrics))
    expect_identical(error$message, "Metrics must have exactly one row",  "Metrics can have only one row")

    # neither imSignals or imMetrics have more than one row
    notAnObject <- data.table("Metric1" = c(2, 3), "Metric3" = c(4, 5))
    error <- capture_error(assertUploadFormat(notAnObject))
    expect_identical(error$message, "Wrong data format for upload (must be imSignals, imMetrics or imEvents)",
                     "Neither metrics nor signals should throw an error.")
})

context("assertExportFormat()");

test_that("should throw errors if data is of wrong format", {
    # with an imSignal object
    data <- data.frame("Timestamp" = 1, "Thresholded value" = 2, check.names = FALSE)
    data <- checkDataFormat(data)
    error <- capture_error(assertExportFormat(data))
    expect_identical(error$message, "Wrong data format for export (must be imMetrics or imExport)",
                     "export should not be a signal")
})
