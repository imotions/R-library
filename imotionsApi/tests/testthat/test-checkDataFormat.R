# checkDataFormat =====================================================================================================
context("checkDataFormat()")

library(mockery)

test_that("error - data is of wrong format", {
    data <- "string"
    expect_error(checkDataFormat(data), "Signals / metrics / events object should be data.frame or data.table",
                 info = "data should be data.frame or data.table")
})

test_that("check - convert to imObject if from the good format", {
    # imSignals if a Timestamp column is found
    data <- data.frame("Timestamp" = seq(1:100), "Thresholded value" = rep(0, 100), check.names = FALSE)
    data <- checkDataFormat(data)
    expect_s3_class(data, "imSignals")

    data <- data.table::data.table("Timestamp" = seq(1:100), "Thresholded value" = rep(0, 100), check.names = FALSE)
    data <- checkDataFormat(data)
    expect_s3_class(data, "imSignals")

    # imEvents if Timestamps, EventName and Description are found
    events <- data.frame("Timestamp" = seq(1:100), "EventName" = "event 1", "Description" = "blah", check.names = FALSE)
    events <- checkDataFormat(events)
    expect_s3_class(events, "imEvents")

    # imAOIMetrics if only composed of 1 row of numeric values and no Timestamp columns
    metrics <- data.table("Metric1" = 2, "Metric3" = 4)
    metrics <- checkDataFormat(metrics)
    expect_s3_class(metrics, "imAOIMetrics")

    # imMetrics if composed of multiple rows, a StimuliId column and Timestamp column
    metrics <- data.table("StimulusId" = c("1000", "1001"), "Timestamp" = c(10, 20), "Metric3" = c(4, 5))
    metrics <- checkDataFormat(metrics)
    expect_s3_class(metrics, "imMetrics")

    # imMetrics if composed of one row, a StimuliId column and Timestamp column
    metrics <- data.table("StimulusId" = "1000", "Timestamp" = 10, "Metric3" = 4)
    metrics <- checkDataFormat(metrics)
    expect_s3_class(metrics, "imMetrics")

    # imExport if a data.table not fitting any of the cases above (non numeric, more than one row, no StimulusId)
    metrics <- data.table::data.table("Respondent Name" = "blah", "Metric1" = 2, "Metric" = 4)
    metrics <- checkDataFormat(metrics)
    expect_s3_class(metrics, "imExport")

    metrics <- data.table::data.table("Metric2" = rep(2, 2), "Metric1" = 2, "Metric" = 4)
    metrics <- checkDataFormat(metrics)
    expect_s3_class(metrics, "imExport")
})

test_that("check - object class stays unchanged if already an imData object", {
    # imSignals if a Timestamp column is found
    data <- data.frame("Timestamp" = seq(1:100), "Thresholded value" = rep(0, 100), check.names = FALSE)
    class(data) <- append(class(data), c("imData", "imTest"))
    data <- checkDataFormat(data)
    expect_s3_class(data, "imTest")
})


# assertUploadFormat ==================================================================================================
context("assertUploadFormat()")

test_that("error - data is of wrong format", {
    # No data rows
    data <- data.frame("Timestamp" = integer(), "Thresholded value" = integer(), check.names = FALSE)
    data <- checkDataFormat(data)
    expect_error(assertUploadFormat(data), "Do not upload an empty dataset", info = "data should not have 0 rows")

    # Only one column
    data <- data.frame("Timestamp" = seq(1:100), check.names = FALSE)
    data <- checkDataFormat(data)
    expect_error(assertUploadFormat(data), "Dataset must contain at least two columns (Timestamp included)",
                 fixed = TRUE, info = "signals should have 2 columns minimum")

    # neither imSignals or imMetrics have more than one row
    data <- data.table("Metric1" = c(2, 3), "Metric3" = c(4, 5))
    expect_error(assertUploadFormat(data), "Wrong data format for upload (must be imSignals, imMetrics or imEvents)",
                 fixed = TRUE, info = "Neither metrics nor signals should throw an error.")
})

# assertExportFormat ==================================================================================================
context("assertExportFormat()")

test_that("error - data is of wrong format", {
    # with an imSignal object
    data <- data.frame("Timestamp" = 1, "Thresholded value" = 2, check.names = FALSE)
    data <- checkDataFormat(data)
    expect_error(assertExportFormat(data), "Wrong data format for export (must be imMetrics or imExport)",
                 fixed = TRUE, info = "export should not be a signal")
})
