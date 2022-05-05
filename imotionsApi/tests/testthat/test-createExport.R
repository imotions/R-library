context("createExport()")

library("imotionsApi")
library("mockery")

# Load study
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))

# Create data to export
data <- data.table("Respondent Name" = "Respondent 1", "Metrics1" = seq(1:100), "Thresholded value" = rep(0, 100),
                   check.names = FALSE)

additionalMetadata <- data.table("Group" = c("", "numeric", "Thresholded"), "Units" = c("", "ms", "binary"))

test_that("should throw errors if arguments are missing or not from the good class", {
    # in case of missing study
    error <- capture_error(createExport())
    expect_identical(error$message, "Please specify a study loaded with `imStudy()`",
                     "missing `study` param not handled properly")

    # in case of missing data
    error <- capture_error(createExport(study))
    expect_identical(error$message, "Please specify a data.table to export",
                     "missing `data` param not handled properly")

    # in case of missing outputDirectory
    error <- capture_error(createExport(study, data))
    expect_identical(error$message, "Please specify an outputDirectory filepath to export the file",
                     "missing `outputDirectory` param not handled properly")

    # in case of missing fileName
    error <- capture_error(createExport(study, data, "outputDirectoryPath"))
    expect_identical(error$message, "Please specify the name of the file to create",
                     "missing `filename` param not handled properly")

    # in case of wrong data format
    wrongData <- data.table(Timestamp = rep(2, 2), variableTest = 2)
    error <- capture_error(createExport(study, wrongData, "outputDirectoryPath", "export.csv"))
    expect_identical(error$message, "Wrong data format for export (must be imMetrics or imExport)",
                     "signals data should not be exported")
})

outputDirectory <- "outputDirectoryPath"
fileName <- "export.csv"
expectedData <- fread("../data/exportData.csv", skip = 2)

mockedCreateExport <- function(study, data, outputDirectory, fileName, expectedData, expectedMetadata, expectedfilePath,
                               expectCall, metadata = NULL) {

    writeLines_Stub <- mock()
    fwrite_Stub <- mock()
    dir.create_Stub <- mock()

    mockr::with_mock(
        fwrite = fwrite_Stub,
        writeLines = writeLines_Stub,
        dir.create = dir.create_Stub, {
            createExport(study, data, outputDirectory, fileName, metadata)
        }
    )

    expect_args(writeLines_Stub, 1, text = expectedMetadata, con = expectedfilePath, useBytes = TRUE)
    expect_args(fwrite_Stub, 1, x = expectedData, file = expectedfilePath, append = TRUE, col.names = TRUE, na = "NA",
                scipen = 999)

    expect_args(dir.create_Stub, 1, path = outputDirectory)

    expect_called(writeLines_Stub, expectCall)
    expect_called(fwrite_Stub, expectCall)
    expect_called(dir.create_Stub, expectCall)
}


test_that("should call writeLines and fwrite with the good parameters", {
    data <- checkDataFormat(data)
    expectedMetadata <- c("\ufeff#METADATA,,,", NULL, "#DATA,,,")
    expectedfilePath <- "outputDirectoryPath/export.csv"

    mockedCreateExport(study, data, outputDirectory, fileName, expectedData, expectedMetadata, expectedfilePath,
                       expectCall = 1)
})


test_that("should not call dir.create, writeLines and fwrite if wrong data format", {
    wrongData <- data.frame("Timestamp" = seq(1:100), "Thresholded value" = rep(0, 100))
    expectedMetadata <- c("\ufeff#METADATA", NULL, "#DATA")
    expectedfilePath <- "outputDirectoryPath/export.csv"

    error <- capture_error(mockedCreateExport(study, wrongData, outputDirectory, fileName, expectedData,
                                              expectedMetadata, expectedfilePath, expectCall = 0))

    expect_identical(error$message, "Wrong data format for export (must be imMetrics or imExport)",
                     "Timestamp column should not be present")
})


test_that("Adding custom metadata should work as expected", {
    data <- checkDataFormat(data)
    expectedMetadata <- readLines("../data/exportMetadata.csv", encoding = "UTF-8")
    expectedfilePath <- "outputDirectoryPath/export.csv"

    mockedCreateExport(study, data, outputDirectory, fileName, expectedData, expectedMetadata, expectedfilePath,
                       expectCall = 1, metadata = additionalMetadata)

    # More or less metadata rows than data columns should throw a warning and not append metadata
    additionalMetadata <- data.frame("Group" = "", "Units" = "ms")
    expectedMetadata <- c("\ufeff#METADATA", NULL, "#DATA")
    warning <- capture_warning(mockedCreateExport(study, data, outputDirectory, fileName, expectedData,
                                                  expectedMetadata, expectedfilePath, expectCall = 0,
                                                  metadata = additionalMetadata))

    expect_identical(warning$message, "Wrong additional metadata format - ignoring it...", "should throw a warning")

})
