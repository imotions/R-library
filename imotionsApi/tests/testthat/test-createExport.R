# createExport ========================================================================================================
context("createExport()")

library(mockery)

# Load study
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))

# Create data to export
data <- data.table("Respondent Name" = "Respondent 1", "Metrics1" = seq(1:100), "Thresholded value" = rep(0, 100),
                   check.names = FALSE)

additionalMetadata <- data.table("Group" = c("", "numeric", "Thresholded"), "Units" = c("", "ms", "binary"))

test_that("error - arguments are missing or not from the good class", {
    # in case of missing study
    expect_error(createExport(), "Please specify a study loaded with `imStudy()`", fixed = TRUE,
                 info = "missing `study` param not handled properly")

    # in case of missing data
    expect_error(createExport(study), "Please specify a data.table to export",
                 info = "missing `data` param not handled properly")

    # in case of missing outputDirectory
    expect_error(createExport(study, data), "Please specify an outputDirectory filepath to export the file",
                 info = "missing `outputDirectory` param not handled properly")

    # in case of missing fileName
    expect_error(createExport(study, data, "outputDirectoryPath"), "Please specify the name of the file to create",
                 info = "missing `filename` param not handled properly")

    # in case of wrong data format
    wrongData <- data.table(Timestamp = rep(2, 2), variableTest = 2)
    expect_error(createExport(study, wrongData, "outputDirectoryPath", "export.csv"),
                 "Wrong data format for export (must be imMetrics or imExport)", fixed = TRUE,
                 info = "signals data should not be exported")
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

    expect_called(writeLines_Stub, expectCall)
    expect_called(fwrite_Stub, expectCall)
    expect_called(dir.create_Stub, expectCall)

    if (expectCall > 0) {
        expect_args(writeLines_Stub, 1, text = expectedMetadata, con = expectedfilePath, useBytes = TRUE)
        expect_args(fwrite_Stub, 1, x = expectedData, file = expectedfilePath, append = TRUE, col.names = TRUE,
                    na = "NA", scipen = 999)

        expect_args(dir.create_Stub, 1, path = outputDirectory)
    }
}


test_that("check - work with good data format", {
    # should call writeLines and fwrite with the good parameters
    data <- checkDataFormat(data)
    expectedMetadata <- c("\ufeff#METADATA,,,", NULL, "#DATA,,,")
    expectedfilePath <- "outputDirectoryPath/export.csv"

    mockedCreateExport(study, data, outputDirectory, fileName, expectedData, expectedMetadata, expectedfilePath,
                       expectCall = 1)
})


test_that("error - wrong data format", {
    # should not call dir.create, writeLines and fwrite if wrong data format
    wrongData <- data.frame("Timestamp" = seq(1:100), "Thresholded value" = rep(0, 100))
    expectedMetadata <- c("\ufeff#METADATA", NULL, "#DATA")
    expectedfilePath <- "outputDirectoryPath/export.csv"

    expect_error(mockedCreateExport(study, wrongData, outputDirectory, fileName, expectedData, expectedMetadata,
                                    expectedfilePath, expectCall = 0),
                 "Wrong data format for export (must be imMetrics or imExport)", fixed = TRUE,
                 info = "Timestamp column should not be present")
})


test_that("check - work with custom metadata", {
    data <- checkDataFormat(data)
    expectedMetadata <- c("\ufeff#METADATA,,,", "#Group,,numeric,Thresholded", "#Units,,ms,binary", "#DATA,,,")
    expectedfilePath <- "outputDirectoryPath/export.csv"

    mockedCreateExport(study, data, outputDirectory, fileName, expectedData, expectedMetadata, expectedfilePath,
                       expectCall = 1, metadata = additionalMetadata)

    # More or less metadata rows than data columns should throw a warning and not append metadata
    additionalMetadata <- data.frame("Group" = "", "Units" = "ms")
    expectedMetadata <- c("\ufeff#METADATA,,,", NULL, "#DATA,,,")
    expect_warning(mockedCreateExport(study, data, outputDirectory, fileName, expectedData, expectedMetadata,
                                      expectedfilePath, expectCall = 1, metadata = additionalMetadata),
                   "Wrong additional metadata format - ignoring it...", info = "should throw a warning")
})
