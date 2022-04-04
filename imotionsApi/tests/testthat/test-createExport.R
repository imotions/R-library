library("imotionsApi");
library("stubthat");

context("createExport()");

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

    writeLines_Stub <- stub(writeLines)
    writeLines_Stub$expects(text = expectedMetadata, con = expectedfilePath)
    fwrite_Stub <- stub(fwrite)
    fwrite_Stub$expects(x = expectedData, file = expectedfilePath)
    dir.create_Stub <- stub(dir.create)
    dir.create_Stub$expects(path = outputDirectory)

    mockr::with_mock(
        fwrite = fwrite_Stub$f,
        writeLines = writeLines_Stub$f,
        dir.create = dir.create_Stub$f,
        {
            createExport(study, data, outputDirectory, fileName, metadata)
        }
    )

    expect_equal(dir.create_Stub$calledTimes(), expectCall, info = "dir.create() wrong number of call")
    expect_equal(writeLines_Stub$calledTimes(), expectCall, info = "writeLines() wrong number of call")
    expect_equal(fwrite_Stub$calledTimes(), expectCall, info = "fwrite() wrong number of call")
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
                                              expectedMetadata, expectedfilePath, expectCall = 0)
    )

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
