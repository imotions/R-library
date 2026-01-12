# getAoiRespondentMetrics =============================================================================================
context("getAoiRespondentMetrics()")

library(mockery)

# Load study
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))
study_cloud <- jsonlite::unserializeJSON(readLines("../data/imStudy_cloud.json"))
study_cloud_local <- jsonlite::unserializeJSON(readLines("../data/imStudy_cloud_local.json"))

# Load AOI
AOI <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imAOI.json")))
AOI_cloud <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imAOI_cloud.json")))
AOIDetailsRespondentPath <- "../data/AOIDetailsRespondent.json"
AOIDetailsFile <- jsonlite::fromJSON(AOIDetailsRespondentPath)
metrics_output <- fread("../data/AOImetrics.csv")

# Load respondent
respondent <- getRespondents(study)[1, ]

mockedGetAoiRespondentMetrics <- function(study, AOI, respondent, AOIDetailsFile = NULL, expectedFilePath = NULL,
                                          expectCallsDetails = 0, expectCallsFread = 0, fail = FALSE) {
    privateGetAoiDetails_Stub <- mock(AOIDetailsFile)
    file.exists_Stub <- mock(!fail)
    fread_Stub <- mock(metrics_output)

    metrics <- mockr::with_mock(privateGetAoiDetails = privateGetAoiDetails_Stub,
                                file.exists = file.exists_Stub,
                                fread = fread_Stub, {
                                    getAoiRespondentMetrics(study, AOI, respondent)
                                })

    expect_called(privateGetAoiDetails_Stub, expectCallsDetails)

    if (expectCallsDetails > 0) {
        expect_args(privateGetAoiDetails_Stub, 1, study = study, imObject = AOI, respondent = respondent)
    }

    expect_called(fread_Stub, expectCallsFread)

    if (expectCallsFread > 0) {
        expect_args(fread_Stub, 1, expectedFilePath)
    }

    return(metrics)
}

test_that("error - arguments are missing or not from the good class", {
    # in case of missing study
    expect_error(getAoiRespondentMetrics(), "Please specify a study loaded with `imStudy()`", fixed = TRUE,
                 info = "missing `study` param not handled properly")

    # in case of missing AOI
    expect_error(getAoiRespondentMetrics(study), "Please specify an AOI loaded with `getAois()`", fixed = TRUE,
                 info = "missing `AOI` param not handled properly")

    # in case of missing respondent
    expect_error(getAoiRespondentMetrics(study, AOI), "Please specify a respondent loaded with `getRespondents()`",
                 fixed = TRUE, info = "missing `respondent` param not handled properly")

    # in case of study that is not an imStudy object
    expect_error(getAoiRespondentMetrics(study = "whatever", AOI, respondent),
                 "`study` argument is not an imStudy object",
                 info = "study not being an imStudy object should throw an error")

    # in case of AOI that is not an imAOI object
    expect_error(getAoiRespondentMetrics(study, AOI = "whatever", respondent), "`AOI` argument is not an imAOI object",
                 info = "AOI not being an imAOI object should throw an error")

    # in case of respondent that is not an imRespondent object
    expect_error(getAoiRespondentMetrics(study, AOI, respondent = "whatever"),
                 "`respondent` argument is not an imRespondent object",
                 info = "respondent not being an imRespondent object should throw an error")
})

test_that("warning - AOI has not been defined for this respondent", {
    AOIDetailsFile <- jsonlite::fromJSON("../data/no_scenes_annotations_aoidetails.json")
    expect_warning(metrics <- mockedGetAoiRespondentMetrics(study, AOI, respondent, AOIDetailsFile,
                                                            expectCallsDetails = 1),
                   "AOI New Aoi was not found for respondent Wendy",
                   info = "no AOI defined for this respondent should throw a warning")

    expect_null(metrics, info = "result should be null")
})

test_that("warning - no metrics have been found for this respondent", {
    expect_warning(metrics <- mockedGetAoiRespondentMetrics(study, AOI, respondent, AOIDetailsFile,
                                                            expectCallsDetails = 1),
                   "No metrics found for AOI: New Aoi, Respondent: Wendy",
                   info = "no metrics found should throw an warning")

    expect_null(metrics, "result should be null")
})

# Modify AOIDetailsFile so it fit test data
AOIDetailsFile$resultId <- "../data/AOImetrics.csv"

test_that("local return - metrics for this AOI/respondent pair", {
    metrics <- mockedGetAoiRespondentMetrics(study, AOI, respondent, AOIDetailsFile, expectCallsDetails = 1,
                                             expectCallsFread = 1, expectedFilePath = "../data/AOImetrics.csv")

    # Check dimensions and class of metrics
    expect_equal(nrow(metrics), 1, infos = "metrics should always only have one row")
    expect_equal(ncol(metrics), 37, infos = "no column should be lost")
    expect_s3_class(metrics, "imAOIMetrics")
})

test_that("remote warning - in case no local path is set, should send a warning and return NULL", {
    expect_warning(metrics <- mockedGetAoiRespondentMetrics(study_cloud, AOI_cloud, respondent),
                   "No localPath set when calling imConnection(), not possible to read metrics locally.",
                   fixed = TRUE, info = "no local path should throw an warning")

    expect_null(metrics, "result should be null")
})

test_that("remote warning - in case no metrics path found", {
    expect_warning(metrics <- mockedGetAoiRespondentMetrics(study_cloud_local, AOI_cloud, respondent, fail = TRUE),
                   "No metrics found for AOI: El Manuel Area, Respondent: Wendy",
                   info = "no metrics found should throw an warning")

    expect_null(metrics, "result should be null")
})

test_that("remote return - metrics for this AOI/respondent pair", {
    expectedDirectory <- "myLocalPath/93fbaae0-8b6f-45b6-b5dd-9a5d4216d7fd/dd8a9342-f5c0-4a02-bf27-de68fc13f2bc"
    expectedFilepath <- paste0(expectedDirectory, "/", respondent$id, "metrics.csv")

    metrics <- mockedGetAoiRespondentMetrics(study_cloud_local, AOI_cloud, respondent, expectCallsFread = 1,
                                             expectedFilePath = expectedFilepath)

    # Check dimensions and class of metrics
    expect_equal(nrow(metrics), 1, infos = "metrics should always only have one row")
    expect_equal(ncol(metrics), 37, infos = "no column should be lost")
    expect_s3_class(metrics, "imAOIMetrics")
})
