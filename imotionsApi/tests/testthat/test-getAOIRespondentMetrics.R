# privateGetAoiRespondentMetricsPath ==================================================================================
context("privateGetAoiRespondentMetricsPath()")

library(mockery)

# Load study
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))
study_cloud <- jsonlite::unserializeJSON(readLines("../data/imStudy_cloud.json"))
study_cloud_local <- jsonlite::unserializeJSON(readLines("../data/imStudy_cloud_local.json"))

# Load AOI
AOI <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imAOI.json")))
AOI_cloud <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imAOI_cloud.json")))
AOIDetailsRespondentPath <- "../data/AOIDetailsRespondent.json"
AOIDetails <- jsonlite::fromJSON(AOIDetailsRespondentPath)
metrics_output <- fread("../data/AOImetrics.csv")

# Load respondent
respondent <- getRespondents(study)[1, ]

expectedTmpPath <- paste0("myLocalPath/93fbaae0-8b6f-45b6-b5dd-9a5d4216d7fd/dd8a9342-f5c0-4a02-bf27-de68fc13f2bc/",
                          respondent$id, "metrics.csv")

test_that("remote check - should return the local AOI respondent metrics path", {
    expect_identical(privateGetAoiRespondentMetricsPath(study_cloud_local, AOI_cloud, respondent), expectedTmpPath)
})

# getAoiRespondentMetrics =============================================================================================
context("getAoiRespondentMetrics()")

mockGetAoiRespondentMetrics <- function(study, AOI, respondent, AOIDetailsFile = NULL, stats = NULL,
                                        expectedFilePath = NULL, fileExists = FALSE, expectCallsDetails = 0,
                                        expectCallsFileExists = 0, expectCallsFread = 0, expectCallsGetJSON = 0,
                                        expectCallsGetFile = 0) {

    privateGetAoiDetails_Stub <- mock(AOIDetailsFile)
    file.exists_Stub <- mock(fileExists)
    fread_Stub <- mock(metrics_output)
    getJSON_Stub <- mock(stats)
    getFile_Stub <- mock(list(file_path = expectedFilePath))

    metrics <- mockr::with_mock(privateGetAoiDetails = privateGetAoiDetails_Stub,
                                file.exists = file.exists_Stub,
                                getJSON = getJSON_Stub,
                                getFile = getFile_Stub,
                                fread = fread_Stub, {
                                    getAoiRespondentMetrics(study, AOI, respondent)
                                })

    expect_called(privateGetAoiDetails_Stub, expectCallsDetails)
    expect_called(file.exists_Stub, expectCallsFileExists)
    expect_called(getJSON_Stub, expectCallsGetJSON)
    expect_called(getFile_Stub, expectCallsGetFile)
    expect_called(fread_Stub, expectCallsFread)

    if (expectCallsDetails > 0) {
        expect_args(privateGetAoiDetails_Stub, 1, study = study, imObject = AOI, respondent = respondent)
    }

    if (!study$connection$localIM) {
        endpoint <- paste0("AOI: ", AOI$name, ", Respondent: ", respondent$name)

        if (expectCallsFileExists > 0) {
            expect_args(file.exists_Stub, 1, expectedFilePath)
        }

        if (expectCallsGetJSON > 0) {
            expect_args(getJSON_Stub, 1, study$connection, getAoiMetricsUrl(study, respondent, AOI),
                        message = paste("Retrieving AOI respondent metrics for", endpoint))
        }

        if (expectCallsGetFile > 0) {
            localFilePath <- if (!is.null(study$connection$localPath)) expectedFilePath else NULL
            expect_args(getFile_Stub, 1, study$connection, stats$aoiRespondentStatsUrl,
                        message = paste("Downloading AOI respondent metrics for", endpoint),
                        localFilePath = localFilePath)
        }
    }

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

test_that("local warning - AOI has not been defined for this respondent", {
    AOIDetails <- jsonlite::fromJSON("../data/no_scenes_annotations_aoidetails.json")
    expect_warning(metrics <- mockGetAoiRespondentMetrics(study, AOI, respondent, AOIDetails, expectCallsDetails = 1),
                   "AOI New Aoi was not found for respondent Wendy",
                   info = "no AOI defined for this respondent should throw a warning")

    expect_null(metrics, info = "result should be null")
})

test_that("local warning - no metrics have been found for this respondent", {
    expect_warning(metrics <- mockGetAoiRespondentMetrics(study, AOI, respondent, AOIDetails, expectCallsDetails = 1),
                   "No metrics found for AOI: New Aoi, Respondent: Wendy",
                   info = "no metrics found should throw an warning")

    expect_null(metrics, "result should be null")
})

# Modify AOIDetails so it fit test data
AOIDetails$resultId <- "../data/AOImetrics.csv"

test_that("local check - should call privateGetAoiDetails and fread for a specific respondent", {
    metrics <- mockGetAoiRespondentMetrics(study, AOI, respondent, AOIDetails,
                                           expectedFilePath = "../data/AOImetrics.csv", expectCallsDetails = 1,
                                           expectCallsFread = 1)

    # Check dimensions and class of metrics
    expect_equal(nrow(metrics), 1, infos = "metrics should always only have one row")
    expect_equal(ncol(metrics), 37, infos = "no column should be lost")
    expect_s3_class(metrics, "imAOIMetrics")
})

test_that("remote warning - no AOI respondent stats have been returned", {
    expect_warning(metrics <- mockGetAoiRespondentMetrics(study_cloud_local, AOI_cloud, respondent, fileExists = FALSE,
                                                          expectedFilePath = expectedTmpPath, expectCallsFileExists = 1,
                                                          expectCallsGetJSON = 1),
                   "No metrics found for AOI: El Manuel Area, Respondent: Wendy",
                   info = "no metrics found should throw an warning")

    expect_null(metrics, "result should be null")
})

test_that("remote warning - no metrics have been uploaded for this respondent", {
    expect_warning(metrics <- mockGetAoiRespondentMetrics(study_cloud_local, AOI_cloud, respondent,
                                                          stats = stop("Resource not found"),
                                                          expectedFilePath = expectedTmpPath,
                                                          expectCallsFileExists = 1, expectCallsGetJSON = 1),
                   "No metrics found for AOI: El Manuel Area, Respondent: Wendy",
                   fixed = TRUE
    )

    expect_null(metrics)
})

test_that("remote error - getJSON errors other than resource not found should be rethrown", {
    expect_error(mockGetAoiRespondentMetrics(study_cloud_local, AOI_cloud, respondent, stats = stop("Server error"),
                                             expectedFilePath = expectedTmpPath, expectCallsFileExists = 1,
                                             expectCallsGetJSON = 1),
                 "Server error", fixed = TRUE)
})

test_that("remote check - should call getJSON, getFile and fread when no local path is set", {
    stats <- list(aoiRespondentStatsUrl = "https://s3.test/respondent-aoi-metrics.csv")

    metrics <- mockGetAoiRespondentMetrics(study_cloud, AOI_cloud, respondent, stats = stats,
                                           expectedFilePath = "../data/AOImetrics.csv", expectCallsFread = 1,
                                           expectCallsGetJSON = 1, expectCallsGetFile = 1)

    # Check dimensions and class of metrics
    expect_equal(nrow(metrics), 1, infos = "metrics should always only have one row")
    expect_equal(ncol(metrics), 37, infos = "no column should be lost")
    expect_s3_class(metrics, "imAOIMetrics")
})



test_that("remote check - should only call fread when metrics are cached locally", {
    metrics <- mockGetAoiRespondentMetrics(study_cloud_local, AOI_cloud, respondent, expectedFilePath = expectedTmpPath,
                                           fileExists = TRUE, expectCallsFileExists = 1, expectCallsFread = 1)

    expect_equal(nrow(metrics), 1, infos = "metrics should always only have one row")
    expect_equal(ncol(metrics), 37, infos = "no column should be lost")
    expect_s3_class(metrics, "imAOIMetrics")
})

test_that("remote check - should call getJSON, getFile and fread when metrics are not cached locally", {
    stats <- list(aoiRespondentStatsUrl = "https://s3.test/respondent-aoi-metrics.csv")

    metrics <- mockGetAoiRespondentMetrics(study_cloud_local, AOI_cloud, respondent, stats = stats,
                                           expectedFilePath = expectedTmpPath, expectCallsFileExists = 1,
                                           expectCallsFread = 1, expectCallsGetJSON = 1, expectCallsGetFile = 1)

    expect_equal(nrow(metrics), 1, infos = "metrics should always only have one row")
    expect_equal(ncol(metrics), 37, infos = "no column should be lost")
    expect_s3_class(metrics, "imAOIMetrics")
})
