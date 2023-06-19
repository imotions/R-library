# uploadAOIMetrics ==========================================================================================
context("uploadAOIMetrics()")

library(mockery)

# Load study, respondent and AOI
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))
study_cloud <- jsonlite::unserializeJSON(readLines("../data/imStudy_cloud.json"))

respondent <- getRespondent(study, "09bd22e6-29b6-4a8a-8cc1-4780a5163e63")
AOI <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imAOI.json")))
AOI_cloud <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imAOI_cloud.json")))

AOIDetailsRespondentPath <- "../data/AOIDetailsRespondent.json"
AOIDetailsFile <- jsonlite::fromJSON(AOIDetailsRespondentPath)

# Create metrics to upload
metrics <- data.frame("metric1" = 2, "metric2" = 234, "metric3" = 1234)


test_that("error - arguments are missing or not from the good class", {
    # in case of missing study
    expect_error(uploadAOIMetrics(), "Please specify a study loaded with `imStudy()`", fixed = TRUE,
                 info = "missing `study` param not handled properly")

    # in case of missing AOI
    expect_error(uploadAOIMetrics(study), "Please specify an AOI loaded with `getAOIs()`", fixed = TRUE,
                 info = "missing `AOI` param not handled properly")

    # in case of missing respondent
    expect_error(uploadAOIMetrics(study, AOI),
                 "Please specify a target respondent/segment loaded with `getRespondents()` or `getSegments()`",
                 fixed = TRUE, info = "missing `target` param not handled properly")

    # in case of missing metrics
    expect_error(uploadAOIMetrics(study, AOI, respondent), "Please specify a data.table with metrics to upload",
                 info = "missing `metrics` param not handled properly")

    # in case of study that is not an imStudy object
    expect_error(uploadAOIMetrics(study = "whatever", AOI, respondent, metrics),
                 "`study` argument is not an imStudy object",
                 info = "study not being an imStudy object should throw an error")

    # in case of AOI that is not an imAOI object
    expect_error(uploadAOIMetrics(study, AOI = "whatever", respondent, metrics),
                 "`AOI` argument is not an imAOI object",
                 info = "AOI not being an imAOI object should throw an error")

    # in case of respondent that is not an imRespondent object
    expect_error(uploadAOIMetrics(study, AOI, target = "whatever", metrics),
                 "`target` argument is not an imRespondent or imSegment object",
                 info = "respondent not being an imRespondent object should throw an error")

    # in case of wrong metrics format
    wrongData <- data.table(Timestamp = integer(), variableTest = numeric())
    expect_error(uploadAOIMetrics(study, AOI, respondent, wrongData), "Do not upload an empty dataset",
                 info = "zero row dataset should not be uploaded")
})

mockLocalUploadAOIMetrics <- function(study, AOI, respondent, metrics, AOIDetailsFile, filepath = NULL,
                                      expectCallDetails = 0, expectCallsWritecsv = 0) {

    privateGetAOIDetails_Stub <- mock(AOIDetailsFile)
    writecsv_Stub <- mock()

    mockr::with_mock(
        privateGetAOIDetails = privateGetAOIDetails_Stub,
        write.csv = writecsv_Stub, {
            uploadAOIMetrics(study, AOI, respondent, metrics)
    })

    expect_called(privateGetAOIDetails_Stub, expectCallDetails)
    expect_called(writecsv_Stub, expectCallsWritecsv)

    if (expectCallDetails > 0) {
        expect_args(privateGetAOIDetails_Stub, 1, study = study, imObject = AOI, respondent = respondent)
    }

    if (expectCallsWritecsv > 0) {
        expect_args(writecsv_Stub, 1, x = metrics, file = filepath, col.names = TRUE, row.names = FALSE)
    }
}

test_that("local warning - AOI is not found for a specific respondent", {
    metrics <- checkDataFormat(metrics)
    AOIDetailsFile <- jsonlite::fromJSON("../data/no_scenes_annotations_aoidetails.json")

    expect_warning(mockLocalUploadAOIMetrics(study, AOI, respondent, metrics, AOIDetailsFile, expectCallDetails = 1),
                   "AOI New Aoi was not found for respondent Wendy",
                   info = "no AOI defined for this respondent should throw an error")
})

test_that("local check - should not call privateGetAOIDetails and write.csv if metrics is of wrong format", {
    wrongData <- data.frame("Timestamp" = seq(1:100), "Thresholded value" = rep(0, 100))

    expect_warning(mockLocalUploadAOIMetrics(study, AOI, respondent, wrongData, AOIDetailsFile),
                   "AOI metrics should be a data.frame/data.table composed of only one row",
                   info = "wrong metrics should give a warning")
})

test_that("local check - should call privateGetAOIDetails and write.csv if metrics are of good format", {
    metrics <- checkDataFormat(metrics)
    filepath <- paste0(tools::file_path_sans_ext(AOIDetailsFile$fileId), "metrics.csv")

    mockLocalUploadAOIMetrics(study, AOI, respondent, metrics, AOIDetailsFile, filepath, expectCallDetails = 1,
                              expectCallsWritecsv = 1)
})

segment <- getSegments(study_cloud)

mockRemoteUploadAOIMetrics <- function(study, AOI, segment, metrics, expectedUrl = NULL, expectedEndpoint = NULL,
                                       expectedBody = NULL, expectCalls = 0) {

    getUploadAoiMetricsUrl_Stub <- mock(expectedUrl)
    putHttr_Stub <- mock()

    mockr::with_mock(
        getUploadAoiMetricsUrl = getUploadAoiMetricsUrl_Stub,
        putHttr = putHttr_Stub, {
            uploadAOIMetrics(study, AOI, segment, metrics)
        })

    expect_called(getUploadAoiMetricsUrl_Stub, expectCalls)
    expect_called(putHttr_Stub, expectCalls)

    if (expectCalls > 0) {
        expect_args(getUploadAoiMetricsUrl_Stub, 1, study, segment, AOI)
        expect_args(putHttr_Stub, 1, study$connection, expectedUrl, expectedBody, expectedEndpoint)
    }
}


test_that("remote check - should not call getUploadAoiMetricsUrl and putHttr if metrics is of wrong format", {
    wrongData <- data.frame("Timestamp" = seq(1:100), "Thresholded value" = rep(0, 100))

    expect_warning(mockRemoteUploadAOIMetrics(study_cloud, AOI_cloud, segment, wrongData),
                   "AOI metrics should be a data.frame/data.table composed of only one row",
                   info = "wrong metrics format not handled correctly")
})


test_that("remote check - should call getUploadAoiMetricsUrl and putHttr if metrics are of good format", {
    metrics <- checkDataFormat(metrics)
    expectedUrl <- getUploadAoiMetricsUrl(study_cloud, segment, AOI_cloud)
    expectedEndpoint <- "Updating AOI metrics for segment: All Respondents, AOI: El Manuel Area"
    expectedBody <- toJSON(fromJSON("../data/AOImetrics_cloud.json"))

    mockRemoteUploadAOIMetrics(study_cloud, AOI_cloud, segment, metrics, expectedUrl, expectedEndpoint, expectedBody,
                               expectCalls = 1)
})


# uploadAOIMetadata ==========================================================================================
context("uploadAOIMetadata()")

test_that("error - arguments are missing or not from the good class", {
    # in case of missing study
    expect_error(uploadAOIMetadata(), "Please specify a study loaded with `imStudy()`", fixed = TRUE,
                 info = "missing `study` param not handled properly")

    # in case of missing metadata
    expect_error(uploadAOIMetadata(study), "Please specify a data.table with metadata to upload",
                 info = "missing `metadata` param not handled properly")

    # in case of study that is not an imStudy object
    expect_error(uploadAOIMetadata(study = "whatever", metadata),
                 "`study` argument is not an imStudy object",
                 info = "study not being an imStudy object should throw an error")

    # in case of a metadata missing the Group and Group description column
    metadata <- data.table("Units" = c("ms", "", "s"), "Show" = c("FALSE", "TRUE", "TRUE"))

    expect_error(uploadAOIMetadata(study_cloud, metadata),
                 "Please provide a `Group` and `Group description` columns in your `metadata` data.table",
                 info = "missing columns should throw an error")
})

mockUploadAOIMetadata <- function(study, metadata, expectedUrl = NULL, expectedEndpoint = NULL,
                                       expectedBody = NULL, expectCalls = 0) {

    getUploadAoiMetadataUrl_Stub <- mock(expectedUrl)
    putHttr_Stub <- mock()

    mockr::with_mock(
        getUploadAoiMetadataUrl = getUploadAoiMetadataUrl_Stub,
        putHttr = putHttr_Stub, {
            uploadAOIMetadata(study, metadata)
        })

    expect_called(getUploadAoiMetadataUrl_Stub, expectCalls)
    expect_called(putHttr_Stub, expectCalls)

    if (expectCalls > 0) {
        expect_args(getUploadAoiMetadataUrl_Stub, 1, study)
        expect_args(putHttr_Stub, 1, study$connection, expectedUrl, expectedBody, expectedEndpoint)
    }
}

metadata <- data.table("Units" = c("ms", "", "s"), "Show" = c("FALSE", "TRUE", "TRUE"),
                       "Group" = c("Group 1", "Group 2", "Group 2"),
                       "Group description" = c("Description 1", "Description 2", "Description 2"))

test_that("remote check - should call getUploadMetadataUrl and putHttr if metadata are of good format", {
    expectedUrl <- getUploadAoiMetadataUrl(study_cloud)
    expectedEndpoint <- "Updating AOI metrics metadata for study: RRRock The R"
    expectedBody <- toJSON(fromJSON("../data/AOImetadata_cloud.json"))

    mockUploadAOIMetadata(study_cloud, metadata, expectedUrl, expectedEndpoint, expectedBody, expectCalls = 1)
})

test_that("local check - should not call getUploadMetadataUrl for a local study", {
    expect_warning(mockUploadAOIMetadata(study, metadata, expectCalls = 0),
                   "Saving of AOI metadata is only available for remote studies.",
                   info = "local studies should throw warning and not do anything")
})
