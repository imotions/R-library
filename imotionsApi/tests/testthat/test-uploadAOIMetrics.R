# privateUploadAoiMetrics ==========================================================================================
context("privateUploadAoiMetrics()")

library(mockery)

# Load study, respondent and AOI
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))
study_cloud <- jsonlite::unserializeJSON(readLines("../data/imStudy_cloud.json"))
study_cloud_local <- jsonlite::unserializeJSON(readLines("../data/imStudy_cloud_local.json"))

respondent <- getRespondent(study, "09bd22e6-29b6-4a8a-8cc1-4780a5163e63")
AOI <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imAOI.json")))
AOI_cloud <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imAOI_cloud.json")))

AOIDetailsFile <- jsonlite::fromJSON("../data/AOIDetailsRespondent.json")

# Create metrics to upload
metrics <- data.frame("metric1" = 2, "metric2" = 234, "metric3" = 1234)

mockPrivateUploadAoiMetrics <- function(study, obj, AOI, metrics, AOIDetailsFile = NULL, expectedFilepath = NULL,
                                        expectedDirectory = NULL, expectedEndpoint = NULL, expectedBody = NULL,
                                        expectCallDetails = 0, expectCallsfwrite = 0, expectCallsDir = 0,
                                        expectCallsPut = 0) {

    privateGetAoiDetails_Stub <- mock(AOIDetailsFile)
    fwrite_Stub <- mock()
    dir.create_Stub <- mock()
    getUploadAoiMetricsUrl_Stub <- mock("myurl")
    putHttr_Stub <- mock()

    mockr::with_mock(privateGetAoiDetails = privateGetAoiDetails_Stub,
                     fwrite = fwrite_Stub,
                     dir.create = dir.create_Stub,
                     getUploadAoiMetricsUrl = getUploadAoiMetricsUrl_Stub,
                     putHttr = putHttr_Stub, {
                         privateUploadAoiMetrics(study, obj, AOI, metrics)
                     })

    expect_called(privateGetAoiDetails_Stub, expectCallDetails)
    expect_called(fwrite_Stub, expectCallsfwrite)
    expect_called(dir.create_Stub, expectCallsDir)
    expect_called(getUploadAoiMetricsUrl_Stub, expectCallsPut)
    expect_called(putHttr_Stub, expectCallsPut)

    if (expectCallDetails > 0) {
        expect_args(privateGetAoiDetails_Stub, 1, study = study, imObject = AOI, respondent = obj)
    }

    if (expectCallsfwrite > 0) {
        expect_args(fwrite_Stub, 1, x = metrics, file = expectedFilepath, col.names = TRUE, row.names = FALSE)
    }

    if (expectCallsDir > 0) {
        expect_args(dir.create_Stub, 1, expectedDirectory, showWarnings = FALSE, recursive = TRUE)
    }

    if (expectCallsPut > 0) {
        expect_args(getUploadAoiMetricsUrl_Stub, 1, study, obj, AOI)
        expect_args(putHttr_Stub, 1, study$connection, "myurl", expectedBody, expectedEndpoint)
    }
}

test_that("warning - AOI is not found for a specific respondent", {
    AOIDetailsFile <- jsonlite::fromJSON("../data/no_scenes_annotations_aoidetails.json")

    expect_warning(mockPrivateUploadAoiMetrics(study, respondent, AOI, metrics, AOIDetailsFile, expectCallDetails = 1),
                   "AOI New Aoi was not found for respondent Wendy",
                   info = "no AOI defined for this respondent should throw a warning")
})

test_that("local check - should call privateGetAoiDetails and fwrite for a specific respondent", {
    expectedFilepath <- paste0(tools::file_path_sans_ext(AOIDetailsFile$fileId), "metrics.csv")

    mockPrivateUploadAoiMetrics(study, respondent, AOI, metrics, AOIDetailsFile, expectedFilepath,
                                expectCallDetails = 1, expectCallsfwrite = 1)
})

test_that("local check - a touch AOI dispatches to privateGetTouchActorDetails rather than privateGetAoiDetails", {
    touchAoi <- AOI
    class(touchAoi) <- c("imTouchAOI", class(touchAoi))

    expectedFilepath <- paste0(tools::file_path_sans_ext(AOIDetailsFile$fileId), "metrics.csv")

    privateGetAoiDetails_Stub <- mock()
    privateGetTouchActorDetails_Stub <- mock(AOIDetailsFile)
    fwrite_Stub <- mock()

    mockr::with_mock(privateGetAoiDetails = privateGetAoiDetails_Stub,
                     privateGetTouchActorDetails = privateGetTouchActorDetails_Stub,
                     fwrite = fwrite_Stub, {
                         privateUploadAoiMetrics(study, respondent, touchAoi, metrics)
                     })

    expect_called(privateGetAoiDetails_Stub, 0)
    expect_called(privateGetTouchActorDetails_Stub, 1)
    expect_args(privateGetTouchActorDetails_Stub, 1, study = study, imObject = touchAoi, respondent = respondent)

    expect_called(fwrite_Stub, 1)
    expect_args(fwrite_Stub, 1, x = metrics, file = expectedFilepath, col.names = TRUE, row.names = FALSE)
})

test_that("remote warning - should throw a warning and exit if no local path was set for a specific respondent", {
    expect_warning(mockPrivateUploadAoiMetrics(study_cloud, respondent, AOI_cloud, metrics, AOIDetailsFile,
                                               expectCallDetails = 1), fixed = TRUE,
                   "Please set a localPath when calling imConnection() to write back metrics locally.",
                   info = "no local path should throw a warning")

})

segment <- getSegments(study_cloud)

test_that("remote check - should call privateGetAoiDetails, dir.create and fwrite for a specific respondent", {
    expectedDirectory <- "myLocalPath/93fbaae0-8b6f-45b6-b5dd-9a5d4216d7fd/dd8a9342-f5c0-4a02-bf27-de68fc13f2bc"
    expectedFilepath <- paste0(expectedDirectory, "/", respondent$id, "metrics.csv")

    mockPrivateUploadAoiMetrics(study_cloud_local, respondent, AOI_cloud, metrics, AOIDetailsFile, expectedFilepath,
                                expectedDirectory, expectCallDetails = 1, expectCallsfwrite = 1, expectCallsDir = 1)
})

test_that("remote check - should call getUploadAoiMetricsUrl and putHttr for a specific segment", {
    expectedEndpoint <- "Updating AOI metrics for segment: All Respondents, AOI: El Manuel Area"
    expectedBody <- toJSON(fromJSON("../data/AOImetrics_cloud.json"))

    mockPrivateUploadAoiMetrics(study_cloud, segment, AOI_cloud, metrics, expectedUrl,
                                expectedEndpoint = expectedEndpoint, expectedBody = expectedBody, expectCallsPut = 1)
})

# uploadAoiMetrics ==========================================================================================
context("uploadAoiMetrics()")

test_that("error - arguments are missing or not from the good class", {
    # in case of missing study
    expect_error(uploadAoiMetrics(), "Please specify a study loaded with `imStudy()`", fixed = TRUE,
                 info = "missing `study` param not handled properly")

    # in case of missing AOI
    expect_error(uploadAoiMetrics(study), "Please specify an AOI loaded with `getAois()`", fixed = TRUE,
                 info = "missing `AOI` param not handled properly")

    # in case of missing respondent
    expect_error(uploadAoiMetrics(study, AOI),
                 "Please specify a target respondent/segment loaded with `getRespondents()` or `getSegments()`",
                 fixed = TRUE, info = "missing `target` param not handled properly")

    # in case of missing metrics
    expect_error(uploadAoiMetrics(study, AOI, respondent), "Please specify a data.table with metrics to upload",
                 info = "missing `metrics` param not handled properly")

    # in case of study that is not an imStudy object
    expect_error(uploadAoiMetrics(study = "whatever", AOI, respondent, metrics),
                 "`study` argument is not an imStudy object",
                 info = "study not being an imStudy object should throw an error")

    # in case of AOI that is not an imAOI object
    expect_error(uploadAoiMetrics(study, AOI = "whatever", respondent, metrics),
                 "`AOI` argument is not an imAOI object",
                 info = "AOI not being an imAOI object should throw an error")

    # in case of respondent that is not an imRespondent object
    expect_error(uploadAoiMetrics(study, AOI, target = "whatever", metrics),
                 "`target` argument is not an imRespondent or imSegment object",
                 info = "respondent not being an imRespondent object should throw an error")

    # in case of wrong metrics format
    wrongData <- data.table(Timestamp = integer(), variableTest = numeric())
    expect_error(uploadAoiMetrics(study, AOI, respondent, wrongData), "Do not upload an empty dataset",
                 info = "zero row dataset should not be uploaded")
})

test_that("check - should not call privateUploadAoiMetrics if metrics are of wrong format", {
    privateUploadAoiMetrics_Stub <- mock()
    wrongData <- data.frame("Timestamp" = seq(1:100), "Thresholded value" = rep(0, 100))

    expect_warning(
        mockr::with_mock(privateUploadAoiMetrics = privateUploadAoiMetrics_Stub, {
            uploadAoiMetrics(study, AOI, respondent, wrongData)
        }),
        "AOI metrics should be a data.frame/data.table composed of only one row",
        info = "wrong metrics should give a warning"
    )

    expect_called(privateUploadAoiMetrics_Stub, 0)
})

test_that("check - should call privateUploadAoiMetrics if metrics are of good format", {
    metrics <- checkDataFormat(metrics)
    privateUploadAoiMetrics_Stub <- mock()

    mockr::with_mock(privateUploadAoiMetrics = privateUploadAoiMetrics_Stub, {
        uploadAoiMetrics(study, AOI, respondent, metrics)
    })

    expect_called(privateUploadAoiMetrics_Stub, 1)
    expect_args(privateUploadAoiMetrics_Stub, 1, study, respondent, AOI, metrics)
})


# uploadAoiMetadata ==========================================================================================
context("uploadAoiMetadata()")

test_that("error - arguments are missing or not from the good class", {
    # in case of missing study
    expect_error(uploadAoiMetadata(), "Please specify a study loaded with `imStudy()`", fixed = TRUE,
                 info = "missing `study` param not handled properly")

    # in case of missing metadata
    expect_error(uploadAoiMetadata(study), "Please specify a data.table with metadata to upload",
                 info = "missing `metadata` param not handled properly")

    # in case of study that is not an imStudy object
    expect_error(uploadAoiMetadata(study = "whatever", metadata),
                 "`study` argument is not an imStudy object",
                 info = "study not being an imStudy object should throw an error")

    # in case of a metadata missing the Group and Group description column
    metadata <- data.table("Units" = c("ms", "", "s"), "Show" = c("FALSE", "TRUE", "TRUE"))

    expect_error(uploadAoiMetadata(study_cloud, metadata),
                 "Please provide a `Group` and `Group description` columns in your `metadata` data.table",
                 info = "missing columns should throw an error")
})

mockUploadAoiMetadata <- function(study, metadata, expectedUrl = NULL, expectedEndpoint = NULL,
                                  expectedBody = NULL, expectCalls = 0) {

    getUploadAoiMetadataUrl_Stub <- mock(expectedUrl)
    putHttr_Stub <- mock()

    mockr::with_mock(getUploadAoiMetadataUrl = getUploadAoiMetadataUrl_Stub, putHttr = putHttr_Stub, {
        uploadAoiMetadata(study, metadata)
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

    mockUploadAoiMetadata(study_cloud, metadata, expectedUrl, expectedEndpoint, expectedBody, expectCalls = 1)
})

test_that("local check - should not call getUploadMetadataUrl for a local study", {
    expect_warning(mockUploadAoiMetadata(study, metadata, expectCalls = 0),
                   "Saving of AOI metadata is only available for remote studies.",
                   info = "local studies should throw warning and not do anything")
})
