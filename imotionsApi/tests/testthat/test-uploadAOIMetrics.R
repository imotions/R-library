# privateUploadToPresignedUrl =========================================================================================
context("privateUploadToPresignedUrl()")

library(mockery)

# Load study, respondent and AOI
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))
study_cloud <- jsonlite::unserializeJSON(readLines("../data/imStudy_cloud.json"))
study_cloud_local <- jsonlite::unserializeJSON(readLines("../data/imStudy_cloud_local.json"))

test_that("remote check - presigned uploads request should work as expected and upload metrics", {
    uploadUrl <- "myurl"
    fileName <- "metrics.csv"
    postData <- '{"name":"AOI","fileName":"metrics.csv"}'
    fileInfos <- list(presignedUrl = "presignedUrl")
    postJSON_Stub <- mock(fileInfos)
    putHttr_Stub <- mock()

    result <- mockr::with_mock(postJSON = postJSON_Stub,
                               putHttr = putHttr_Stub, {
                                   privateUploadToPresignedUrl(study_cloud$connection, uploadUrl, postData, fileName,
                                                               "Getting upload credentials", "Uploading")
                               })

    expect_identical(result, fileInfos)
    expect_args(postJSON_Stub, 1, study_cloud$connection, uploadUrl, postData, message = "Getting upload credentials")
    expect_args(putHttr_Stub, 1, study_cloud$connection, fileInfos$presignedUrl, fileName, message = "Uploading")
})

# privateUploadAoiMetrics ==========================================================================================
context("privateUploadAoiMetrics()")

respondent <- getRespondent(study, "09bd22e6-29b6-4a8a-8cc1-4780a5163e63")
respondent_cloud <- getRespondent(study_cloud, "6c74637b-6250-4d13-bbbc-c999d9c8a74b")
AOI <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imAOI.json")))
AOI_cloud <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imAOI_cloud.json")))

AOIDetailsFile <- jsonlite::fromJSON("../data/AOIDetailsRespondent.json")

# Create metrics to upload
metrics <- data.frame("metric1" = 2, "metric2" = 234, "metric3" = 1234)

mockPrivateUploadAoiMetrics <- function(study, obj, AOI, metrics, AOIDetailsFile = NULL, expectedFilepath = NULL,
                                        expectedEndpoint = NULL, expectedBody = NULL, expectCallsfwrite = 0,
                                        expectCallsPut = 0, expectCallsPresigned = 0, expectFileRemoved = FALSE) {

    privateGetAoiDetails_Stub <- mock(AOIDetailsFile)
    fwrite_Stub <- mock()
    fwriteMock <- if (expectFileRemoved) function(...) data.table::fwrite(...) else fwrite_Stub
    getUploadAoiMetricsUrl_Stub <- mock("myurl")
    fileInfos <- list(assetId = "assetId", presignedUrl = "presignedUrl")
    privateUploadToPresignedUrl_Stub <- mock(fileInfos)
    putHttr_Stub <- mock()

    mockr::with_mock(privateGetAoiDetails = privateGetAoiDetails_Stub,
                     fwrite = fwriteMock,
                     getUploadAoiMetricsUrl = getUploadAoiMetricsUrl_Stub,
                     privateUploadToPresignedUrl = privateUploadToPresignedUrl_Stub,
                     putHttr = putHttr_Stub, {
                         privateUploadAoiMetrics(study, obj, AOI, metrics)
                     })

    expect_called(privateGetAoiDetails_Stub, as.integer(inherits(obj, "imRespondent")))
    expect_called(fwrite_Stub, expectCallsfwrite)
    expect_called(getUploadAoiMetricsUrl_Stub, expectCallsPut)
    expect_called(privateUploadToPresignedUrl_Stub, expectCallsPresigned)
    expect_called(putHttr_Stub, expectCallsPut)

    if (!study$connection$localIM) {
        expect_args(getUploadAoiMetricsUrl_Stub, 1, study, obj, AOI)
    }

    if (inherits(obj, "imRespondent")) {
        expect_args(privateGetAoiDetails_Stub, 1, study = study, imObject = AOI, respondent = obj)

        if (expectCallsPresigned > 0) {
            expectedFilepath <- mock_args(privateUploadToPresignedUrl_Stub)[[1]][[4]]
            postData <- toJSON(list(name = AOI$name, fileName = expectedFilepath), null = "null")
            endpoint <- paste0("respondent: ", obj$name, ", AOI: ", AOI$name)

            expect_args(privateUploadToPresignedUrl_Stub, 1, study$connection, "myurl", postData, expectedFilepath,
                        "Getting presignedUrl to upload AOI metrics", paste("Uploading AOI metrics for", endpoint))

            expect_args(putHttr_Stub, 1, study$connection, "myurl", reqBody = toJSON(fileInfos, null = "null"),
                        message = paste("Upload of AOI metrics for", endpoint, "confirmed"))

            if (expectFileRemoved) {
                expect_false(file.exists(expectedFilepath), info = "temporary metrics file should have been deleted")
            }
        }

        if (expectCallsfwrite > 0) {
            expect_args(fwrite_Stub, 1, x = metrics, file = expectedFilepath, col.names = TRUE, row.names = FALSE)
        }
    } else if (inherits(obj, "imSegment") && expectCallsPut > 0) {
        expect_args(putHttr_Stub, 1, study$connection, "myurl", expectedBody, expectedEndpoint)
    }
}

test_that("warning - AOI is not found for a specific respondent", {
    AOIDetailsFile <- jsonlite::fromJSON("../data/no_scenes_annotations_aoidetails.json")

    expect_warning(mockPrivateUploadAoiMetrics(study, respondent, AOI, metrics, AOIDetailsFile),
                   "AOI New Aoi was not found for respondent Wendy",
                   info = "no AOI defined for this respondent should throw a warning")
})

test_that("local check - should call privateGetAoiDetails and fwrite for a specific respondent", {
    expectedFilepath <- paste0(tools::file_path_sans_ext(AOIDetailsFile$fileId), "metrics.csv")

    mockPrivateUploadAoiMetrics(study, respondent, AOI, metrics, AOIDetailsFile, expectedFilepath,
                                expectCallsfwrite = 1)
})

test_that("remote check - should call privateUploadToPresignedUrl and putHttr for a specific respondent", {
    mockPrivateUploadAoiMetrics(study_cloud, respondent_cloud, AOI_cloud, metrics, AOIDetailsFile = AOI_cloud,
                                expectCallsPut = 1, expectCallsPresigned = 1, expectFileRemoved = TRUE)
})

test_that("remote check - should remove cached metrics after uploading for a specific respondent", {
    expectedTmpPath <- paste0("myLocalPath/93fbaae0-8b6f-45b6-b5dd-9a5d4216d7fd/",
                              "dd8a9342-f5c0-4a02-bf27-de68fc13f2bc/", respondent_cloud$id, "metrics.csv")

    dir.create(dirname(expectedTmpPath), recursive = TRUE)
    file.create(expectedTmpPath)

    mockPrivateUploadAoiMetrics(study_cloud_local, respondent_cloud, AOI_cloud, metrics, AOIDetailsFile = AOI_cloud,
                                expectCallsPut = 1, expectCallsPresigned = 1, expectFileRemoved = TRUE)

    expect_false(file.exists(expectedTmpPath), info = "cached metrics file should have been deleted")
    unlink(study_cloud_local$connection$localPath, recursive = TRUE)
})

segment <- getSegments(study_cloud)

test_that("remote check - should call getUploadAoiMetricsUrl and putHttr for a specific segment", {
    expectedEndpoint <- "Updating AOI metrics for segment: All Respondents, AOI: El Manuel Area"
    expectedBody <- toJSON(fromJSON("../data/AOImetrics_cloud.json"))

    mockPrivateUploadAoiMetrics(study_cloud, segment, AOI_cloud, metrics, expectedEndpoint = expectedEndpoint,
                                expectedBody = expectedBody, expectCallsPut = 1)
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
