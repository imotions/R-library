context("uploadAOIRespondentMetrics()")

library("imotionsApi")
library("mockery")
library("arrow")

# Load study, respondent and AOI
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))
respondent <- getRespondent(study, "09bd22e6-29b6-4a8a-8cc1-4780a5163e63")
AOI <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imAOI.json")))

AOIDetailsRespondentPath <- "../data/AOIDetailsRespondent.json"
AOIDetailsFile <- jsonlite::fromJSON(AOIDetailsRespondentPath)

# Create metrics to upload
metrics <- data.frame("metric1" = 2, "metric2" = 234, "metric3" = 1234)


test_that("should throw errors if arguments are missing or not from the good class", {
    # in case of missing study
    error <- capture_error(uploadAOIRespondentMetrics())
    expect_identical(error$message, "Please specify a study loaded with `imStudy()`",
                     "missing `study` param not handled properly")

    # in case of missing AOI
    error <- capture_error(uploadAOIRespondentMetrics(study))
    expect_identical(error$message, "Please specify an AOI loaded with `getAOIs()`",
                     "missing `target` param not handled properly")

    # in case of missing respondent
    error <- capture_error(uploadAOIRespondentMetrics(study, AOI))
    expect_identical(error$message, "Please specify a respondent loaded with `getRespondents()`",
                     "missing `target` param not handled properly")

    # in case of missing metrics
    error <- capture_error(uploadAOIRespondentMetrics(study, AOI, respondent))
    expect_identical(error$message, "Please specify a data.table with metrics to upload",
                     "missing `metrics` param not handled properly")

    # in case of study that is not an imStudy object
    error <- capture_error(uploadAOIRespondentMetrics(study = "whatever", AOI, respondent, metrics))
    expect_identical(error$message, "`study` argument is not an imStudy object",
                     "study not being an imStudy object should throw an error")

    # in case of AOI that is not an imAOI object
    error <- capture_error(uploadAOIRespondentMetrics(study, AOI = "whatever", respondent, metrics))
    expect_identical(error$message, "`AOI` argument is not an imAOI object",
                     "AOI not being an imAOI object should throw an error")

    # in case of respondent that is not an imRespondent object
    error <- capture_error(uploadAOIRespondentMetrics(study, AOI, respondent = "whatever", metrics))
    expect_identical(error$message, "`respondent` argument is not an imRespondent object",
                     "respondent not being an imRespondent object should throw an error")

    # in case of wrong metrics format
    wrongData <- data.table(Timestamp = integer(), variableTest = numeric())
    error <- capture_error(uploadAOIRespondentMetrics(study, AOI, respondent, wrongData))
    expect_identical(error$message, "Do not upload an empty dataset", "zero row dataset should not be uploaded")
})


test_that("should return a warning if the AOI is not found for the specific respondent", {
    metrics <- checkDataFormat(metrics)
    privateGetAOIDetails_Stub <- mock(jsonlite::fromJSON("../data/no_scenes_annotations_aoidetails.json"), cycle = T)

    warning <- capture_warning(mockr::with_mock(privateGetAOIDetails = privateGetAOIDetails_Stub, {
                                                    uploadAOIRespondentMetrics(study, AOI, respondent, metrics)
                                                }))

    expect_called(privateGetAOIDetails_Stub, 1)
    expect_args(privateGetAOIDetails_Stub, 1, study = study, imObject = AOI, respondent = respondent)
    expect_identical(warning$message, "AOI New Aoi was not found for respondent Wendy",
                     "no AOI defined for this respondent should throw an error")

    expect_null(suppressWarnings(mockr::with_mock(privateGetAOIDetails = privateGetAOIDetails_Stub, {
                                                      uploadAOIRespondentMetrics(study, AOI, respondent, metrics)
                                                  })), "result should be null")
})

test_that("should not call write.csv if metrics is of wrong format", {
    wrongData <- data.frame("Timestamp" = seq(1:100), "Thresholded value" = rep(0, 100))
    privateGetAOIDetails_Stub <- mock(AOIDetailsFile)

    writecsv_Stub <- mock()

    warning <- capture_warning(mockr::with_mock(
        privateGetAOIDetails = privateGetAOIDetails_Stub,
        write.csv = writecsv_Stub, {
            uploadAOIRespondentMetrics(study, AOI, respondent, wrongData)
        }))

    expect_called(writecsv_Stub, 0)
    expect_identical(warning$message, "Metrics should be a data.frame/data.table composed of only one row")
})

test_that("should call write.csv if metrics are of good format", {
    metrics <- checkDataFormat(metrics)
    privateGetAOIDetails_Stub <- mock(AOIDetailsFile)

    writecsv_Stub <- mock()
    filepath <- paste0(tools::file_path_sans_ext(AOIDetailsFile$fileId), "metrics.csv")

    mockr::with_mock(
        privateGetAOIDetails = privateGetAOIDetails_Stub,
        write.csv = writecsv_Stub, {
            uploadAOIRespondentMetrics(study, AOI, respondent, metrics)
        })

    expect_called(writecsv_Stub, 1)
    expect_args(writecsv_Stub, 1, x = metrics, file = filepath, col.names = TRUE, row.names = FALSE)
})
