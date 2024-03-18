# getAOIRespondentMetrics =============================================================================================
context("getAOIRespondentMetrics()")

library(mockery)

# Load study
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))

# Load AOI
AOI <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imAOI.json")))
AOIDetailsRespondentPath <- "../data/AOIDetailsRespondent.json"
AOIDetailsFile <- jsonlite::fromJSON(AOIDetailsRespondentPath)

# Load respondent
respondent <- getRespondents(study)[1, ]

mockedGetAOIRespondentMetrics <- function(study, AOI, respondent, AOIDetailsFile) {
    privateGetAOIDetails_Stub <- mock(AOIDetailsFile)

    metrics <- mockr::with_mock(privateGetAOIDetails = privateGetAOIDetails_Stub, {
        getAOIRespondentMetrics(study, AOI, respondent)
    })

    expect_args(privateGetAOIDetails_Stub, 1, study = study, imObject = AOI, respondent = respondent)
    return(metrics)
}

test_that("error - arguments are missing or not from the good class", {
    # in case of missing study
    expect_error(getAOIRespondentMetrics(), "Please specify a study loaded with `imStudy()`", fixed = TRUE,
                 info = "missing `study` param not handled properly")

    # in case of missing AOI
    expect_error(getAOIRespondentMetrics(study), "Please specify an AOI loaded with `getAOIs()`", fixed = TRUE,
                 info = "missing `AOI` param not handled properly")

    # in case of missing respondent
    expect_error(getAOIRespondentMetrics(study, AOI), "Please specify a respondent loaded with `getRespondents()`",
                 fixed = TRUE, info = "missing `respondent` param not handled properly")

    # in case of study that is not an imStudy object
    expect_error(getAOIRespondentMetrics(study = "whatever", AOI, respondent),
                 "`study` argument is not an imStudy object",
                 info = "study not being an imStudy object should throw an error")

    # in case of AOI that is not an imAOI object
    expect_error(getAOIRespondentMetrics(study, AOI = "whatever", respondent), "`AOI` argument is not an imAOI object",
                 info = "AOI not being an imAOI object should throw an error")

    # in case of respondent that is not an imRespondent object
    expect_error(getAOIRespondentMetrics(study, AOI, respondent = "whatever"),
                 "`respondent` argument is not an imRespondent object",
                 info = "respondent not being an imRespondent object should throw an error")
})

test_that("warning - AOI has not been defined for this respondent", {
    AOIDetailsFile <- jsonlite::fromJSON("../data/no_scenes_annotations_aoidetails.json")
    expect_warning(metrics <- mockedGetAOIRespondentMetrics(study, AOI, respondent, AOIDetailsFile),
                   "AOI New Aoi was not found for respondent Wendy",
                   info = "no AOI defined for this respondent should throw a warning")

    expect_null(metrics, info = "result should be null")
})

test_that("warning - no metrics have been found for this respondent", {
    expect_warning(metrics <- mockedGetAOIRespondentMetrics(study, AOI, respondent, AOIDetailsFile),
                   "No metrics found for AOI: New Aoi, Respondent: Wendy",
                   info = "no metrics found should throw an warning")

    expect_null(metrics, "result should be null")
})

# Modify AOIDetailsFile so it fit test data
AOIDetailsFile$resultId <- "../data/AOImetrics.csv"

test_that("local return - metrics for this AOI/respondent pair", {
    metrics <- mockedGetAOIRespondentMetrics(study, AOI, respondent, AOIDetailsFile)

    # Check dimensions and class of metrics
    expect_equal(nrow(metrics), 1, infos = "metrics should always only have one row")
    expect_equal(ncol(metrics), 37, infos = "no column should be lost")
    expect_s3_class(metrics, "imAOIMetrics")
})
