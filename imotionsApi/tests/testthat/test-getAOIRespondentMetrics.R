library("imotionsApi");
library("stubthat");
library("arrow");

context("getAOIRespondentMetrics()");

# Load study
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))

# Load AOI
AOI <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imAOI.json")))
AOIDetailsRespondentPath <- "../data/AOIDetailsRespondent.json"
AOIDetailsFile <- jsonlite::fromJSON(AOIDetailsRespondentPath)

# Load respondent
respondent <- getRespondents(study)[1, ]

mockedGetAOIRespondentMetrics <- function(study, AOI, respondent, AOIDetailsFile) {
    privateGetAOIDetails_Stub <- stub(privateGetAOIDetails)
    privateGetAOIDetails_Stub$expects(study = study, imObject = AOI, respondent = respondent)
    privateGetAOIDetails_Stub$returns(AOIDetailsFile)

    metrics <- mockr::with_mock(privateGetAOIDetails = privateGetAOIDetails_Stub$f,
                                {
                                    getAOIRespondentMetrics(study, AOI, respondent)
                                })

    return(metrics)
}

test_that("should throw errors if arguments are missing or not from the good class", {
    # in case of missing study
    error <- capture_error(getAOIRespondentMetrics())
    expect_identical(error$message, "Please specify a study loaded with `imStudy()`",
                     "missing `study` param not handled properly")

    # in case of missing AOI
    error <- capture_error(getAOIRespondentMetrics(study))
    expect_identical(error$message, "Please specify an AOI loaded with `getAOIs()`",
                     "missing `AOI` param not handled properly")

    # in case of missing respondent
    error <- capture_error(getAOIRespondentMetrics(study, AOI))
    expect_identical(error$message, "Please specify a respondent loaded with `getRespondents()`",
                     "missing `respondent` param not handled properly")

    # in case of study that is not an imStudy object
    error <- capture_error(getAOIRespondentMetrics(study = "whatever", AOI, respondent))
    expect_identical(error$message, "`study` argument is not an imStudy object",
                     "study not being an imStudy object should throw an error")

    # in case of AOI that is not an imAOI object
    error <- capture_error(getAOIRespondentMetrics(study, AOI = "whatever", respondent))
    expect_identical(error$message, "`AOI` argument is not an imAOI object",
                     "AOI not being an imAOI object should throw an error")

    # in case of respondent that is not an imRespondent object
    error <- capture_error(getAOIRespondentMetrics(study, AOI, respondent = "whatever"))
    expect_identical(error$message, "`respondent` argument is not an imRespondent object",
                     "respondent not being an imRespondent object should throw an error")
})

test_that("should throw a warning if the AOI has not been defined for this respondent", {
    AOIDetailsFile <- jsonlite::fromJSON("../data/no_scenes_annotations_aoidetails.json")
    warning <- capture_warning(mockedGetAOIRespondentMetrics(study, AOI, respondent, AOIDetailsFile))
    expect_identical(warning$message, "AOI New Aoi was not found for respondent Wendy",
                     "no AOI defined for this respondent should throw an error")

    expect_null(suppressWarnings(mockedGetAOIRespondentMetrics(study, AOI, respondent, AOIDetailsFile)),
                "result should be null")
})

test_that("should throw a warning if no metrics have been found for this respondent", {
    warning <- capture_warning(mockedGetAOIRespondentMetrics(study, AOI, respondent, AOIDetailsFile))
    expect_identical(warning$message, "No metrics found for AOI: New Aoi, Respondent: Wendy",
                     "no metrics found should throw an error")

    expect_null(suppressWarnings(mockedGetAOIRespondentMetrics(study, AOI, respondent, AOIDetailsFile)),
                "result should be null")
})

# Modify AOIDetailsFile so it fit test data
AOIDetailsFile$resultId <- "../data/AOImetrics.csv"

test_that("should return the correct metrics for this AOI/respondent pair", {
    metrics <- mockedGetAOIRespondentMetrics(study, AOI, respondent, AOIDetailsFile)

    # Check dimensions and class of metrics
    expect_equal(nrow(metrics), 1, infos = "metrics should always only have one row")
    expect_equal(ncol(metrics), 37, infos = "no column should be lost")
    expect_true(inherits(metrics, "imMetrics"), "`metrics` should be an imMetrics object")
})
