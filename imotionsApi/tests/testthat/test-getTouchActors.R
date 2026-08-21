# getTouchActors ======================================================================================================
context("getTouchActors()")

library(mockery)

# Load study
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))
study_cloud <- jsonlite::unserializeJSON(readLines("../data/imStudy_cloud.json"))

# Load respondent and stimulus
respondent <- getRespondents(study)[1, ]
stimulus <- getStimuli(study)[4, ]

TA1 <- "5b1f0c42-9d6e-4a3b-8f21-0c7d5e9a1b34"
TA2 <- "c8e4a17d-2f60-4b95-a3d8-6e1b70f2c945"
AOI1 <- "09a234fc-00ad-4257-b7e1-da5754986c9d"
AOI2 <- "7d3b81e5-4c92-4f07-b6a1-52e8d90c3af6"

touchActorsPath <- "../data/touchActors.json"
noTouchActorsPath <- "../data/noTouchActors.json"
touchActorDetailsPath <- "../data/touchActorDetails.json"
touchActorDetailsMissingPath <- "../data/touchActorDetails_missingfileId.json"
touchActorDetailsNoMatchPath <- "../data/touchActorDetails_noMatchingPair.json"

# The AOI definitions getTouchActorAois() joins against - what getAois() would return for this stimulus.
aoiDefinitions <- function() {
    aois <- data.table(stimulusId = "1002", stimulusName = "IAAF", id = c(AOI1, AOI2),
                       name = c("New Aoi", "Second Aoi"), type = "Static", group = NA_character_,
                       area = c(5000, 2500))

    return(imotionsApi:::createImObject(aois, "AOI"))
}

mockedGetTouchActors <- function(study, stimulus, jsonPath = touchActorsPath, expectedCall = 1) {
    getJSON_Stub <- mock(jsonlite::fromJSON(jsonPath))

    touchActors <- mockr::with_mock(getJSON = getJSON_Stub, {
        getTouchActors(study, stimulus)
    })

    expect_called(getJSON_Stub, expectedCall)
    return(touchActors)
}

test_that("error - arguments are missing or not from the good class", {
    expect_error(getTouchActors(), "Please specify a study loaded with `imStudy()`", fixed = TRUE,
                 info = "missing `study` param not handled properly")

    expect_error(getTouchActors(study), "Please specify a stimulus loaded with `getStimuli()`", fixed = TRUE,
                 info = "missing `stimulus` param not handled properly")

    expect_error(getTouchActors(study = "whatever", stimulus), "`study` argument is not an imStudy object",
                 info = "study not being an imStudy object should throw an error")

    expect_error(getTouchActors(study, stimulus = "whatever"), "`stimulus` argument is not an imStimulus object",
                 info = "stimulus not being an imStimulus object should throw an error")
})

test_that("local return - touch actors defined for a stimulus", {
    touchActors <- mockedGetTouchActors(study, stimulus)

    expect_equal(nrow(touchActors), 2, info = "2 touch actors should be defined for this stimulus")
    expect_identical(touchActors$id, c(TA1, TA2), "wrong touch actor ids")
    expect_identical(touchActors$name, c("Right hand", "Left hand"), "wrong touch actor names")
    expect_identical(touchActors$hand, c("Right", "Left"), "wrong hand")
    expect_identical(unique(touchActors$detectionMethod), "HoiDetr", "wrong detection method")
    expect_identical(unique(touchActors$mediaSourceType), "StimulusCamera", "wrong media source type")
    expect_s3_class(touchActors, "imTouchActorList")
})

test_that("local return - stimulusId is coerced to a string to match every other imObject", {
    touchActors <- mockedGetTouchActors(study, stimulus)

    expect_identical(unique(touchActors$stimulusId), "1002", "stimulusId should be a character")
    expect_identical(unique(touchActors$stimulusName), "IAAF", "wrong stimulus name")
})

test_that("local return - interactiveAoiIds is kept as a list column", {
    touchActors <- mockedGetTouchActors(study, stimulus)

    expect_identical(touchActors$interactiveAoiIds[[1]], c(AOI1, AOI2), "first actor should reference 2 AOIs")
    expect_identical(touchActors$interactiveAoiIds[[2]], AOI1, "second actor should reference 1 AOI")
})

test_that("warning - no touch actor defined for this stimulus", {
    expect_warning(touchActors <- mockedGetTouchActors(study, stimulus, noTouchActorsPath),
                   "No touch actor defined for Stimulus: IAAF",
                   info = "no touch actor defined should throw a warning")

    expect_null(touchActors, info = "result should be null")
})

test_that("warning - touch actors are not available for a remote study", {
    stimulus_cloud <- getStimuli(study)[4, ]

    expect_warning(touchActors <- mockedGetTouchActors(study_cloud, stimulus_cloud, expectedCall = 0),
                   "Touch actors are only available for local studies",
                   info = "remote study should throw a warning")

    expect_null(touchActors, info = "result should be null")
})


# privateGetTouchActorDetails =========================================================================================
context("privateGetTouchActorDetails()")

mockedGetTouchActorDetails <- function(study, imObject, respondent, jsonPath = touchActorDetailsPath,
                                       expectedCall = 1) {
    getJSON_Stub <- mock(jsonlite::fromJSON(jsonPath))

    details <- mockr::with_mock(getJSON = getJSON_Stub, {
        # Called bare rather than via `imotionsApi:::` - mockr::with_mock() patches the getJSON binding it's given in
        # the imotionsApi namespace, but a `:::`-qualified call here bypasses that patched binding and hits the real
        # network, as `privateGetAoiDetails()`'s equivalent test does it (see test-getAOIRespondentData.R).
        privateGetTouchActorDetails(study, imObject, respondent)
    })

    expect_called(getJSON_Stub, expectedCall)
    return(details)
}

expectedDetailNames <- c("touchActorId", "aoiId", "respId", "fileId", "resultId", "fileIdIsEmptyPlaceHolder")

test_that("local return - details for every touch actor on a stimulus", {
    details <- mockedGetTouchActorDetails(study, stimulus, respondent)

    expect_equal(nrow(details), 3, info = "3 (touch actor, AOI) combinations should be returned")
    expect_named(details, expectedDetailNames, info = "touch actor details infos not matching")
    expect_identical(unique(details$respId), respondent$id, "details should all be for the requested respondent")
})

test_that("local return - combinations without a fileId are filtered out", {
    details <- mockedGetTouchActorDetails(study, stimulus, respondent, touchActorDetailsMissingPath)

    expect_equal(nrow(details), 2, info = "the combination without a fileId should be removed")
    expect_false(TA2 %in% details$touchActorId, "the touch actor without contact detection should be gone")
})

test_that("local return - a fileId already present short-circuits the request", {
    touchAoi <- aoiDefinitions()[1, ]
    touchAoi$fileId <- "../data/touchActorRespondentData.pbin"
    touchAoi$touchActorId <- TA1
    class(touchAoi) <- c("imTouchAOI", class(touchAoi))

    details <- mockedGetTouchActorDetails(study, touchAoi, respondent, expectedCall = 0)

    expect_identical(details$fileId, touchAoi$fileId, "fileId stored in the AOI should be retrieved directly")
})

test_that("warning - no contact in/out signal for this respondent", {
    expect_warning(details <- mockedGetTouchActorDetails(study, stimulus, respondent, noTouchActorsPath),
                   "has no contact in/out signals, has contact detection been run?",
                   info = "absent contact signals should throw a warning")

    expect_null(details, info = "result should be null")
})

test_that("warning - contact data is not available for a remote study", {
    expect_warning(details <- mockedGetTouchActorDetails(study_cloud, stimulus, respondent, expectedCall = 0),
                   "Contact data is only available for local studies",
                   info = "remote study should throw a warning")

    expect_null(details, info = "result should be null")
})


# getTouchActorAois ===================================================================================================
context("getTouchActorAois()")

mockedGetTouchActorAois <- function(study, imObject, respondent, detailsPath = touchActorDetailsPath) {
    touchActors <- suppressWarnings(mockedGetTouchActors(study, stimulus))

    touchAois <- mockr::with_mock(
        getTouchActors = function(...) touchActors,
        privateAoiFiltering = function(...) aoiDefinitions(),
        privateGetTouchActorDetails = function(...) jsonlite::fromJSON(detailsPath), {
            getTouchActorAois(study, imObject, respondent)
        }
    )

    return(touchAois)
}

test_that("error - arguments are missing or not from the good class", {
    expect_error(getTouchActorAois(study, respondent = respondent),
                 "Please specify a stimulus loaded with `getStimuli()` or a touch actor loaded with `getTouchActors()`",
                 fixed = TRUE, info = "missing `imObject` param not handled properly")

    expect_error(getTouchActorAois(study, stimulus), "Please specify a respondent loaded with `getRespondents()`",
                 fixed = TRUE, info = "missing `respondent` param not handled properly")

    expect_error(getTouchActorAois(study, stimulus, respondent = "whatever"),
                 "`respondent` argument is not an imRespondent object",
                 info = "respondent not being an imRespondent object should throw an error")

    expect_error(getTouchActorAois(study, "whatever", respondent),
                 "`imObject` argument is not an imStimulus or imTouchActor object",
                 info = "imObject not being an imStimulus or an imTouchActor object should throw an error")
})

test_that("local return - one row per (touch actor, AOI) combination", {
    touchAois <- mockedGetTouchActorAois(study, stimulus, respondent)

    expect_equal(nrow(touchAois), 3, info = "3 (touch actor, AOI) combinations should be returned")
    expect_equal(sum(touchAois$id == AOI1), 2, info = "AOI1 is referenced by both touch actors")
    expect_equal(sum(touchAois$id == AOI2), 1, info = "AOI2 is referenced by one touch actor")
})

test_that("local return - the AOI definition wins the id/name columns so it can stand in for an imAOI", {
    touchAois <- mockedGetTouchActorAois(study, stimulus, respondent)

    expect_s3_class(touchAois, "imAOIList")
    expect_identical(unique(touchAois[touchAois$id == AOI1, ]$name), "New Aoi", "AOI name should win over the actor's")
    expect_identical(unique(touchAois$type), "Static", "AOI type should be carried over")
    expect_equal(unique(touchAois[touchAois$id == AOI1, ]$area), 5000, info = "AOI area should be carried over")
    expect_identical(unique(touchAois$stimulusName), "IAAF", "stimulus name should be carried over")
})

test_that("local return - result is an imTouchAOI(List), a subclass of imAOI(List) rather than a plain one", {
    touchAois <- mockedGetTouchActorAois(study, stimulus, respondent)

    expect_identical(class(touchAois), c("imTouchAOIList", "imAOIList", "imObjectList", "data.table", "data.frame"),
                     "wrong class for a multi-row result")

    singleTouchAoi <- touchAois[1, ]
    expect_identical(class(singleTouchAoi), c("imTouchAOI", "imAOI", "imObject", "data.table", "data.frame"),
                     "wrong class for a single-row result")
})

test_that("local return - the touch actor is kept alongside under its own columns", {
    touchAois <- mockedGetTouchActorAois(study, stimulus, respondent)

    expect_true(all(c("touchActorId", "touchActorName", "hand") %in% names(touchAois)),
                info = "touch actor columns should be present")

    expect_setequal(touchAois$touchActorId, c(TA1, TA1, TA2))
    expect_identical(unique(touchAois[touchAois$touchActorId == TA2, ]$hand), "Left", "wrong hand for second actor")
})

test_that("local return - the fileId of each combination is carried over", {
    touchAois <- mockedGetTouchActorAois(study, stimulus, respondent)

    expect_true(all(!is.na(touchAois$fileId)), info = "every combination should have an in/out file")
    expect_identical(touchAois[touchAois$id == AOI2, ]$fileId, "../data/touchActorEmptyRespondentData.pbin",
                     "the never-active AOI should point at the empty placeholder")
})

test_that("local return - filtering on a single touch actor", {
    touchActor <- suppressWarnings(mockedGetTouchActors(study, stimulus))[2, ]
    touchAois <- mockedGetTouchActorAois(study, touchActor, respondent)

    expect_equal(nrow(touchAois), 1, info = "only the second actor's single AOI should be returned")
    expect_identical(touchAois$touchActorId, TA2, "wrong touch actor kept")
})

test_that("local return - a touch actor on a media source getAois() cannot see keeps its row, AOI columns NA", {
    TA_ENV <- "4f2d6b30-7c44-4d1b-9c19-9c6b9e2a5b60"
    AOI_ENV <- "1c9c8b18-3e1d-4b53-9c62-2a6b1a0d7e11"

    touchActorsMulti <- mockedGetTouchActors(study, stimulus, "../data/touchActorsMultiSource.json")

    touchAois <- mockr::with_mock(
        getTouchActors = function(...) touchActorsMulti,
        privateAoiFiltering = function(...) aoiDefinitions(),
        privateGetTouchActorDetails = function(...) jsonlite::fromJSON("../data/touchActorDetailsMultiSource.json"), {
            getTouchActorAois(study, stimulus, respondent)
        }
    )

    expect_equal(nrow(touchAois), 2, info = "both touch actors' AOIs should be returned regardless of media source")
    expect_setequal(touchAois$mediaSourceType, c("StimulusCamera", "Environment"))

    sceneRow <- touchAois[touchAois$touchActorId == TA1, ]
    envRow <- touchAois[touchAois$touchActorId == TA_ENV, ]

    expect_identical(sceneRow$id, AOI1, "wrong AOI kept for the Scene touch actor")
    expect_identical(sceneRow$name, "New Aoi", info = "the Scene AOI is still enriched from getAois() as before")

    expect_identical(envRow$id, AOI_ENV, info = "the AOI id itself comes from the touch actor side, never dropped")
    expect_true(is.na(envRow$name), info = "an AOI on a camera getAois() can't see gets NA, not a dropped row")
    expect_true(is.na(envRow$type), info = "type should be NA for the same reason")
    expect_true(is.na(envRow$area), info = "area should be NA for the same reason")
    expect_false(is.na(envRow$fileId), info = "the contact in/out file should still be resolved for this row")
    expect_identical(envRow$stimulusId, "1002",
                     info = "stimulusId should come from the touch actor, not the (Scene-only) AOI join")
    expect_identical(envRow$mediaSourceType, "Environment", "wrong media source kept for the environment actor")
})

test_that("warning - contact in/out signals found do not match any touch actor pair", {
    expect_warning(
        touchAois <- mockedGetTouchActorAois(study, stimulus, respondent,
                                             detailsPath = "../data/touchActorDetails_noMatchingPair.json"),
        "The contact in/out signals found do not match any touch actor AOI",
        info = "the guard should still fire when pairs and details genuinely have nothing in common"
    )

    expect_null(touchAois, info = "result should be null")
})


# getAoiRespondentTouchData ===========================================================================================
context("getAoiRespondentTouchData()")

touchAoiInOut <- function(fileId = "../data/touchActorRespondentData.pbin", resultId = NA_character_) {
    touchAoi <- aoiDefinitions()[1, ]
    touchAoi$touchActorId <- TA1
    touchAoi$fileId <- fileId
    touchAoi$resultId <- resultId
    class(touchAoi) <- c("imTouchAOI", class(touchAoi))
    return(touchAoi)
}

test_that("error - arguments are missing or not from the good class", {
    expect_error(getAoiRespondentTouchData(), "Please specify a study loaded with `imStudy()`", fixed = TRUE,
                 info = "missing `study` param not handled properly")

    expect_error(getAoiRespondentTouchData(study),
                 "Please specify a touch actor AOI loaded with `getTouchActorAois()`", fixed = TRUE,
                 info = "missing `touchAoi` param not handled properly")

    expect_error(getAoiRespondentTouchData(study, touchAoiInOut()),
                 "Please specify a respondent loaded with `getRespondents()`", fixed = TRUE,
                 info = "missing `respondent` param not handled properly")

    expect_error(getAoiRespondentTouchData(study, touchAoi = "whatever", respondent),
                 "`touchAoi` argument is not an imTouchAOI object",
                 info = "touchAoi not being an imTouchAOI object should throw an error")
})

test_that("local return - intervals for a specific (touch actor, AOI)/respondent pair", {
    intervals <- getAoiRespondentTouchData(study, touchAoiInOut(), respondent)$intervals

    expect_equal(nrow(intervals), 2, info = "should have 2 active fragments")
    expect_equal(intervals$fragments.start, c(9181.255, 16015.560), 1e-2, info = "wrong fragments start")
    expect_equal(intervals$fragments.end, c(11400.944, 18352.070), 1e-2, info = "wrong fragments end")
    expect_equal(intervals$fragments.duration, c(2219.689, 2336.510), 1e-2, info = "wrong fragments duration")
    expect_identical(unique(intervals$type), "AOI", "intervals should all be of AOI type")
    expect_identical(unique(intervals$parentId), "1002", "intervals should carry the stimulus as parent")
    expect_identical(unique(intervals$parentName), "IAAF", "intervals should carry the stimulus name as parent")
    expect_identical(unique(intervals$name), "New Aoi", "intervals should be named after the AOI")
    expect_s3_class(intervals, "imIntervalList")
})

test_that("local return - inOutContact is compressed on contact and hand", {
    inOutContact <- getAoiRespondentTouchData(study, touchAoiInOut(), respondent)$inOutContact

    expect_named(inOutContact, c("Timestamp", "IsContactInAOI", "HandType", "HandId"), info = "wrong column names")
    expect_equal(nrow(inOutContact), 6, info = "should have 6 change rows")
    expect_identical(inOutContact$IsContactInAOI, c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE), "wrong contact values")
})

test_that("local return - a hand handover mid-contact stays visible", {
    inOutContact <- getAoiRespondentTouchData(study, touchAoiInOut(), respondent)$inOutContact

    expect_identical(inOutContact$HandType, c("None", "Left", "Right", "None", "Right", "None"),
                     "hand codes should be decoded and the handover kept as its own row")

    expect_equal(inOutContact[inOutContact$HandType == "Left", ]$Timestamp, 9500, 1e-2,
                 info = "wrong timestamp for the left hand contact")
})

test_that("local check - AOI never active returns the empty shape rather than an error", {
    result <- getAoiRespondentTouchData(study, touchAoiInOut("../data/touchActorEmptyRespondentData.pbin"),
                                        respondent)

    expect_named(result, c("inOutContact", "intervals"), info = "wrong names")
    expect_equal(nrow(result$inOutContact), 0, info = "should have no contact events")
    expect_named(result$inOutContact, c("Timestamp", "IsContactInAOI", "HandType", "HandId"),
                 info = "empty inOutContact should still be well formed")

    expect_equal(nrow(result$intervals), 1, info = "should have a single placeholder interval")
    expect_true(is.na(result$intervals$fragments.start), info = "placeholder interval should have no start")
    expect_equal(result$intervals$fragments.duration, 0, info = "placeholder interval should have a 0 duration")
})


# getAoiRespondentTouchMetrics ========================================================================================
context("getAoiRespondentTouchMetrics()")

test_that("error - arguments are missing or not from the good class", {
    expect_error(getAoiRespondentTouchMetrics(study, touchAoi = "whatever", respondent),
                 "`touchAoi` argument is not an imTouchAOI object",
                 info = "touchAoi not being an imTouchAOI object should throw an error")
})

test_that("local return - metrics are read from the resultId", {
    touchAoi <- touchAoiInOut(resultId = "../data/AOImetrics.csv")
    metrics <- getAoiRespondentTouchMetrics(study, touchAoi, respondent)

    expect_equal(nrow(metrics), 1, info = "metrics should be a single row")
    expect_s3_class(metrics, "imAOIMetrics")
})

test_that("warning - no metrics computed yet for this combination", {
    expect_warning(metrics <- getAoiRespondentTouchMetrics(study, touchAoiInOut(), respondent),
                   "No contact metrics found for AOI: New Aoi, Respondent: Wendy",
                   info = "a missing resultId should throw a warning")

    expect_null(metrics, info = "result should be null")
})


# privateRespondentFiltering.imTouchAOI ===============================================================================
context("privateRespondentFiltering.imTouchAOI()")

touchAoiForRespondents <- function() {
    touchAoi <- aoiDefinitions()[1, ]
    touchAoi$touchActorId <- TA1
    class(touchAoi) <- c("imTouchAOI", class(touchAoi))
    return(touchAoi)
}

test_that("local return - getRespondents() dispatches on imTouchAOI to use contact data, not gaze data", {
    allRespondents <- getRespondents(study, stimulus = stimulus)

    # Only the first respondent of the stimulus has a matching (touchActorId, aoiId) contact pair.
    respondents <- mockr::with_mock(
        privateGetTouchActorDetails = function(study, imObject, respondent) {
            if (respondent$id == allRespondents$id[1]) jsonlite::fromJSON(touchActorDetailsPath) else NULL
        }, {
            getRespondents(study, AOI = touchAoiForRespondents())
        }
    )

    expect_equal(nrow(respondents), 1, info = "only the respondent with a matching contact pair should be kept")
    expect_identical(respondents$id, allRespondents$id[1], "wrong respondent kept")
})

test_that("local return - a respondent with contact data for another (touch actor, AOI) pair is not kept", {
    respondents <- mockr::with_mock(
        # This respondent has contact data, but not for the TA1/AOI1 pair being asked about.
        privateGetTouchActorDetails = function(...) jsonlite::fromJSON(touchActorDetailsNoMatchPath), {
            getRespondents(study, AOI = touchAoiForRespondents())
        }
    )

    expect_equal(nrow(respondents), 0, info = "no respondent should be kept")
})

test_that("local return - no respondent has contact data", {
    respondents <- mockr::with_mock(
        privateGetTouchActorDetails = function(...) NULL, {
            getRespondents(study, AOI = touchAoiForRespondents())
        }
    )

    expect_equal(nrow(respondents), 0, info = "no respondent should be kept")
})


# privateDecodeHandType ===============================================================================================
context("privateDecodeHandType()")

test_that("hand codes are decoded to their labels", {
    expect_identical(imotionsApi:::privateDecodeHandType(c(0, 1, 2, 3)), c("None", "Left", "Right", "Unknown"),
                     "wrong labels for the documented hand codes")
})

test_that("an unrecognised or missing code is reported as unknown rather than dropped", {
    expect_identical(imotionsApi:::privateDecodeHandType(c(9, NA, -1)), rep("Unknown", 3),
                     "unrecognised codes should not become missing values")
})


# Touch actor urls ====================================================================================================
context("getTouchActorDetailsUrl()")

test_that("url for every touch actor on a stimulus uses the wildcard form", {
    url <- imotionsApi:::getTouchActorDetailsUrl(study, stimulus, respondent)

    expect_identical(url, file.path("http://localhost:8086/touchactors", study$id, "stimuli", stimulus$id,
                                    "respondent", respondent$id, "*"),
                     "wrong url for all touch actors")
})

test_that("url for a single touch actor uses its id - the wildcard sits on the actor, not the AOI", {
    touchAoi <- aoiDefinitions()[1, ]
    touchAoi$touchActorId <- TA1
    class(touchAoi) <- c("imTouchAOI", class(touchAoi))

    url <- imotionsApi:::getTouchActorDetailsUrl(study, touchAoi, respondent)

    expect_identical(url, file.path("http://localhost:8086/touchactors", study$id, "stimuli", "1002",
                                    "respondent", respondent$id, TA1),
                     "wrong url for a single touch actor")
})
