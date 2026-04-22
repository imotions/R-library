# privateCalculateAreaAoi =============================================================================================
context("privateCalculateAreaAoi()")

library(mockery)

test_that("return - AOI area", {
    # Case of a square
    x <- c(0, 5, 5, 0)
    y <- c(5, 5, 0, 0)
    area <- privateCalculateAreaAoi(x, y)
    expect_equal(area, 25, info = "wrong computation for square")

    # Case of a rectangle
    x <- c(0, 5, 5, 0)
    y <- c(1, 1, 0, 0)
    area <- privateCalculateAreaAoi(x, y)
    expect_equal(area, 5, info = "wrong computation for rectangle")

    # Case of an empty shape
    x <- c(0, 5, 5, 0)
    y <- c(0, 0, 0, 0)
    area <- privateCalculateAreaAoi(x, y)
    expect_equal(area, 0, info = "wrong computation for empty shape")
})

# privateAoiFormatting ================================================================================================
context("privateAoiFormatting()")

# Load study and stimuli
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))
stimuli <- getStimuli(study)
respondents <- getRespondents(study)

# Load remote study
study_cloud <- jsonlite::unserializeJSON(readLines("../data/imStudy_cloud.json"))

studyAOIsPath <- "../data/noAOIs.json"
stimulusAOIsPath <- "../data/noAOIs.json"
respondentAOIsPath <- "../data/noAOIs.json"
studyAOIsCloudPath <- "../data/no_scenes_annotations_aoidetails.json"
respondentAOIsCloudPath <- "../data/no_scenes_annotations_aoidetails.json"

mockedPrivateAoiFormatting <- function(study, AOIsUrl, endpoint, expectedCallGetJSON = 1, remove_saved_AOIs = TRUE) {
    getJSON_Stub <- mock(jsonlite::fromJSON(AOIsUrl))

    AOIs <- mockr::with_mock(getJSON = getJSON_Stub, {
        privateAoiFormatting(study, AOIsUrl, endpoint)
    })

    expect_called(getJSON_Stub, expectedCallGetJSON)

    if (expectedCallGetJSON > 0) {
        expect_args(getJSON_Stub, 1, connection = study$connection, url = AOIsUrl,
                    message = paste("Retrieving AOIs for", endpoint))
    }

    if (remove_saved_AOIs) {
        # For testing we want to be able to load different AOI set to the same study which doesn't happen in real life
        setattr(study, "AOIs", NULL)
    }

    return(AOIs)
}


test_that("warning - no AOI defined", {
    # Should return a warning when no AOI have been defined
    expect_warning(mockedPrivateAoiFormatting(study, studyAOIsPath, "expected endpoint"),
                   "No AOI defined for expected endpoint",
                   info = "no AOI warning should have been thrown")
})

test_that("local return - AOIs data.table", {
    studyAOIsPath <- "../data/studyAOIs.json"

    # Should return all AOIs from this study if no stimulus is provided
    AOIs <- mockedPrivateAoiFormatting(study, studyAOIsPath, "expected endpoint")

    expect_equal(nrow(AOIs), 68, info = "study should contain 68 AOIs")
    expect_named(AOIs, c("stimulusId", "stimulusName", "id", "name", "type", "group", "area"),
                 info = "AOIs infos not matching")

    # Dynamic AOIs should have NA instead of area values (0 should be converted)
    expect_identical(unique(AOIs[type == "Dynamic", ]$area), NA_real_, "area should be NA")

    # Group can be either missing or filled
    expect_identical(unique(AOIs$group), c("Rectangle", NA_character_, "Test"), "group is wrong")
})

test_that("remote return - AOIs data.table", {
    studyAOIsPath <- "../data/studyAOIs_cloud.json"

    # Should return all well formed AOIs from this study if no stimulus is provided (removing 2 malformed studies)
    AOIs <- mockedPrivateAoiFormatting(study_cloud, studyAOIsPath, "expected endpoint", remove_saved_AOIs = FALSE)
    expect_equal(nrow(AOIs), 8, info = "study should contain 8 AOIs")
    expect_named(AOIs, c("stimulusId", "stimulusName", "id", "name", "type", "group", "area", "displayColor", "fileId",
                         "timelineType", "updatedDate", "aoiStatsNeedCalculating"), info = "AOIs infos not matching")

    # Dynamic AOIs should have NA instead of area values, Static AOIs should be computed correctly
    expect_equal(AOIs$area[7:8], c(NA_real_, 725886), tolerance = 0.001, infos = "area should be valid")

    # Group can be either missing or filled
    expect_identical(unique(AOIs$group), NA_character_, "group is wrong")

    # For online we save the AOIs in the study object as there is only one endpoint for the whole study
    expect_identical(attr(study_cloud, "AOIs"), AOIs, "AOIs are not stored correctly")

    # Trying the call again should bypass the getJSON call
    AOIs <- mockedPrivateAoiFormatting(study_cloud, studyAOIsPath, "expected endpoint", expectedCallGetJSON = 0)
})

# privateAoiFiltering =================================================================================================
context("privateAoiFiltering()")

# Replace url to load test data
mockUrl <- function(study, url) {
    if (grepl("respondent", url)) {
        if (study$connection$localIM) {
            return(respondentAOIsPath)
        } else {
            return(respondentAOIsCloudPath)
        }
    } else if (grepl("stimuli", url)) {
        return(stimulusAOIsPath)
    } else {
        if (study$connection$localIM) {
            return(studyAOIsPath)
        } else {
            return(studyAOIsCloudPath)
        }
    }
}

mockedPrivateAoiFiltering <- function(study, stimulus = NULL, respondent = NULL, expectedCallAoiDetails = 0,
                                      fail = FALSE) {

    # Get expected endpoint
    if (is.null(stimulus) && is.null(respondent)) {
        expectedEndpoint <- paste("study:", study$name)
    } else if (!is.null(stimulus) && is.null(respondent)) {
        expectedEndpoint <- paste("stimulus:", stimulus$name)
    } else if (!is.null(respondent)) {
        expectedEndpoint <- paste("respondent:", respondent$name)
    }

    if (!is.null(stimulus) && !is.null(respondent)) {
        expectedUrl <- getAoisUrl(study, NULL, respondent$id)
    } else {
        expectedUrl <- getAoisUrl(study, stimulus$id, respondent$id)
    }

    privateGetAoiDetails_Stub <- mock(jsonlite::fromJSON("../data/AOIDetails_processed_cloud.json"),
                                      jsonlite::fromJSON("../data/AOIDetails_processed_cloud2.json"), cycle = TRUE)

    if (fail) {
        privateGetAoiDetails_Stub <- mock(jsonlite::fromJSON("../data/AOIDetails_failed.json"), cycle = TRUE)
    }

    privateAoiFormatting_Stub <- mock(mockedPrivateAoiFormatting(study, mockUrl(study, expectedUrl), expectedEndpoint))

    AOIs <- mockr::with_mock(privateAoiFormatting = privateAoiFormatting_Stub,
                             privateGetAoiDetails = privateGetAoiDetails_Stub, {
                                 privateAoiFiltering(study, stimulus, respondent)
                             })

    expect_args(privateAoiFormatting_Stub, 1, study, AOIsUrl = expectedUrl, endpoint =  expectedEndpoint)

    if (!study$connection$localIM && !is.null(respondent)) {
        expect_called(privateGetAoiDetails_Stub, expectedCallAoiDetails)
    }

    return(AOIs)
}


test_that("local warning - no AOIs defined", {
    # Should return a warning when no AOI have been defined at the study level
    expect_warning(AOIs <- mockedPrivateAoiFiltering(study), "No AOI defined for study: 2 GSR 81",
                   info = "no AOI warning should have been thrown for this study")

    expect_null(AOIs, "AOIs should be null")

    # Should return a warning when no AOI have been defined at the stimulus level
    expect_warning(AOIs <- mockedPrivateAoiFiltering(study, stimuli[1, ]),
                   "No AOI defined for stimulus: AntiSmoking40Sec",
                   info = "no AOI warning should have been thrown for this stimulus")

    expect_null(AOIs, "AOIs should be null")

    # Should return a warning when no AOI have been defined at the respondent level
    expect_warning(AOIs <- mockedPrivateAoiFiltering(study, respondent = respondents[1, ]),
                   "No AOI defined for respondent: Wendy",
                   info = "no AOI warning should have been thrown for this respondent")

    expect_null(AOIs, "AOIs should be null")
})


test_that("remote warning - no AOIs defined for the study", {
    # Should return a warning when no AOI have been defined at the study level
    expect_warning(AOIs <- mockedPrivateAoiFiltering(study_cloud), "No AOI defined for study: RRRock The R",
                   info = "no AOI warning should have been thrown for this study")

    expect_null(AOIs, "AOIs should be null")

    # Should return a warning when no AOI have been defined at the stimulus level
    expect_warning(AOIs <- mockedPrivateAoiFiltering(study_cloud, stimuli[1, ]),
                   "No AOI defined for stimulus: AntiSmoking40Sec",
                   info = "no AOI warning should have been thrown for this stimulus")

    expect_null(AOIs, "AOIs should be null")

    # Should return a warning when no AOI have been defined at the respondent level
    expect_warning(AOIs <- mockedPrivateAoiFiltering(study_cloud, respondent = respondents[1, ]),
                   "No AOI defined for respondent: Wendy",
                   info = "no AOI warning should have been thrown for this respondent")

    expect_null(AOIs, "AOIs should be null")
})

studyAOIsPath <- "../data/studyAOIs.json"
stimulusAOIsPath <- "../data/stimulusAOIs.json"
respondentAOIsPath <- "../data/respondentAOIs.json"
studyAOIsCloudPath <- "../data/studyAOIs_cloud.json"
respondentAOIsCloudPath <- "../data/respondentAOIs_cloud.json"

test_that("local warning - no AOIs for a specific respondent/stimulus pair", {
    # if no AOIs is present for a specific respondent/stimulus pair, should return the correct warning
    expect_warning(AOIs <- mockedPrivateAoiFiltering(study, respondent = respondents[1, ], stimulus = stimuli[1, ]),
                   "No AOI defined for respondent: Wendy, stimulus: AntiSmoking40Sec",
                   info = "no AOI warning should have been thrown for this respondent/stimulus")

    expect_null(AOIs, "AOIs should be null")
})

test_that("remote warning - no AOIs for a specific respondent/stimulus pair or a specific stimulus", {
    # if no AOIs is present for a specific respondent/stimulus pair in remote study, should return the correct warning
    expect_warning(AOIs <- mockedPrivateAoiFiltering(study_cloud, respondent = respondents[1, ],
                                                     stimulus = stimuli[1, ], expectedCallAoiDetails = 0),
                   "No AOI defined for respondent: Wendy, stimulus: AntiSmoking40Sec",
                   info = "no AOI warning should have been thrown for this respondent/stimulus")

    expect_null(AOIs, "AOIs should be null")

    # if no AOIs is present for a specific stimulus in remote study, should return the correct warning
    expect_warning(AOIs <- mockedPrivateAoiFiltering(study_cloud, stimuli[1, ]),
                   "No AOI defined for stimulus: AntiSmoking40Sec",
                   info = "no AOI warning should have been thrown for this stimulus")

    expect_null(AOIs, "AOIs should be null")
})


test_that("local return - filtered AOIs data.table", {
    # Should return all AOIs from this study if no stimulus is provided
    AOIs <- mockedPrivateAoiFiltering(study)
    expect_equal(nrow(AOIs), 68, info = "study should contain 68 AOIs")

    # Should return all AOIs from this study for a specific stimulus
    AOIs <- mockedPrivateAoiFiltering(study, stimulus = stimuli[1, ])
    expect_equal(nrow(AOIs), 3, info = "stimulus should contain 3 AOIs")

    # Should return all AOIs from this study for a specific respondent
    AOIs <- mockedPrivateAoiFiltering(study, respondent = respondents[1, ])
    expect_equal(nrow(AOIs), 4, info = "study for this respondent should contain 4 AOIs")

    # Should return all AOIs from this study for a specific respondent and stimulus
    AOIs <- mockedPrivateAoiFiltering(study, stimulus = stimuli[4, ], respondent = respondents[1, ])
    expect_equal(nrow(AOIs), 4, info = "combination should contain 4 AOIs")
})

test_that("remote return - filtered AOIs data.table", {
    stimuli <- getStimuli(study_cloud)
    respondents <- getRespondents(study_cloud)

    # Should return all AOIs from this study if no stimulus is provided
    AOIs <- mockedPrivateAoiFiltering(study_cloud)
    expect_equal(nrow(AOIs), 8, info = "study should contain 8 AOIs")

    # Should return all AOIs from this study for a specific stimulus
    AOIs <- mockedPrivateAoiFiltering(study_cloud, stimulus = stimuli[15, ])
    expect_equal(nrow(AOIs), 3, info = "stimulus should contain 3 AOI")

    # Should return all AOIs from this study for a specific respondent
    AOIs <- mockedPrivateAoiFiltering(study_cloud, respondent = respondents[1, ], expectedCallAoiDetails = 8)
    expect_equal(nrow(AOIs), 8, info = "study for this respondent should contain 8 AOIs")

    # Should return all AOIs from this study for a specific respondent and stimulus
    AOIs <- mockedPrivateAoiFiltering(study_cloud, stimulus = stimuli[15, ], respondent = respondents[1, ],
                                      expectedCallAoiDetails = 3)

    expect_equal(nrow(AOIs), 3, info = "combination should contain 2 AOIs")
})

test_that("remote warning - no in/out was generated", {
    stimuli <- getStimuli(study_cloud)
    respondents <- getRespondents(study_cloud)

    # Should fail without issue in case no in/out is present and just return no AOIs
    expect_warning(
        AOIs <- mockedPrivateAoiFiltering(study_cloud, stimulus = stimuli[15, ], respondent = respondents[1, ],
                                          expectedCallAoiDetails = 3, fail = TRUE),
        "No AOI defined for respondent: bab55356-43fc-4c25-a39d-a1d513965614, stimulus: Pool-lounge-view-from-atop",
        info = "no AOI warning should have been thrown for this respondent"
    )

    expect_null(AOIs, "AOIs should be null")
})

# getAois =============================================================================================================
context("getAois()")

mockedGetAois <- function(study, stimulus = NULL, respondent = NULL, generateInOutFiles = FALSE,
                          expectedCallAoiDetails = 0, expectedCallFiltering = 0, fail = FALSE, verbose = TRUE,
                          wrong_id = FALSE) {

    privateAoiFiltering_Stub <- mock(mockedPrivateAoiFiltering(study, stimulus, respondent, expectedCallFiltering))

    # Create the different failing usecases
    aoi_details <- jsonlite::fromJSON("../data/AOIDetailsForStimulusRespondent.json")

    if (fail) {
        aoi_details <- NULL
    } else if (wrong_id) {
        aoi_details$aoiId <- "wrong_id"
    }

    privateGetAoiDetails_Stub <- mock(aoi_details)

    AOIs <- mockr::with_mock(privateAoiFiltering = privateAoiFiltering_Stub,
                             privateGetAoiDetails = privateGetAoiDetails_Stub, {
                                 getAois(study, stimulus, respondent, generateInOutFiles, verbose)
                             })

    expect_args(privateAoiFiltering_Stub, 1, study, stimulus, respondent)

    expect_called(privateGetAoiDetails_Stub, expectedCallAoiDetails)

    if (expectedCallAoiDetails > 0) {
        expect_args(privateGetAoiDetails_Stub, 1, study = study, imObject = stimulus, respondent = respondent)
    }

    return(AOIs)
}

test_that("error/warning - arguments are missing or not from the good class", {
    # in case of missing study
    expect_error(getAois(), "Please specify a study loaded with `imStudy()`", fixed = TRUE,
                 info = "missing `study` param not handled properly")

    # in case of study that is not an imStudy object
    expect_error(getAois(study = "whatever"), "`study` argument is not an imStudy object",
                 info = "study not being an imStudy object should throw an error")

    # in case of respondent that is not an imStimulus object
    expect_error(getAois(study, stimulus = "whatever"), "`stimulus` argument is not an imStimulus object",
                 info = "stimulus not being an imStimulus object should throw an error")

    # in case of stimulus that is not an imStimulus object
    expect_error(getAois(study, stimuli[1, ], respondent = "whatever"),
                 "`respondent` argument is not an imRespondent object",
                 info = "respondent not being an imRespondent object should throw an error")

    # in case generateInOutFiles is true and not both respondent and stimulus are provided
    expect_warning(mockedGetAois(study, stimuli[1, ], generateInOutFiles = TRUE),
                   "InOut files can only be generated when both respondent and stimulus argument are provided.",
                   info = "respondent argument must be provided")

    expect_warning(mockedGetAois(study, stimulus = NULL, respondents[1, ], generateInOutFiles = TRUE),
                   "InOut files can only be generated when both respondent and stimulus argument are provided.",
                   info = "stimulus argument must be provided")
})

test_that("check - verbose parameter", {
    # Private filtering warnings should still be shown by default
    expect_warning(AOIs <- mockedGetAois(study, respondent = respondents[1, ], stimulus = stimuli[1, ]),
                   "No AOI defined for respondent: Wendy, stimulus: AntiSmoking40Sec",
                   info = "private filtering warning should be shown when verbose = TRUE")

    expect_null(AOIs, "AOIs should be null")

    # The same private filtering warning should be silenced when verbose = FALSE
    expect_no_warning(AOIs <- mockedGetAois(study, respondent = respondents[1, ], stimulus = stimuli[1, ],
                                            verbose = FALSE))

    expect_null(AOIs, "AOIs should be null")

    # Warnings emitted by getAois() itself should still be shown
    expect_warning(mockedGetAois(study, respondent = respondents[1, ], generateInOutFiles = TRUE, verbose = FALSE),
                   "InOut files can only be generated when both respondent and stimulus argument are provided.",
                   info = "getAois warning should still be shown when verbose = FALSE")
})

test_that("return - imAOIList object", {
    # Should return all AOIs from this study if no stimulus is provided
    AOIs <- mockedGetAois(study)

    expect_s3_class(AOIs, "imAOIList")
    expect_equal(nrow(AOIs), 68, info = "study should contain 68 AOIs")
    expect_named(AOIs, c("stimulusId", "stimulusName", "id", "name", "type", "group", "area"),
                 info = "AOIs infos not matching")

    # Dynamic AOIs should have NA instead of area values (0 should be converted)
    expect_identical(unique(AOIs[type == "Dynamic", ]$area), NA_real_, "area should be NA")

    # Group can be either missing or filled
    expect_identical(unique(AOIs$group), c("Rectangle", NA_character_, "Test"), "group is wrong")

    # check that taking only one AOI changes the class of the object
    AOI <- AOIs[1, ]
    expect_s3_class(AOI, "imAOI")

    # check that only taking ids of the list of stimuli changes the class of the object
    AOIs <- AOIs[, c("name", "id")]
    expect_s3_class(AOIs, c("data.table", "data.frame"), exact = TRUE)
})


test_that("check - generateInOutFiles parameter", {
    # Should return all AOIs from this study for a specific respondent and stimulus
    AOIs <- mockedGetAois(study, stimulus = stimuli[4, ], respondent = respondents[1, ], generateInOutFiles = TRUE, 1)

    expect_s3_class(AOIs, "imAOIList")
    expect_equal(nrow(AOIs), 4, info = "combination should contain 4 AOIs")
    expect_named(AOIs, c("stimulusId", "stimulusName", "id", "name", "type", "group", "area", "respId", "fileId",
                         "resultId"), info = "AOIs infos not matching")

    # If stimulus is missing we shouldn't add any filepaths
    expect_warning(AOIs <- mockedGetAois(study, respondent = respondents[1, ], generateInOutFiles = TRUE))

    expect_s3_class(AOIs, "imAOIList")
    expect_equal(nrow(AOIs), 4, info = "study for this respondent should contain 4 AOIs")
    expect_named(AOIs, c("stimulusId", "stimulusName", "id", "name", "type", "group", "area"),
                 info = "AOIs infos not matching")

    # If respondent is missing we shouldn't add any filepaths
    expect_warning(AOIs <- mockedGetAois(study, stimulus = stimuli[4, ], generateInOutFiles = TRUE))

    expect_s3_class(AOIs, "imAOIList")
    expect_equal(nrow(AOIs), 3, info = "study for this stimulus should contain 3 AOIs")
    expect_named(AOIs, c("stimulusId", "stimulusName", "id", "name", "type", "group", "area"),
                 info = "AOIs infos not matching")

    # If the in/out generation failed we should not return any AOIs
    AOIs <- mockedGetAois(study, stimulus = stimuli[4, ], respondent = respondents[1, ], generateInOutFiles = TRUE,
                          expectedCallAoiDetails = 1, fail = TRUE)

    expect_null(AOIs, "No AOIs should be returned")

    # If in/out details do not match any AOIs we should not return any AOIs
    expect_warning(AOIs <- mockedGetAois(study, stimulus = stimuli[4, ], respondent = respondents[1, ],
                                         generateInOutFiles = TRUE, expectedCallAoiDetails = 1, wrong_id = TRUE),
                   "The InOut files found do not match any AOIs for this respondent/stimulus combination.",
                   info = "A mismatch between AOIs and generated in/out files should send a warning")

    expect_null(AOIs, "No AOIs should be returned when in/out details do not match any AOIs")

    # For remote study, should have the in/out data in case a respondent was provided
    stimuli <- getStimuli(study_cloud)
    respondents <- getRespondents(study_cloud)
    AOIs <- mockedGetAois(study_cloud, stimulus = stimuli[15, ], respondent = respondents[1, ],
                          generateInOutFiles = TRUE, expectedCallFiltering = 3)

    expect_s3_class(AOIs, "imAOIList")
    expect_equal(nrow(AOIs), 3, info = "combination should contain 3 AOIs")
    expect_named(AOIs, c("stimulusId", "stimulusName", "id", "name", "type", "group", "area", "displayColor", "fileId",
                         "timelineType", "updatedDate", "aoiStatsNeedCalculating", "startMediaOffset", "aoiInOuts"),
                 info = "AOIs infos not matching")

    # For remote study, should not have the in/out data in case no respondent was provided
    expect_warning(AOIs <- mockedGetAois(study_cloud, stimulus = stimuli[15, ], generateInOutFiles = TRUE))

    expect_s3_class(AOIs, "imAOIList")
    expect_equal(nrow(AOIs), 3, info = "combination should contain 3 AOIs")
    expect_named(AOIs, c("stimulusId", "stimulusName", "id", "name", "type", "group", "area", "displayColor", "fileId",
                         "timelineType", "updatedDate", "aoiStatsNeedCalculating"), info = "AOIs infos not matching")
})

studyAOIsPath <- "../data/studyAOI.json"

test_that("return - imAOI object in case of only one AOI ", {
    # Should return only one AOI from this study
    AOIs <- mockedGetAois(study)

    expect_s3_class(AOIs, "imAOI")
    expect_equal(nrow(AOIs), 1, info = "AOIs should only contain a single AOI")
    expect_identical(AOIs$stimulusId, "1000", "AOI parent id is not matching")
    expect_identical(AOIs$stimulusName, "AntiSmoking40Sec", "AOI parent name is not matching")
    expect_identical(AOIs$name, "AOI 1", "AOI name is not matching")
    expect_identical(AOIs$id, "19884f0d-a730-432a-9843-366d0437b456", "AOI id is not matching")
    expect_identical(AOIs$type, "Dynamic", "AOI type is not matching")
    expect_identical(AOIs$group, NA_character_, "AOI group is not matching")
    expect_identical(AOIs$area, 20.534, "AOI area is not matching")
})

# getAoi ==============================================================================================================
context("getAoi()")

AOIId <- "a966ada8-2428-4748-91d8-884f7b31eebf"

mockedGetAoi <- function(study, AOIId) {
    mockr::with_mock(getAois = mockedGetAois, {
        getAoi(study, AOIId)
    })
}

studyAOIsPath <- "../data/noAOIs.json"

test_that("error/warning - arguments are missing or no AOIs in the study", {
    # in case of missing AOI id
    expect_error(getAoi(study), "Please specify an AOIId. Available AOIs can be found with `getAois()`",
                 fixed = TRUE, info = "missing `AOIId` param not handled properly")

    # in case of no AOI
    expect_warning(AOI <- mockedGetAoi(study, AOIId), "No AOI defined for study: 2 GSR 81",
                   info = "no AOIs not handled properly")

    expect_null(AOI, "AOI should be null")
})

studyAOIsPath <- "../data/studyAOIs.json"

test_that("warning - wrong AOIId is provided", {
    expect_warning(AOI <- mockedGetAoi(study, AOIId = "1010"), "No AOIs found matching id: 1010",
                   info = "wrong `AOIId` param not handled properly")

    expect_null(AOI, "AOI should be null")
})

test_that("return - specific imAOI object", {
    AOI <- mockedGetAoi(study, AOIId)

    expect_s3_class(AOI, "imAOI")
    expect_equal(nrow(AOI), 1, info = "should only contain a single AOI")
    expect_identical(AOI$id, AOIId, "AOI id is not matching")

    # print should work as expected
    expect_output(print(AOI), "iMotions AOI `Blue` with ID = a966ada8-2428-4748-91d8-884f7b31eebf")
    expect_output(print(AOI[name == "Test", ]), "No iMotions AOI found")
})
