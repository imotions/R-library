# privateCalculateAreaAOI =============================================================================================
context("privateCalculateAreaAOI()")

library(mockery)

test_that("return - AOI area", {
  # Case of a square
  x <- c(0, 5, 5, 0)
  y <- c(5, 5, 0, 0)
  area <- privateCalculateAreaAOI(x, y)
  expect_equal(area, 25, info = "wrong computation for square")

  # Case of a rectangle
  x <- c(0, 5, 5, 0)
  y <- c(1, 1, 0, 0)
  area <- privateCalculateAreaAOI(x, y)
  expect_equal(area, 5, info = "wrong computation for rectangle")

  # Case of an empty shape
  x <- c(0, 5, 5, 0)
  y <- c(0, 0, 0, 0)
  area <- privateCalculateAreaAOI(x, y)
  expect_equal(area, 0, info = "wrong computation for empty shape")
})

# privateAOIFormatting ================================================================================================
context("privateAOIFormatting()")

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

mockedPrivateAOIFormatting <- function(study, AOIsUrl, endpoint) {
  getJSON_Stub <- mock(jsonlite::fromJSON(AOIsUrl))

  AOIs <- mockr::with_mock(
    getJSON = getJSON_Stub, {
      privateAOIFormatting(study, AOIsUrl, endpoint)
    })

  expect_args(getJSON_Stub, 1, connection = study$connection, url = AOIsUrl,
              message = paste("Retrieving AOIs for", endpoint))

  return(AOIs)
}


test_that("warning - no AOI defined", {
  # Should return a warning when no AOI have been defined
  expect_warning(mockedPrivateAOIFormatting(study, studyAOIsPath, "expected endpoint"),
                 "No AOI defined for expected endpoint",
                 info = "no AOI warning should have been thrown")
})

test_that("local return - AOIs data.table", {
  studyAOIsPath <- "../data/studyAOIs.json"

  # Should return all AOIs from this study if no stimulus is provided
  AOIs <- mockedPrivateAOIFormatting(study, studyAOIsPath, "expected endpoint")

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

  # Should return all AOIs from this study if no stimulus is provided
  AOIs <- mockedPrivateAOIFormatting(study_cloud, studyAOIsPath, "expected endpoint")
  expect_equal(nrow(AOIs), 2, info = "study should contain 2 AOIs")
  expect_named(AOIs, c("stimulusId", "stimulusName", "id", "name", "type", "group", "area", "fileId",
                       "aoiStatsNeedCalculating", "updatedDate"), info = "AOIs infos not matching")

  # Dynamic AOIs should have NA instead of area values, Static AOIs should be computed correctly
  expect_equal(AOIs$area, c(32897, NA_real_), tolerance = 0.001, infos = "area should be valid")

  # Group can be either missing or filled
  expect_identical(unique(AOIs$group), NA_character_, "group is wrong")
})

# privateAOIFiltering =================================================================================================
context("privateAOIFiltering()")

mockedPrivateAOIFiltering <- function(study, stimulus = NULL, respondent = NULL) {
    # Get expected endpoint
    if (is.null(stimulus) && is.null(respondent)) {
        expectedEndpoint <- paste("study:", study$name)
    } else if (!is.null(stimulus) && is.null(respondent)) {
        expectedEndpoint <- paste("stimulus:", stimulus$name)
    } else if (!is.null(respondent)) {
        expectedEndpoint <- paste("respondent:", respondent$name)
    }

    # Replace url to load test data
    mockUrl <- function(url) {
        if (grepl("respondent", url)) {
            return(respondentAOIsPath)
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

    if (!is.null(stimulus) && !is.null(respondent)) {
        expectedUrl <- getAOIsUrl(study, NULL, respondent$id)
    } else {
        expectedUrl <- getAOIsUrl(study, stimulus$id, respondent$id)
    }

    privateAOIFormatting_Stub <- mock(mockedPrivateAOIFormatting(study, mockUrl(expectedUrl), expectedEndpoint))

    AOIs <- mockr::with_mock(
      privateAOIFormatting = privateAOIFormatting_Stub, {
          privateAOIFiltering(study, stimulus, respondent)
      })

    expect_args(privateAOIFormatting_Stub, 1, study, AOIsUrl = expectedUrl, endpoint =  expectedEndpoint)
    return(AOIs)
}


test_that("local warning - no AOIs defined", {
    # Should return a warning when no AOI have been defined at the study level
    expect_warning(AOIs <- mockedPrivateAOIFiltering(study), "No AOI defined for study: 2 GSR 81",
                   info = "no AOI warning should have been thrown for this study")

    expect_null(AOIs, "AOIs should be null")

    # Should return a warning when no AOI have been defined at the stimulus level
    expect_warning(AOIs <- mockedPrivateAOIFiltering(study, stimuli[1, ]),
                   "No AOI defined for stimulus: AntiSmoking40Sec",
                   info = "no AOI warning should have been thrown for this stimulus")

    expect_null(AOIs, "AOIs should be null")

    # Should return a warning when no AOI have been defined at the respondent level
    expect_warning(AOIs <- mockedPrivateAOIFiltering(study, respondent = respondents[1, ]),
                   "No AOI defined for respondent: Wendy",
                   info = "no AOI warning should have been thrown for this respondent")

    expect_null(AOIs, "AOIs should be null")
})


test_that("remote warning - no AOIs defined for the study", {
  # Should return a warning when no AOI have been defined at the study level
  expect_warning(AOIs <- mockedPrivateAOIFiltering(study_cloud), "No AOI defined for study: RRRock The R",
                 info = "no AOI warning should have been thrown for this study")

  expect_null(AOIs, "AOIs should be null")

  # Should return a warning when no AOI have been defined at the stimulus level
  expect_warning(AOIs <- mockedPrivateAOIFiltering(study_cloud, stimuli[1, ]),
                 "No AOI defined for stimulus: AntiSmoking40Sec",
                 info = "no AOI warning should have been thrown for this stimulus")

  expect_null(AOIs, "AOIs should be null")

  # Should return a warning when no AOI have been defined at the respondent level
  expect_warning(AOIs <- mockedPrivateAOIFiltering(study_cloud, respondent = respondents[1, ]),
                 "No AOI defined for respondent: Wendy",
                 info = "no AOI warning should have been thrown for this respondent")

  expect_null(AOIs, "AOIs should be null")
})

studyAOIsPath <- "../data/studyAOIs.json"
stimulusAOIsPath <- "../data/stimulusAOIs.json"
respondentAOIsPath <- "../data/respondentAOIs.json"
studyAOIsCloudPath <- "../data/studyAOIs_cloud.json"

test_that("local warning - no AOIs for a specific respondent/stimulus pair", {
    # if no AOIs is present for a specific respondent/stimulus pair, should return the correct warning
    expect_warning(AOIs <- mockedPrivateAOIFiltering(study, respondent = respondents[1, ], stimulus = stimuli[1, ]),
                   "No AOI defined for respondent: Wendy, stimulus: AntiSmoking40Sec",
                   info = "no AOI warning should have been thrown for this respondent/stimulus")

    expect_null(AOIs, "AOIs should be null")
})

test_that("remote warning - no AOIs for a specific respondent/stimulus pair or a specific stimulus", {
  # if no AOIs is present for a specific respondent/stimulus pair in remote study, should return the correct warning
  expect_warning(AOIs <- mockedPrivateAOIFiltering(study_cloud, respondent = respondents[1, ], stimulus = stimuli[1, ]),
                 "No AOI defined for respondent: Wendy, stimulus: AntiSmoking40Sec",
                 info = "no AOI warning should have been thrown for this respondent/stimulus")

  expect_null(AOIs, "AOIs should be null")

  # if no AOIs is present for a specific stimulus in remote study, should return the correct warning
  expect_warning(AOIs <- mockedPrivateAOIFiltering(study_cloud, stimuli[1, ]),
                 "No AOI defined for stimulus: AntiSmoking40Sec",
                 info = "no AOI warning should have been thrown for this stimulus")

  expect_null(AOIs, "AOIs should be null")
})


test_that("local return - filtered AOIs data.table", {
  # Should return all AOIs from this study if no stimulus is provided
  AOIs <- mockedPrivateAOIFiltering(study)
  expect_equal(nrow(AOIs), 68, info = "study should contain 68 AOIs")

  # Should return all AOIs from this study for a specific stimulus
  AOIs <- mockedPrivateAOIFiltering(study, stimulus = stimuli[1, ])
  expect_equal(nrow(AOIs), 3, info = "stimulus should contain 3 AOIs")

  # Should return all AOIs from this study for a specific respondent
  AOIs <- mockedPrivateAOIFiltering(study, respondent = respondents[1, ])
  expect_equal(nrow(AOIs), 4, info = "study for this respondent should contain 4 AOIs")

  # Should return all AOIs from this study for a specific respondent and stimulus
  AOIs <- mockedPrivateAOIFiltering(study, stimulus = stimuli[4, ], respondent = respondents[1, ])
  expect_equal(nrow(AOIs), 4, info = "combination should contain 4 AOIs")
})

test_that("remote return - filtered AOIs data.table", {
  stimuli <- getStimuli(study_cloud)
  respondents <- getRespondents(study_cloud)

  # Should return all AOIs from this study if no stimulus is provided
  AOIs <- mockedPrivateAOIFiltering(study_cloud)
  expect_equal(nrow(AOIs), 2, info = "study should contain 2 AOIs")

  # Should return all AOIs from this study for a specific stimulus
  AOIs <- mockedPrivateAOIFiltering(study_cloud, stimulus = stimuli[14, ])
  expect_equal(nrow(AOIs), 1, info = "stimulus should contain 1 AOI")

  # Should return all AOIs from this study for a specific respondent
  AOIs <- mockedPrivateAOIFiltering(study_cloud, respondent = respondents[1, ])
  expect_equal(nrow(AOIs), 2, info = "study for this respondent should contain 2 AOIs")

  # Should return all AOIs from this study for a specific respondent and stimulus
  AOIs <- mockedPrivateAOIFiltering(study_cloud, stimulus = stimuli[14, ], respondent = respondents[1, ])
  expect_equal(nrow(AOIs), 1, info = "combination should contain 1 AOIs")
})

# getAOIs =============================================================================================================
context("getAOIs()")

mockedGetAOIs <- function(study, stimulus = NULL, respondent = NULL, generateInOutFiles = FALSE,
                          expectedCallAoiDetails = 0) {

    privateAOIFiltering_Stub <- mock(mockedPrivateAOIFiltering(study, stimulus, respondent))
    privateGetAOIDetails_Stub <- mock(jsonlite::fromJSON("../data/AOIDetailsForStimulusRespondent.json"))

    AOIs <- mockr::with_mock(
        privateAOIFiltering = privateAOIFiltering_Stub,
        privateGetAOIDetails = privateGetAOIDetails_Stub, {
            getAOIs(study, stimulus, respondent, generateInOutFiles)
        })

    expect_args(privateAOIFiltering_Stub, 1, study, stimulus, respondent)
    expect_called(privateGetAOIDetails_Stub, expectedCallAoiDetails)

    if (expectedCallAoiDetails > 0) {
        expect_args(privateGetAOIDetails_Stub, 1, study = study, imObject = stimulus, respondent = respondent)
    }

    return(AOIs)
}

test_that("error/warning - arguments are missing or not from the good class", {
    # in case of missing study
    expect_error(getAOIs(), "Please specify a study loaded with `imStudy()`", fixed = TRUE,
                 info = "missing `study` param not handled properly")

    # in case of study that is not an imStudy object
    expect_error(getAOIs(study = "whatever"), "`study` argument is not an imStudy object",
                 info = "study not being an imStudy object should throw an error")

    # in case of respondent that is not an imStimulus object
    expect_error(getAOIs(study, stimulus = "whatever"), "`stimulus` argument is not an imStimulus object",
                 info = "stimulus not being an imStimulus object should throw an error")

    # in case of stimulus that is not an imStimulus object
    expect_error(getAOIs(study, stimuli[1, ], respondent = "whatever"),
                 "`respondent` argument is not an imRespondent object",
                 info = "respondent not being an imRespondent object should throw an error")

    # in case generateInOutFiles is true and not both respondent and stimulus are provided
    expect_warning(mockedGetAOIs(study, stimuli[1, ], generateInOutFiles = TRUE),
                   "InOut files can only be generated when both respondent and stimulus argument are provided.",
                   info = "respondent argument must be provided")

    expect_warning(mockedGetAOIs(study, stimulus = NULL, respondents[1, ], generateInOutFiles = TRUE),
                   "InOut files can only be generated when both respondent and stimulus argument are provided.",
                   info = "stimulus argument must be provided")
})

test_that("return - imAOIList object", {
    # Should return all AOIs from this study if no stimulus is provided
    AOIs <- mockedGetAOIs(study)

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
  AOIs <- mockedGetAOIs(study, stimulus = stimuli[4, ], respondent = respondents[1, ], generateInOutFiles = TRUE, 1)

  expect_s3_class(AOIs, "imAOIList")
  expect_equal(nrow(AOIs), 4, info = "combination should contain 4 AOIs")
  expect_named(AOIs, c("stimulusId", "stimulusName", "id", "name", "type", "group", "area", "respId", "fileId",
                       "resultId"), info = "AOIs infos not matching")

  # If stimulus is missing we shouldn't add any filepaths
  expect_warning(AOIs <- mockedGetAOIs(study, respondent = respondents[1, ], generateInOutFiles = TRUE))

  expect_s3_class(AOIs, "imAOIList")
  expect_equal(nrow(AOIs), 4, info = "study for this respondent should contain 4 AOIs")
  expect_named(AOIs, c("stimulusId", "stimulusName", "id", "name", "type", "group", "area"),
               info = "AOIs infos not matching")

  # If respondent is missing we shouldn't add any filepaths
  expect_warning(AOIs <- mockedGetAOIs(study, stimulus = stimuli[4, ], generateInOutFiles = TRUE))

  expect_s3_class(AOIs, "imAOIList")
  expect_equal(nrow(AOIs), 3, info = "study for this stimulus should contain 3 AOIs")
  expect_named(AOIs, c("stimulusId", "stimulusName", "id", "name", "type", "group", "area"),
               info = "AOIs infos not matching")

  # For remote study, should skip the InOutFile generation
  stimuli <- getStimuli(study_cloud)
  respondents <- getRespondents(study_cloud)
  AOIs <- mockedGetAOIs(study_cloud, stimulus = stimuli[14, ], respondent = respondents[1, ], generateInOutFiles = TRUE)

  expect_s3_class(AOIs, "imAOI")
  expect_equal(nrow(AOIs), 1, info = "combination should contain 1 AOI")
  expect_named(AOIs, c("stimulusId", "stimulusName", "id", "name", "type", "group", "area", "fileId",
                      "aoiStatsNeedCalculating", "updatedDate"), info = "AOIs infos not matching")
})

studyAOIsPath <- "../data/studyAOI.json"

test_that("return - imAOI object in case of only one AOI ", {
    # Should return only one AOI from this study
    AOIs <- mockedGetAOIs(study)

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

# getAOI ==============================================================================================================
context("getAOI()")

AOIId <- "a966ada8-2428-4748-91d8-884f7b31eebf"

mockedGetAOI <- function(study, AOIId) {
  mockr::with_mock(
    getAOIs = mockedGetAOIs, {
        getAOI(study, AOIId)
    })
}

studyAOIsPath <- "../data/noAOIs.json"

test_that("error/warning - arguments are missing or no AOIs in the study", {
    # in case of missing AOI id
    expect_error(getAOI(study), "Please specify an AOIId. Available AOIs can be found with `getAOIs()`",
                 fixed = TRUE, info = "missing `AOIId` param not handled properly")

    # in case of no AOI
    expect_warning(AOI <- mockedGetAOI(study, AOIId), "No AOI defined for study: 2 GSR 81",
                   info = "no AOIs not handled properly")

    expect_null(AOI, "AOI should be null")
})

studyAOIsPath <- "../data/studyAOIs.json"

test_that("warning - wrong AOIId is provided", {
    expect_warning(AOI <- mockedGetAOI(study, AOIId = "1010"), "No AOIs found matching id: 1010",
                   info = "wrong `AOIId` param not handled properly")

    expect_null(AOI, "AOI should be null")
})

test_that("return - specific imAOI object", {
    AOI <- mockedGetAOI(study, AOIId)

    expect_s3_class(AOI, "imAOI")
    expect_equal(nrow(AOI), 1, info = "should only contain a single AOI")
    expect_identical(AOI$id, AOIId, "AOI id is not matching")

    # print should work as expected
    expect_output(print(AOI), "iMotions AOI `Blue` with ID = a966ada8-2428-4748-91d8-884f7b31eebf")
    expect_output(print(AOI[name == "Test", ]), "No iMotions AOI found")
})
