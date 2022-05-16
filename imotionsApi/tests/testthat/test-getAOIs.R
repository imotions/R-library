context("privateAOIFiltering()")

library("imotionsApi")
library("mockery")

# Load study and stimuli
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))

stimuli <- getStimuli(study)
respondents <- getRespondents(study)

studyAOIsPath <- "../data/noAOIs.json"
stimulusAOIsPath <- "../data/noAOIs.json"
respondentAOIsPath <- "../data/noAOIs.json"

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
            return(studyAOIsPath)
        }
    }

    if (!is.null(stimulus) && !is.null(respondent)) {
        expectedUrl <- getAOIsUrl(study, NULL, respondent$id)
    } else {
        expectedUrl <- getAOIsUrl(study, stimulus$id, respondent$id)
    }

    getJSON_Stub <- mock(jsonlite::fromJSON(mockUrl(expectedUrl)))

    AOIs <- mockr::with_mock(
      getJSON = getJSON_Stub, {
          privateAOIFiltering(study, stimulus, respondent)
      })

    expect_args(getJSON_Stub, 1, connection = study$connection, url = expectedUrl,
                message = paste("Retrieving AOIs for", expectedEndpoint))

    return(AOIs)
}


test_that("getAOIs() in case of no AOI defined should throw a warning as expected", {
    # Should return a warning when no AOI have been defined at the study level
    warning <- capture_warning(mockedPrivateAOIFiltering(study))
    expect_identical(warning$message, "No AOI defined for study: 2 GSR 81",
                     "no AOI warning should have been thrown for this study")
    expect_null(suppressWarnings(mockedPrivateAOIFiltering(study)), "AOI should be null")

    # Should return a warning when no AOI have been defined at the stimulus level
    warning <- capture_warning(mockedPrivateAOIFiltering(study, stimuli[1, ]))
    expect_identical(warning$message, "No AOI defined for stimulus: AntiSmoking40Sec",
                     "no AOI warning should have been thrown for this stimulus")
    expect_null(suppressWarnings(mockedPrivateAOIFiltering(study, stimuli[1, ])), "AOI should be null")

    # Should return a warning when no AOI have been defined at the respondent level
    warning <- capture_warning(mockedPrivateAOIFiltering(study, respondent = respondents[1, ]))
    expect_identical(warning$message, "No AOI defined for respondent: Wendy",
                     "no AOI warning should have been thrown for this respondent")
    expect_null(suppressWarnings(mockedPrivateAOIFiltering(study, respondent = respondents[1, ])), "AOI should be null")
})

studyAOIsPath <- "../data/studyAOIs.json"
stimulusAOIsPath <- "../data/stimulusAOIs.json"
respondentAOIsPath <- "../data/respondentAOIs.json"

test_that("no AOIs is present for a specific respondent/stimulus pair, should return the correct warning", {
    # if no AOIs is present for a specific respondent/stimulus pair, should return the correct warning
    warning <- capture_warning(mockedPrivateAOIFiltering(study, respondent = respondents[1, ], stimulus = stimuli[1, ]))
    expect_identical(warning$message, "No AOI defined for respondent: Wendy, stimulus: AntiSmoking40Sec",
                     "no AOI warning should have been thrown for this respondent/stimulus")
    expect_null(suppressWarnings(mockedPrivateAOIFiltering(study, respondent = respondents[1, ],
                                                           stimulus = stimuli[1, ])), "AOI should be null")
})

test_that("should return a imAOIList object with AOIs info", {
    # Should return all AOIs from this study if no stimulus is provided
    AOIs <- mockedPrivateAOIFiltering(study)
    expect(sum(sapply(AOIs$aois, NROW)) == 68, "study should contain 68 AOIs")

    # Should return all AOIs from this study for a specific stimulus
    AOIs <- mockedPrivateAOIFiltering(study, stimulus = stimuli[1, ])
    expect(sum(sapply(AOIs$aois, NROW)) == 3, "stimulus should contain 3 AOIs")

    # Should return all AOIs from this study for a specific respondent
    AOIs <- mockedPrivateAOIFiltering(study, respondent = respondents[1, ])
    expect(sum(sapply(AOIs$aois, NROW)) == 4, "study for this respondent should contain 4 AOIs")

    # Should return all AOIs from this study for a specific respondent and stimulus
    AOIs <- mockedPrivateAOIFiltering(study, stimulus = stimuli[4, ], respondent = respondents[1, ])
    expect(sum(sapply(AOIs$aois, NROW)) == 4, "combination should contain 4 AOIs")
})


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

test_that("should throw errors if arguments are missing or not from the good class", {
    # in case of missing study
    error <- capture_error(getAOIs())
    expect_identical(error$message,
                     "Please specify a study loaded with `imStudy()`",
                     "missing `study` param not handled properly")

    # in case of study that is not an imStudy object
    error <- capture_error(getAOIs(study = "whatever"))
    expect_identical(error$message, "`study` argument is not an imStudy object",
                     "study not being an imStudy object should throw an error")

    # in case of respondent that is not an imStimulus object
    error <- capture_error(getAOIs(study, stimulus = "whatever"))
    expect_identical(error$message, "`stimulus` argument is not an imStimulus object",
                     "stimulus not being an imStimulus object should throw an error")

    # in case of stimulus that is not an imStimulus object
    error <- capture_error(getAOIs(study, stimuli[1, ], respondent = "whatever"))
    expect_identical(error$message, "`respondent` argument is not an imRespondent object",
                     "respondent not being an imRespondent object should throw an error")

    # in case generateInOutFiles is true and not both respondent and stimulus are provided
    warning <- capture_warning(mockedGetAOIs(study, stimuli[1, ], generateInOutFiles = T))
    expect_identical(warning$message,
                     "InOut files can only be generated when both respondent and stimulus argument are provided.",
                     "respondent argument must be provided")

    warning <- capture_warning(mockedGetAOIs(study, stimulus = NULL, respondents[1, ], generateInOutFiles = T))
    expect_identical(warning$message,
                     "InOut files can only be generated when both respondent and stimulus argument are provided.",
                     "stimulus argument must be provided")
})

test_that("should return a imAOIList object with AOIs info", {
    # Should return all AOIs from this study if no stimulus is provided
    AOIs <- mockedGetAOIs(study)

    expect_true(inherits(AOIs, "imAOIList"), "`AOIs` should be an imAOIList object")
    expect(nrow(AOIs) == 68, "study should contain 68 AOIs")
    expect_identical(colnames(AOIs), c("stimulusId", "stimulusName", "id", "name", "type", "group", "area"),
                     "AOIs infos not matching")

    # Dynamic AOIs should have NA instead of area values (0 should be converted)
    expect_identical(unique(AOIs[type == "Dynamic", ]$area), NA_real_, "area should be NA")

    # Group can be either missing or filled
    expect_identical(unique(AOIs$group), c("Rectangle", NA_character_, "Test"), "group is wrong")

    # check that taking only one AOI changes the class of the object
    AOI <- AOIs[1, ]
    expect_true(inherits(AOI, "imAOI"), "`AOI` should be an imAOI object")

    # check that only taking ids of the list of stimuli changes the class of the object
    AOIs <- AOIs[, c("name", "id")]
    expect_true(all(class(AOIs) == c("data.table", "data.frame")), "truncated AOIs should be data.table")

})


test_that("generateInOutFiles should works as expected", {
  # Should return all AOIs from this study for a specific respondent and stimulus
  AOIs <- mockedGetAOIs(study, stimulus = stimuli[4, ], respondent = respondents[1, ], generateInOutFiles = T, 1)

  expect_true(inherits(AOIs, "imAOIList"), "`AOIs` should be an imAOIList object")
  expect(nrow(AOIs) == 4, "combination should contain 4 AOIs")
  expect_identical(colnames(AOIs), c("stimulusId", "stimulusName", "id", "name", "type", "group", "area", "fileId",
                                     "resultId"), "AOIs infos not matching")

  # If stimulus is missing we shouldn't add any filepaths
  AOIs <- suppressWarnings(mockedGetAOIs(study, stimulus = NULL, respondent = respondents[1, ], generateInOutFiles = T))

  expect_true(inherits(AOIs, "imAOIList"), "`AOIs` should be an imAOIList object")
  expect(nrow(AOIs) == 4, "study for this respondent should contain 4 AOIs")
  expect_identical(colnames(AOIs), c("stimulusId", "stimulusName", "id", "name", "type", "group", "area"),
                   "AOIs infos not matching")

  # If respondent is missing we shouldn't add any filepaths
  AOIs <- suppressWarnings(mockedGetAOIs(study, stimulus = stimuli[4, ], respondent = NULL, generateInOutFiles = T))

  expect_true(inherits(AOIs, "imAOIList"), "`AOIs` should be an imAOIList object")
  expect(nrow(AOIs) == 3, "study for this stimulus should contain 3 AOIs")
  expect_identical(colnames(AOIs), c("stimulusId", "stimulusName", "id", "name", "type", "group", "area"),
                   "AOIs infos not matching")
})

studyAOIsPath <- "../data/studyAOI.json"

test_that("getAOIs() in case of only one AOI should return an imAOI object", {
    # Should return only one AOI from this study
    AOIs <- mockedGetAOIs(study)

    expect_true(inherits(AOIs, "imAOI"), "`AOIs` should be an imAOI object")
    expect(nrow(AOIs) == 1, "AOIs should only contain a single AOI")
    expect_identical(AOIs$stimulusId, "1000", "AOI parent id is not matching")
    expect_identical(AOIs$stimulusName, c("AntiSmoking40Sec"), "AOI parent name is not matching")
    expect_identical(AOIs$name, c("AOI 1"), "AOI name is not matching")
    expect_identical(AOIs$id, c("19884f0d-a730-432a-9843-366d0437b456"), "AOI id is not matching")
    expect_identical(AOIs$type, c("Dynamic"), "AOI type is not matching")
    expect_identical(AOIs$group, NA_character_, "AOI group is not matching")
    expect_identical(AOIs$area, 20.534, "AOI area is not matching")
})


context("getAOI()")

AOIId <- "a966ada8-2428-4748-91d8-884f7b31eebf"

mockedGetAOI <- function(study, AOIId) {
  mockr::with_mock(
    getAOIs = mockedGetAOIs, {
        getAOI(study, AOIId)
    })
}

studyAOIsPath <- "../data/noAOIs.json"

test_that("should throw error/warnings if arguments are missing or if no AOIs in the study", {
    # in case of missing AOI id
    error <- capture_error(getAOI(study))
    expect_identical(error$message, "Please specify an AOIId. Available AOIs can be found with `getAOIs()`",
                     "missing `AOIId` param not handled properly")

    # in case of no AOI
    warning <- capture_warning(mockedGetAOI(study, AOIId))
    expect_identical(warning$message, "No AOI defined for study: 2 GSR 81", "no AOIs not handled properly")
    expect_null(suppressWarnings(mockedGetAOI(study, AOIId)), "AOI should be null")
})

studyAOIsPath <- "../data/studyAOIs.json"

test_that("should throw warnings if a wrong AOI id is provided", {
    warning <- capture_warning(mockedGetAOI(study, AOIId = "1010"))
    expect_identical(warning$message, "No AOIs found matching id: 1010", "wrong `AOIId` param not handled properly")
    expect_null(suppressWarnings(mockedGetAOI(study, AOIId = "1010")), "AOI should be null")
})

test_that("should return one AOI from the study", {
    AOI <- mockedGetAOI(study, AOIId)
    expect_true(inherits(AOI, "imAOI"), "`AOI` should be an imAOI object")
    expect(nrow(AOI) == 1, "should only contain a single AOI")
    expect_identical(AOI$id, AOIId, "AOI id is not matching")

     # print should work as expected
     expect_output(print(AOI), "iMotions AOI `Blue` with ID = a966ada8-2428-4748-91d8-884f7b31eebf")
     expect_output(print(AOI[name == "Test", ]), "No iMotions AOI found")
})
