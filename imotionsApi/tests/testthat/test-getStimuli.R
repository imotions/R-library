# getStimuli ==========================================================================================================
context("getStimuli()")

library(mockery)

# Load first study
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))

# Load second study
study_one_stimulus <- jsonlite::unserializeJSON(readLines("../data/imStudy_oneofeach.json"))

# Load cloud study
study_cloud <- jsonlite::unserializeJSON(readLines("../data/imStudy_cloud.json"))

test_that("error - arguments are missing or not from the good class", {
    # in case of missing study
    expect_error(getStimuli(), "Please specify a study loaded with `imStudy()`", fixed = TRUE,
                 info = "missing `study` param not handled properly")

    # in case of study that is not an imStudy object
    expect_error(getStimuli(study = "whatever"), "`study` argument is not an imStudy object",
                 info = "study not being an imStudy object should throw an error")

    # in case of respondent that is not an imRespondent object
    expect_error(getStimuli(study, respondent = "whatever"), "`respondent` argument is not an imRespondent object",
                 info = "respondent not being an imRespondent object should throw an error")
})

expected_column_names <- c("name", "id", "type", "imageUrl", "videoUrl", "exposureTimeMs", "displayOrder", "relevant",
                           "parentId", "parentName")

test_that("local return - imStimulusList from a study", {
    stimuli <- getStimuli(study)

    expect_s3_class(stimuli, "imStimulusList")
    expect_equal(nrow(stimuli), 6, info = "stimuli should contain 6 stimuli")
    expect_named(stimuli, expected_column_names, info = "stimuli infos not matching")

    # check that taking only one stimulus changes the class of the object
    stimulus <- stimuli[1, ]
    expect_s3_class(stimulus, "imStimulus")

    # check that only taking ids of the list of stimuli changes the class of the object
    stimuli <- stimuli[, c("name", "id")]
    expect_s3_class(stimuli, c("data.table", "data.frame"), exact = TRUE)
})

test_that("remote return - imStimulusList from a study", {
    stimuli <- getStimuli(study_cloud)

    expect_s3_class(stimuli, "imStimulusList")
    expect_equal(nrow(stimuli), 28, info = "stimuli should contain 28 stimuli")
    expect_equal(ncol(stimuli), 21, info = "stimuli should have 21 properties")
    expect_true(all(expected_column_names %in% names(stimuli)), "stimuli infos not matching")
})

test_that("local return - imStimulus in case of only one stimulus", {
    stimuli <- getStimuli(study_one_stimulus)

    expect_s3_class(stimuli, "imStimulus")
    expect_equal(nrow(stimuli), 1, info = "stimuli should only contain a single stimulus")
    expect_identical(stimuli$name, "AntiSmoking40Sec", "stimulus name is not matching")
    expect_identical(stimuli$id, "1000", "stimulus id is not matching")
})

test_that("local return - imStimulusList for a specific respondent", {
    # Case where only two respondents (out of 3) have seen a stimulus
    respondents <- getRespondents(study)
    stimuli <- getStimuli(study, respondents[1, ])

    expect_s3_class(stimuli, "imStimulusList")
    expect_equal(nrow(stimuli), 5, info = "stimuli should contain 5 stimuli")
    expect_named(stimuli, expected_column_names, info = "stimuli infos not matching")
    expect_identical(stimuli$name, c("AntiSmoking40Sec", "CHAMONIX_Living_on_the_edge", "IAAF", "IAAF_Scene[1]",
                                     "NiceElementTypes"), "wrong stimuli selected")
})

test_that("remote return - imStimulusList for a specific respondent", {
    respondents <- getRespondents(study_cloud)
    stimuli <- getStimuli(study_cloud, respondents[7, ])

    expect_s3_class(stimuli, "imStimulusList")
    expect_equal(nrow(stimuli), 28, info = "stimuli should contain 28 stimuli")
    expect_equal(ncol(stimuli), 21, info = "stimuli should have 21 properties")

    # Case where respondent didn't see any stimulus
    stimuli <- getStimuli(study_cloud, respondents[1, ])
    expect_s3_class(stimuli, "imStimulus")
    expect_equal(nrow(stimuli), 0, info = "stimuli should contain 28 stimuli")
    expect_equal(ncol(stimuli), 21, info = "stimuli should have 21 properties")
})

# getStimulus =========================================================================================================
context("getStimulus()")

test_that("error - arguments are missing or wrong", {
    # in case of missing stimulusId
    expect_error(getStimulus(study), "Please specify a stimulusId. Available stimuli can be found with `getStimuli()`",
                 fixed = TRUE, info = "missing `stimulus` param not handled properly")

    # in case of wrong stimulusId
    expect_error(getStimulus(study, "1006"), "No stimulus found matching id: 1006",
                 info = "wrong stimulusId not handled properly")
})

test_that("local return - specific stimulus from a study", {
    stimulusId <- "1002"
    stimulus <- getStimulus(study, stimulusId)

    expect_s3_class(stimulus, "imStimulus")
    expect_equal(nrow(stimulus), 1, info = "should only contain a single stimulus")
    expect_identical(stimulus$id, stimulusId, "stimulus id is not matching")

    # print should work as expected
    expect_output(print(stimulus), "iMotions Stimulus `IAAF` with ID = 1002")
    expect_output(print(stimulus[name == "Test", ]), "No iMotions Stimulus found")
})

test_that("remote return - specific stimulus from a study", {
    stimulusId <- "5ef46004-9ad4-419f-ad4f-139e38faad7e"
    stimulus <- getStimulus(study_cloud, stimulusId)

    expect_s3_class(stimulus, "imStimulus")
    expect_equal(nrow(stimulus), 1, info = "should only contain a single stimulus")
    expect_identical(stimulus$id, stimulusId, "stimulus id is not matching")

    # print should work as expected
    expected_output <- "iMotions Stimulus `Pool-house-driveway` with ID = 5ef46004-9ad4-419f-ad4f-139e38faad7e"
    expect_output(print(stimulus), expected_output)
    expect_output(print(stimulus[name == "Test", ]), "No iMotions Stimulus found")
})
