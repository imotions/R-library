library("imotionsApi");
library("stubthat");

context("getStimuli()");

# Load first study
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))

# Load second study
study_one_stimulus <- jsonlite::unserializeJSON(readLines("../data/imStudy_oneofeach.json"))

test_that("should throw errors if arguments are missing or not from the good class", {
    # in case of missing study
    error <- capture_error(getStimuli())
    expect_identical(error$message,
                     "Please specify a study loaded with `imStudy()`",
                     "missing `study` param not handled properly")

    # in case of study that is not an imStudy object
    error <- capture_error(getStimuli(study = "whatever"))
    expect_identical(error$message, "`study` argument is not an imStudy object",
                     "study not being an imStudy object should throw an error")

    # in case of respondent that is not an imRespondent object
    error <- capture_error(getStimuli(study, respondent = "whatever"))
    expect_identical(error$message, "`respondent` argument is not an imRespondent object",
                     "respondent not being an imRespondent object should throw an error")
})

expected_column_names <- c("name", "id", "type", "imageUrl", "videoUrl", "exposureTimeMs", "displayOrder", "relevant",
                           "parentId", "parentName")

test_that("should return all stimuli from this study", {
    stimuli <- getStimuli(study)
    expect_true(inherits(stimuli, "imStimulusList"), "`stimuli` should be an imStimulusList object")
    expect(nrow(stimuli) == 6, "stimuli should contain 6 stimuli")

    expect_identical(colnames(stimuli), expected_column_names, "stimuli infos not matching")

    # check that taking only one stimulus changes the class of the object
    stimulus <- stimuli[1, ]
    expect_true(inherits(stimulus, "imStimulus"), "`stimulus` should be an imStimulus object")

    # check that only taking ids of the list of stimuli changes the class of the object
    stimuli <- stimuli[, c("name", "id")]
    expect_true(all(class(stimuli) == c("data.table", "data.frame")), "truncated stimuli should be data.table")
})

test_that("getStimuli() in case of only one stimulus should return an imStimulus object", {
    stimuli <- getStimuli(study_one_stimulus)

    expect_true(inherits(stimuli, "imStimulus"), "`stimuli` should be an imStimulus object")
    expect(nrow(stimuli) == 1, "stimuli should only contain a single stimulus")
    expect_identical(stimuli$name, c("AntiSmoking40Sec"), "stimulus name is not matching")
    expect_identical(stimuli$id, c("1000"), "stimulus id is not matching")
})

test_that("getStimuli() by respondent", {
    # Case where only two respondents (out of 3) have seen a stimulus
    respondents <- getRespondents(study)
    stimuli <- getStimuli(study, respondents[1, ])
    expect_true(inherits(stimuli, "imStimulusList"), "`stimuli` should be an imStimulusList object")
    expect(nrow(stimuli) == 5, "stimuli should contain 5 stimuli")
    expect_identical(colnames(stimuli), expected_column_names, "stimuli infos not matching")
    expect_identical(stimuli$name, c("AntiSmoking40Sec", "CHAMONIX_Living_on_the_edge", "IAAF", "IAAF_Scene[1]",
                                     "NiceElementTypes"), "wrong stimuli selected")
})

context("getStimulus()")

test_that("should throw errors if arguments are missing or wrong", {
    # in case of missing stimulusId
    error <- capture_error(getStimulus(study))
    expect_identical(error$message,
                     "Please specify a stimulusId. Available stimuli can be found with `getStimuli()`",
                     "missing `stimulus` param not handled properly")

    # in case of wrong stimulusId
    error <- capture_error(getStimulus(study, "1006"))
    expect_identical(error$message, "No stimulus found matching id: 1006", "wrong stimulusId not handled properly")
})

test_that("should return one stimulus from the study", {
    stimulusId <- "1002"
    stimulus <- getStimulus(study, stimulusId)
    expect_true(inherits(stimulus, "imStimulus"), "`stimulus` should be an imStimulus object")
    expect(nrow(stimulus) == 1, "should only contain a single stimulus")
    expect_identical(stimulus$id, stimulusId, "stimulus id is not matching")

    # print should work as expected
    expect_output(print(stimulus), "iMotions Stimulus `IAAF` with ID = 1002")
    expect_output(print(stimulus[name == "Test", ]), "No iMotions Stimulus found")
})
