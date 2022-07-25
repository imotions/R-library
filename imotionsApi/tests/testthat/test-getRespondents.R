context("privateRespondentFiltering()")

library("imotionsApi")
library("mockery")

# Load first study
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))

# Load second study
study_one_respondent <- jsonlite::unserializeJSON(readLines("../data/imStudy_oneofeach.json"))

# Load AOI object
AOI <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imAOI.json")))
AOIDetails <- jsonlite::fromJSON("../data/AOIDetails.json")

test_that("should return all respondents from this study", {
    respondents <- privateRespondentFiltering(study)
    expect(nrow(respondents) == 3, "respondents should contain 3 respondents")
    expect_identical(colnames(respondents), c("name", "id", "group", "age", "gender"),
                     "respondents infos not matching")
})

test_that("getRespondents() in case of only one respondent should work correctly", {
    respondents <- privateRespondentFiltering(study_one_respondent)
    expect(nrow(respondents) == 1, "respondents should only contain a single Respondent")
    expect_identical(respondents$name, c("Wendy"), "respondent name is not matching")
    expect_identical(respondents$id, c("09bd22e6-29b6-4a8a-8cc1-4780a5163e63"), "respondent id is not matching")
})

test_that("getRespondents() by stimulus", {
    # Case where only two respondents (out of 3) have seen a stimulus
    stimuli <- getStimuli(study)
    respondents <- privateRespondentFiltering(study, obj = stimuli[2, ])
    expect(nrow(respondents) == 2, "respondents should only contain a 2 respondents")
    expect_identical(respondents$name, c("Quilana", "Olana"), "respondents name is not matching")
})

# Create get AOI details stub
privateGetAOIDetails_Stub <- mock(AOIDetails, cycle = TRUE)

test_that("getRespondents() by AOI", {
    # Case where only two respondents (out of 3) have an AOI defined
    respondents <- mockr::with_mock(privateGetAOIDetails = privateGetAOIDetails_Stub, {
        privateRespondentFiltering(study, obj = AOI)
    })

    expect_args(privateGetAOIDetails_Stub, 1, study = study, imObject = AOI)
    expect(nrow(respondents) == 2, "respondents should only contain a 2 respondents")
    expect_identical(respondents$name, c("Quilana", "Olana"), "respondents name is not matching")
})


test_that("should create a defaut group if none are available", {
    study_no_group <- study
    names(study_no_group$respondents$variables) <- "TEST"

    respondents <- privateRespondentFiltering(study_no_group)
    expect_identical(unique(respondents$group), "Default", "Default group should have been created")
})


context("getRespondents()")

test_that("should throw errors if arguments are missing or not from the good class", {
    # in case of missing study
    error <- capture_error(getRespondents())
    expect_identical(error$message,
                     "Please specify a study loaded with `imStudy()`",
                     "missing `study` param not handled properly")

    # in case of study that is not an imStudy object
    error <- capture_error(getRespondents(study = "whatever"))
    expect_identical(error$message, "`study` argument is not an imStudy object",
                     "study not being an imStudy object should throw an error")

    # in case of stimulus that is not an imStimulus object
    error <- capture_error(getRespondents(study, stimulus = "whatever"))
    expect_identical(error$message, "`stimulus` argument is not an imStimulus object",
                     "stimulus not being an imStimulus object should throw an error")

    # in case of AOI that is not an imAOI object
    error <- capture_error(getRespondents(study, AOI = "whatever"))
    expect_identical(error$message, "`AOI` argument is not an imAOI object",
                     "AOI not being an imAOI object should throw an error")

    # in case of segment that is not an imSegment object
    error <- capture_error(getRespondents(study, segment = "whatever"))
    expect_identical(error$message, "`segment` argument is not an imSegment object",
                     "segment not being an imSegment object should throw an error")

    # in case both stimulus and AOI are provided
    stimuli <- getStimuli(study)
    error <- capture_error(getRespondents(study, stimulus = stimuli[1, ], AOI = AOI))
    expect_identical(error$message, "AOIs are linked to a stimulus, please provide either stimulus or AOI, not both.",
                     "providing both AOI and stimulus should throw an error")
})

test_that("should return all respondents from this study", {
    respondents <- getRespondents(study)
    expect_true(inherits(respondents, "imRespondentList"), "`respondents` should be an imRespondentList object")
    expect(nrow(respondents) == 3, "respondents should contain 3 respondents")
    expect_identical(colnames(respondents), c("name", "id", "group", "age", "gender"),
                     "respondents infos not matching")

    # check that taking only one respondent changes the class of the object
    respondent <- respondents[1, ]
    expect_true(inherits(respondent, "imRespondent"), "`respondent` should be an imRespondent object")

    # check that only taking ids of the list of respondents changes the class of the object
    respondents <- respondents[, c("name", "id")]
    expect_true(all(class(respondents) == c("data.table", "data.frame")), "truncated respondents should be data.table")
})

test_that("getRespondents() in case of only one respondent should return an imRespondent object", {
    respondents <- getRespondents(study_one_respondent)
    expect_true(inherits(respondents, "imRespondent"), "`respondents` should be an imRespondent object")
    expect(nrow(respondents) == 1, "respondents should only contain a single Respondent")
    expect_identical(respondents$name, c("Wendy"), "respondent name is not matching")
    expect_identical(respondents$id, c("09bd22e6-29b6-4a8a-8cc1-4780a5163e63"), "respondent id is not matching")
})

test_that("getRespondents() by segment", {
    # Case where only two respondents (out of 3) are part of a segment
    segments <- getSegments(study)
    respondents <- getRespondents(study, segment = segments[1, ])
    expect(nrow(respondents) == 2, "respondents should only contain a 2 respondents")
    expect_identical(respondents$name, c("Wendy", "Olana"), "respondents name is not matching")

})

test_that("getRespondents() by segment and stimulus", {
    # Only 1 respondent (out of 2) from the segment has been exposed to this stimulus
    stimuli <- getStimuli(study)
    segments <- getSegments(study)
    respondents <- getRespondents(study, stimulus = stimuli[2, ], segment = segments[1, ])
    expect(nrow(respondents) == 1, "respondents should only contain a 1 respondent")
    expect_identical(respondents$name, c("Olana"), "respondent name is not matching")
})


test_that("getRespondents() by segment and AOI", {
    # Only 1 respondent (out of 2) from the segment has the AOI defined
    segments <- getSegments(study)

    respondents <- mockr::with_mock(privateGetAOIDetails = privateGetAOIDetails_Stub, {
                                        getRespondents(study, AOI = AOI, segment = segments[1, ])
                                    })

    expect(nrow(respondents) == 1, "respondents should only contain a 1 respondent")
    expect_identical(respondents$name, c("Olana"), "respondent name is not matching")
})

test_that("should expose respondent variables if available", {
    #Add respondent variables to the study
    study_more_variables <- study
    study_more_variables$respondents$variables$var1 <- "var1"
    study_more_variables$respondents$variables$var2 <- "var2"

    respondents <- getRespondents(study_more_variables, keepRespondentVariables = TRUE)
    expect_identical(names(respondents), c("name", "id", "group", "age", "gender", "variables.var1", "variables.var2"),
                     "Columns with variables should have been created")

    respondents <- getRespondents(study_more_variables, keepRespondentVariables = FALSE)
    expect_identical(names(respondents), c("name", "id", "group", "age", "gender"),
                     "No columns with variables should have been created")
})

context("getRespondent()")

test_that("should throw errors if arguments are missing or wrong", {
    # in case of missing respondentId
    error <- capture_error(getRespondent(study))
    expect_identical(error$message,
                     "Please specify a respondentId. Available respondents can be found with `getRespondents()`",
                     "missing `respondent` param not handled properly")

    # in case of wrong respondentId
    error <- capture_error(getRespondent(study, "09bd22e6-29b6-4a8a-8cc1-4780a5163e65"))
    expect_identical(error$message, "No respondent found matching id: 09bd22e6-29b6-4a8a-8cc1-4780a5163e65",
                     "wrong respondentId not handled properly")
})

test_that("should return one respondent from the study", {
    respondentId <- "750ed075-1c8d-4aff-91b1-ed6c9e052808"
    respondent <- getRespondent(study, respondentId)
    expect_true(inherits(respondent, "imRespondent"), "`respondent` should be an imRespondent object")
    expect(nrow(respondent) == 1, "should only contain a single respondent")
    expect_identical(respondent$id, respondentId, "respondent id is not matching")

    # print should work as expected
    expect_output(print(respondent), "iMotions Respondent `Quilana` with ID = 750ed075-1c8d-4aff-91b1-ed6c9e052808")
    expect_output(print(respondent[name == "Test", ]), "No iMotions Respondent found")
})
