# privateRespondentFiltering ==========================================================================================
context("privateRespondentFiltering()")

library(mockery)

# Load first study
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))

# Load second study
study_one_respondent <- jsonlite::unserializeJSON(readLines("../data/imStudy_oneofeach.json"))

# Load cloud study
study_cloud <- jsonlite::unserializeJSON(readLines("../data/imStudy_cloud.json"))

# Load AOI object
AOI <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imAOI.json")))
AOI_cloud <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imAOI_cloud.json")))

AOIDetailsPath <- "../data/AOIDetails.json"
AOIDetailsPath_cloud <- "../data/AOIDetails_processed_cloud.json"

test_that("local return - all respondents from a study", {
    respondents <- privateRespondentFiltering(study)

    expect_equal(nrow(respondents), 3, info = "respondents should contain 3 respondents")
    expect_named(respondents, c("name", "id", "group", "age", "gender"), info = "respondents infos not matching")
})

test_that("remote return - all respondents from a study", {
    respondents <- privateRespondentFiltering(study_cloud)

    expect_equal(nrow(respondents), 7, info = "respondents should contain 7 respondents")
    expect_named(respondents, c("name", "id", "group", "age", "gender"), info = "respondents infos not matching")
})

test_that("local return - only one respondent in a study", {
    respondents <- privateRespondentFiltering(study_one_respondent)

    expect_equal(nrow(respondents), 1, info = "respondents should only contain a single Respondent")
    expect_identical(respondents$name, c("Wendy"), "respondent name is not matching")
    expect_identical(respondents$id, c("09bd22e6-29b6-4a8a-8cc1-4780a5163e63"), "respondent id is not matching")
})

test_that("local return - respondents for a specific stimulus", {
    # Case where only two respondents (out of 3) have seen a stimulus
    stimuli <- getStimuli(study)
    respondents <- privateRespondentFiltering(study, obj = stimuli[2, ])

    expect_equal(nrow(respondents), 2, info = "respondents should only contain a 2 respondents")
    expect_identical(respondents$name, c("Quilana", "Olana"), "respondents name is not matching")
})

test_that("remote return - respondents for a specific stimulus", {
    # Case where only two respondents (out of 3) have seen a stimulus
    stimuli <- getStimuli(study_cloud)
    respondents <- privateRespondentFiltering(study_cloud, obj = stimuli[2, ])

    expect_equal(nrow(respondents), 2, info = "respondents should only contain a 2 respondents")
    expect_identical(respondents$id, c("6c74637b-6250-4d13-bbbc-c999d9c8a74b", "50e16f69-52e2-43e2-9005-ed303f90225e"),
                     "respondents id is not matching")
})

test_that("local return - respondents for a specific AOI", {
    privateGetAOIDetails_Stub <- mock(jsonlite::fromJSON(AOIDetailsPath))

    # Case where only two respondents (out of 3) have an AOI defined
    respondents <- mockr::with_mock(privateGetAOIDetails = privateGetAOIDetails_Stub, {
        privateRespondentFiltering(study, obj = AOI)
    })

    expect_args(privateGetAOIDetails_Stub, 1, study = study, imObject = AOI)
    expect_equal(nrow(respondents), 2, info = "respondents should only contain a 2 respondents")
    expect_identical(respondents$name, c("Quilana", "Olana"), "respondents name is not matching")
})

test_that("remote return - respondents for a specific AOI", {
    privateGetAOIDetails_Stub <- mock(jsonlite::fromJSON(AOIDetailsPath_cloud))

    # Case where only 2 respondents (out of 7) have an AOI defined
    respondents <- mockr::with_mock(privateGetAOIDetails = privateGetAOIDetails_Stub, {
        privateRespondentFiltering(study_cloud, obj = AOI_cloud)
    })

    expect_args(privateGetAOIDetails_Stub, 1, study = study_cloud, imObject = AOI_cloud)
    expect_equal(nrow(respondents), 2, info = "respondents should only contain 2 respondents")
    expect_identical(respondents$name, c("bab55356-43fc-4c25-a39d-a1d513965614",
                                         "0b042e95-93e7-4e00-8534-5307452cc757"), "respondents name are not matching")
})

test_that("check - create a defaut group if none are available", {
    study_no_group <- study
    names(study_no_group$respondents$variables) <- "TEST"

    respondents <- privateRespondentFiltering(study_no_group)
    expect_identical(unique(respondents$group), "Default", "Default group should have been created")

    # case for a remote study
    respondents <- privateRespondentFiltering(study_cloud)
    expect_identical(unique(respondents$group), "Default", "Default group should have been created")
})

# getRespondents ======================================================================================================
context("getRespondents()")

test_that("error - arguments are missing or not from the good class", {
    # in case of missing study
    expect_error(getRespondents(), "Please specify a study loaded with `imStudy()`", fixed = TRUE,
                 info = "missing `study` param not handled properly")

    # in case of study that is not an imStudy object
    expect_error(getRespondents(study = "whatever"), "`study` argument is not an imStudy object",
                 info = "study not being an imStudy object should throw an error")

    # in case of stimulus that is not an imStimulus object
    expect_error(getRespondents(study, stimulus = "whatever"), "`stimulus` argument is not an imStimulus object",
                 info = "stimulus not being an imStimulus object should throw an error")

    # in case of AOI that is not an imAOI object
    expect_error(getRespondents(study, AOI = "whatever"), "`AOI` argument is not an imAOI object",
                 info = "AOI not being an imAOI object should throw an error")

    # in case of segment that is not an imSegment object
    expect_error(getRespondents(study, segment = "whatever"), "`segment` argument is not an imSegment object",
                 info = "segment not being an imSegment object should throw an error")

    # in case both stimulus and AOI are provided
    stimuli <- getStimuli(study)
    expect_error(getRespondents(study, stimulus = stimuli[1, ], AOI = AOI),
                 "AOIs are linked to a stimulus, please provide either stimulus or AOI, not both.",
                 info = "providing both AOI and stimulus should throw an error")
})

test_that("local return - imRespondentList from a study", {
    respondents <- getRespondents(study)

    expect_s3_class(respondents, "imRespondentList")
    expect_equal(nrow(respondents), 3, info = "respondents should contain 3 respondents")
    expect_named(respondents, c("name", "id", "group", "age", "gender"), info = "respondents infos not matching")

    # check that taking only one respondent changes the class of the object
    respondent <- respondents[1, ]
    expect_s3_class(respondent, "imRespondent")

    # check that only taking ids of the list of respondents changes the class of the object
    respondents <- respondents[, c("name", "id")]
    expect_s3_class(respondents, c("data.table", "data.frame"), exact = TRUE)
})


test_that("remote return - imRespondentList from a study", {
    respondents <- getRespondents(study_cloud)

    expect_s3_class(respondents, "imRespondentList")
    expect_equal(nrow(respondents), 7, info = "respondents should contain 7 respondents")
    expect_named(respondents, c("name", "id", "group", "age", "gender"), info = "respondents infos not matching")

    # check that taking only one respondent changes the class of the object
    respondent <- respondents[1, ]
    expect_s3_class(respondent, "imRespondent")

    # check that only taking ids of the list of respondents changes the class of the object
    respondents <- respondents[, c("name", "id")]
    expect_s3_class(respondents, c("data.table", "data.frame"), exact = TRUE)
})

test_that("local return - imRespondent in case of only one respondent", {
    respondents <- getRespondents(study_one_respondent)

    expect_s3_class(respondents, "imRespondent")
    expect_equal(nrow(respondents), 1, info = "respondents should only contain a single Respondent")
    expect_identical(respondents$name, "Wendy", "respondent name is not matching")
    expect_identical(respondents$id, "09bd22e6-29b6-4a8a-8cc1-4780a5163e63", "respondent id is not matching")
})

test_that("local return - imRespondentList for a specific segment", {
    # Case where only two respondents (out of 3) are part of a segment
    segments <- getSegments(study)
    respondents <- getRespondents(study, segment = segments[1, ])

    expect_equal(nrow(respondents), 2, info = "respondents should only contain a 2 respondents")
    expect_identical(respondents$name, c("Wendy", "Olana"), "respondents name is not matching")

})

test_that("remote return - imRespondentList for a specific segment", {
    segments <- getSegments(study_cloud)
    respondents <- getRespondents(study_cloud, segment = segments[1, ])

    expect_equal(nrow(respondents), 2, info = "respondents should only contain a 2 respondents")
    expect_identical(respondents$id, c("6c74637b-6250-4d13-bbbc-c999d9c8a74b", "50e16f69-52e2-43e2-9005-ed303f90225e"),
                     "respondents id is not matching")
})

test_that("local return - imRespondentList for a specific segment/stimulus", {
    # Only 1 respondent (out of 2) from the segment has been exposed to this stimulus
    stimuli <- getStimuli(study)
    segments <- getSegments(study)
    respondents <- getRespondents(study, stimulus = stimuli[2, ], segment = segments[1, ])

    expect_equal(nrow(respondents), 1, info = "respondents should only contain a 1 respondent")
    expect_identical(respondents$name, "Olana", "respondent name is not matching")
})

test_that("remote return - imRespondentList for a specific segment/stimulus", {
    # Only 2 respondent from the segment has been exposed to this stimulus
    stimuli <- getStimuli(study_cloud)
    segments <- getSegments(study_cloud)
    respondents <- getRespondents(study_cloud, stimulus = stimuli[2, ], segment = segments[1, ])

    expect_equal(nrow(respondents), 2, info = "respondents should only contain a 2 respondents")
    expect_identical(respondents$id, c("6c74637b-6250-4d13-bbbc-c999d9c8a74b", "50e16f69-52e2-43e2-9005-ed303f90225e"),
                     "respondent id is not matching")
})

test_that("local return - imRespondentList for a specific segment/AOI", {
    privateGetAOIDetails_Stub <- mock(jsonlite::fromJSON(AOIDetailsPath))

    # Only 1 respondent (out of 2) from the segment has the AOI defined
    segments <- getSegments(study)

    respondents <- mockr::with_mock(privateGetAOIDetails = privateGetAOIDetails_Stub, {
        getRespondents(study, AOI = AOI, segment = segments[1, ])
    })

    expect_equal(nrow(respondents), 1, info = "respondents should only contain a 1 respondent")
    expect_identical(respondents$name, c("Olana"), "respondent name is not matching")
})

test_that("remote return - imRespondentList for a specific segment/AOI", {
    privateGetAOIDetails_Stub <- mock(jsonlite::fromJSON(AOIDetailsPath_cloud))

    # Only 1 respondent from the segment has the AOI defined
    segments <- getSegments(study_cloud)

    respondents <- mockr::with_mock(privateGetAOIDetails = privateGetAOIDetails_Stub, {
        getRespondents(study_cloud, AOI = AOI_cloud, segment = segments[1, ])
    })

    expect_equal(nrow(respondents), 1, info = "respondents should only contain a 1 respondent")
    expect_identical(respondents$name, "0b042e95-93e7-4e00-8534-5307452cc757", "respondent name is not matching")
})


test_that("check - keep respondent variables if available", {
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

# getRespondent =======================================================================================================
context("getRespondent()")

test_that("error - arguments are missing or wrong", {
    # in case of missing respondentId
    expect_error(getRespondent(study),
                 "Please specify a respondentId. Available respondents can be found with `getRespondents()`",
                 fixed = TRUE, info =  "missing `respondent` param not handled properly")

    # in case of wrong respondentId
    expect_error(getRespondent(study, "09bd22e6-29b6-4a8a-8cc1-4780a5163e65"),
                 "No respondent found matching id: 09bd22e6-29b6-4a8a-8cc1-4780a5163e65",
                 info = "wrong respondentId not handled properly")
})

test_that("local return - specific respondent from a study", {
    respondentId <- "750ed075-1c8d-4aff-91b1-ed6c9e052808"
    respondent <- getRespondent(study, respondentId)

    expect_s3_class(respondent, "imRespondent")
    expect_equal(nrow(respondent), 1, info = "should only contain a single respondent")
    expect_identical(respondent$id, respondentId, "respondent id is not matching")

    # print should work as expected
    expect_output(print(respondent), "iMotions Respondent `Quilana` with ID = 750ed075-1c8d-4aff-91b1-ed6c9e052808")
    expect_output(print(respondent[name == "Test", ]), "No iMotions Respondent found")
})

test_that("remote return - specific respondent from a study", {
    respondentId <- "50e16f69-52e2-43e2-9005-ed303f90225e"
    respondent <- getRespondent(study_cloud, respondentId)

    expect_s3_class(respondent, "imRespondent")
    expect_equal(nrow(respondent), 1, info = "should only contain a single respondent")
    expect_identical(respondent$id, respondentId, "respondent id is not matching")

    # print should work as expected
    expected_output <- paste0("iMotions Respondent `152f69fc-7a6c-4c97-8d9c-3786a8e2b3e7` with ID = ",
                              "50e16f69-52e2-43e2-9005-ed303f90225e")

    expect_output(print(respondent), expected_output)
    expect_output(print(respondent[name == "Test", ]), "No iMotions Respondent found")
})
