context("privateGetAOIDetails()")

library("imotionsApi")
library("stubthat")
library("arrow")

# Load study
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))

# Load AOI
AOI <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imAOI.json")))

AOIDetailsPath <- "../data/AOIDetails.json"
AOIDetailsRespondentPath <- "../data/AOIDetailsRespondent.json"
AOIDetailForStimulusPath <- "../data/AOIDetailsForStimulus.json"
AOIDetailForStimulusRPath <- "../data/AOIDetailsForStimulusRespondent.json"

# Load respondent and stimulus
respondent <- getRespondents(study)[1, ]
stimulus <- getStimuli(study)[4, ]

mockedPrivateGetAOIDetails <- function(study, imObject, respondent = NULL) {
    getAOIDetailsUrl_Stub <- stub(getAOIDetailsUrl)
    getAOIDetailsUrl_Stub$expects(study = study, imObject = imObject, respondent = respondent)

    if (inherits(imObject, "imAOI")) {
        getAOIDetailsUrl_Stub$withArgs(respondent = respondent)$returns(AOIDetailsRespondentPath)
        getAOIDetailsUrl_Stub$withArgs(respondent = NULL)$returns(AOIDetailsPath)

        if (is.null(respondent)) {
            endpoint <- paste("AOI:", imObject$name)
        } else {
            endpoint <- paste0("AOI: ", imObject$name, ", Respondent:", respondent$name)
        }
    } else if (inherits(imObject, "imStimulus")) {
        getAOIDetailsUrl_Stub$withArgs(respondent = respondent)$returns(AOIDetailForStimulusRPath)
        getAOIDetailsUrl_Stub$withArgs(respondent = NULL)$returns(AOIDetailForStimulusPath)

        if (is.null(respondent)) {
            endpoint <- paste("Stimulus:", imObject$name)
        } else {
            endpoint <- paste0("Stimulus: ", imObject$name, ", Respondent:", respondent$name)
        }
    }

    getJSON_Stub <- stub(getJSON)
    getJSON_Stub$expects(connection = study$connection, message = paste("Retrieving details for", endpoint))
    getJSON_Stub$withArgs(url = AOIDetailsPath)$returns(jsonlite::fromJSON(AOIDetailsPath))
    getJSON_Stub$withArgs(url = AOIDetailsRespondentPath)$returns(jsonlite::fromJSON(AOIDetailsRespondentPath))
    getJSON_Stub$withArgs(url = AOIDetailForStimulusPath)$returns(jsonlite::fromJSON(AOIDetailForStimulusPath))
    getJSON_Stub$withArgs(url = AOIDetailForStimulusRPath)$returns(jsonlite::fromJSON(AOIDetailForStimulusRPath))

    AOIdetails <- mockr::with_mock(getAOIDetailsUrl = getAOIDetailsUrl_Stub$f,
                                   getJSON = getJSON_Stub$f, {
                                        privateGetAOIDetails(study, imObject, respondent)
                                   })

    return(AOIdetails)
}

expectedNames <- c("aoiId", "respId", "fileId", "resultId", "fileIdIsEmptyPlaceHolder")

test_that("privateGetAOIDetails() should return the correct AOI details during local connection", {
    aoiDetails <- mockedPrivateGetAOIDetails(study, AOI)

    expect(nrow(aoiDetails) == 3, "3 respondents should have the AOI defined")
    expect_identical(names(aoiDetails), expectedNames, "aoi details infos not matching")
    expect_identical(unique(aoiDetails$aoiId), AOI$id, "AOI id should be the same for the two respondents")
})

test_that("privateGetAOIDetails() should return the correct AOI details for a respondent if provided", {
    aoiDetails <- mockedPrivateGetAOIDetails(study, AOI, respondent)

    expect(nrow(aoiDetails) == 1, "only the respondent of interest should be kept")
    expect_identical(names(aoiDetails), expectedNames, "aoi details infos not matching")

    # in case a fileId is present, there is no need to call the getAOIDetails function
    AOI$fileId <- "fileId"
    AOI$resultId <- "resultId"
    aoiDetails <- mockedPrivateGetAOIDetails(study, AOI, respondent)
    expect_identical(aoiDetails$fileId, "fileId", "fileId stored in AOI should be retrieved directly")
    expect_identical(aoiDetails$resultId, "resultId", "resultId stored in AOI should be retrieved directly")
})

test_that("privateGetAOIDetails() should return the correct AOI details for a stimulus during local connection", {
    aoiDetails <- mockedPrivateGetAOIDetails(study, stimulus)

    expect(nrow(aoiDetails) == 9, "3 AOIs should be defined for the 3 respondents")
    expect_identical(names(aoiDetails), expectedNames, "aoi details infos not matching")
    expect_identical(unique(aoiDetails$aoiId),
                     c(AOI$id, "c456ff27-1e29-409f-9dcc-1042c489f2b0", "dc1f9a1d-4dc3-4959-b088-fee5fec29f1f"),
                     "AOI id should be the same for the 3 respondents")
})

test_that("privateGetAOIDetails() should return the correct AOI details for a respondent/stimulus if provided", {
    aoiDetails <- mockedPrivateGetAOIDetails(study, stimulus, respondent)

    expect(nrow(aoiDetails) == 4, "only the respondent of interest should be kept")
    expect_identical(names(aoiDetails), expectedNames, "aoi details infos not matching")
})




context("getAOIRespondentData()");

AOIDetailsFile <- jsonlite::fromJSON(AOIDetailsRespondentPath)

mockedGetAOIRespondentData  <- function(study, AOI, respondent, AOIDetailsFile) {
    privateGetAOIDetails_Stub <- stub(privateGetAOIDetails)
    privateGetAOIDetails_Stub$expects(study = study, imObject = AOI, respondent = respondent)
    privateGetAOIDetails_Stub$returns(AOIDetailsFile)

    listResult <- mockr::with_mock(privateGetAOIDetails = privateGetAOIDetails_Stub$f, {
                                        getAOIRespondentData(study, AOI, respondent)
                                   })

    return(listResult)
}

test_that("should throw errors if arguments are missing or not from the good class", {
    # in case of missing study
    error <- capture_error(getAOIRespondentData())
    expect_identical(error$message, "Please specify a study loaded with `imStudy()`",
                     "missing `study` param not handled properly")

    # in case of missing AOI
    error <- capture_error(getAOIRespondentData(study))
    expect_identical(error$message, "Please specify an AOI loaded with `getAOIs()`",
                     "missing `AOI` param not handled properly")

    # in case of missing respondent
    error <- capture_error(getAOIRespondentData(study, AOI))
    expect_identical(error$message, "Please specify a respondent loaded with `getRespondents()`",
                     "missing `respondent` param not handled properly")

    # in case of study that is not an imStudy object
    error <- capture_error(getAOIRespondentData(study = "whatever", AOI, respondent))
    expect_identical(error$message, "`study` argument is not an imStudy object",
                     "study not being an imStudy object should throw an error")

    # in case of AOI that is not an imAOI object
    error <- capture_error(getAOIRespondentData(study, AOI = "whatever", respondent))
    expect_identical(error$message, "`AOI` argument is not an imAOI object",
                     "AOI not being an imAOI object should throw an error")

    # in case of respondent that is not an imRespondent object
    error <- capture_error(getAOIRespondentData(study, AOI, respondent = "whatever"))
    expect_identical(error$message, "`respondent` argument is not an imRespondent object",
                     "respondent not being an imRespondent object should throw an error")
})

test_that("should throw a warning if the AOI has not been defined for this respondent", {
    AOIDetailsFile <- jsonlite::fromJSON("../data/no_scenes_annotations_aoidetails.json")
    warning <- capture_warning(mockedGetAOIRespondentData(study, AOI, respondent, AOIDetailsFile))
    expect_identical(warning$message, "AOI New Aoi was not found for respondent Wendy",
                     "no AOI defined for this respondent should throw an error")

    expect_null(suppressWarnings(mockedGetAOIRespondentData(study, AOI, respondent, AOIDetailsFile)),
                "result should be null")
})

# Modify AOIDetailsFile so it fit test data
AOIDetailsFile$respId <- respondent$id
AOIDetailsFile$fileId <- "../data/aoiRespondentData.pbin"

test_that("should return the correct intervals for this AOI/respondent pair", {
    AOIintervals <- mockedGetAOIRespondentData(study, AOI, respondent, AOIDetailsFile)$intervals

    # Check AOI intervals
    expect_identical(unique(AOIintervals$type), "AOI", "intervals should all be of AOI type")
    expect_identical(unique(AOIintervals$parentId), "1002", "AOI intervals should have the same parent")
    expect_identical(unique(AOIintervals$parentName), "IAAF", "AOI intervals should have the same parent")
    expect_identical(unique(AOIintervals$name), "New Aoi", "AOI intervals should have the same name")
    expect_equal(AOIintervals$fragments.start, c(9181.255, 16015.560, 21810.115), 1e-2, info = "wrong fragments start")
    expect_equal(AOIintervals$fragments.end, c(11400.94, 18352.07, 25887.33), 1e-2, info = "wrong fragments end")

    expect_equal(AOIintervals$fragments.duration, c(2219.689, 2336.514, 4077.218), 1e-2,
                 info = "wrong fragments duration")

    expect_identical(unique(AOIintervals$id), c("09a234fc-00ad-4257-b7e1-da5754986c9d"),
                     "AOI intervals should have the same id")

    expect_identical(unique(AOIintervals$text), NA_character_, "AOI intervals should have no text")

    # Checking dimensions and class
    expect_identical(AOIintervals$respondent[[1]], respondent, "should have the correct respondent information")
    expect_equal(ncol(AOIintervals), 10, info = "`intervals` should have 10 columns")
    expect_equal(nrow(AOIintervals), 3, info = "should have 3 intervals")
    expect_true(inherits(AOIintervals, "imIntervalList"), "`intervals` should be an imIntervalList object")

    # check that taking only one intervals changes the class of the object
    AOIinterval <- AOIintervals[1, ]
    expect_true(inherits(AOIinterval, "imInterval"), "`interval` should be an imInterval object")

    # check that only taking names and type of the list of intervals changes the class of the object
    AOIintervals <- AOIintervals[, c("name", "type")]
    expect_true(all(class(AOIintervals) == c("data.table", "data.frame")), "truncated intervals should be data.table")
})

test_that("should return the correct inOutGaze for this AOI/respondent pair", {
    inOutGaze <- mockedGetAOIRespondentData(study, AOI, respondent, AOIDetailsFile)$inOutGaze

    # Check inOutGaze data.table
    expect_identical(names(inOutGaze), c("Timestamp", "IsGazeInAOI"), info = "wrong column names")
    expect_equal(nrow(inOutGaze), 7, info = "should have 7 in/off values")
    expect_identical(inOutGaze$IsGazeInAOI, c(F, T, F, T, F, T, F), "wrong value")
    expect_equal(inOutGaze$Timestamp, c(9181.255, 9195.980, 11400.944, 16016.118, 17195.937, 21816.359, 22655.954),
                 1e-2, info = "wrong in/off Timestamp")
})

test_that("should return the correct inOutMouseClick for this AOI/respondent pair", {
    inOutMouseClick <- mockedGetAOIRespondentData(study, AOI, respondent, AOIDetailsFile)$inOutMouseClick

    # Check inOutMouseClick data.table
    expect_identical(names(inOutMouseClick), c("Timestamp", "IsMouseInAOI"), info = "wrong column names")
    expect_equal(nrow(inOutMouseClick), 1, info = "should have 1 in/off values")
    expect_true(inOutMouseClick$IsMouseInAOI, "wrong value")
    expect_equal(inOutMouseClick$Timestamp, 22109.5, 1e-2, info = "wrong in/off Timestamp")
})

# Modify AOIDetailsFile so it returns an empty event (no gaze, mouse click in AOI)
AOIDetailsFile$fileId <- "../data/aoiEmptyRespondentData.pbin"

test_that("should work as expected if no gazes or mouse click in", {
    resultList <- mockedGetAOIRespondentData(study, AOI, respondent, AOIDetailsFile)

    expect_identical(names(resultList), c("inOutGaze", "inOutMouseClick", "intervals"), info = "wrong names")
    expect_equal(nrow(resultList$inOutMouseClick), 0, info = "should be empty (no click)")
    expect_equal(nrow(resultList$inOutGaze), 1, info = "should only have one value (no gaze in)")
    expect_false(resultList$inOutGaze$IsGazeInAOI, info = "wrong value")
})


# Modify AOIDetailsFile so it returns an AOI with 0 ms duration of exposure
AOIDetailsFile$fileId <- "../data/AOInotDefined.pbin"

test_that("should work as expected if no AOI exposure", {
    resultList <- mockedGetAOIRespondentData(study, AOI, respondent, AOIDetailsFile)
    expect_identical(names(resultList), c("inOutGaze", "inOutMouseClick", "intervals"), info = "wrong names")

    # Check inOutMouseClick
    expect_identical(names(resultList$inOutMouseClick), c("Timestamp", "IsMouseInAOI"), info = "wrong column names")
    expect_equal(nrow(resultList$inOutMouseClick), 0, info = "should be empty (no exposure)")

    # Check inOutGaze
    expect_identical(names(resultList$inOutGaze), c("Timestamp", "IsGazeInAOI"), info = "wrong column names")
    expect_equal(nrow(resultList$inOutGaze), 0, info = "should be empty (no exposure)")

    # Check interval
    expect_identical(resultList$intervals$type, "AOI", "interval should be of AOI type")
    expect_identical(resultList$intervals$parentId, "1002", "AOI interval should have the good parent")
    expect_identical(resultList$intervals$parentName, "IAAF", "AOI intervals should have the same parent")
    expect_identical(resultList$intervals$name, "New Aoi", "AOI interval should have the good name")
    expect_true(is.na(resultList$intervals$fragments.start), NA, info = "wrong fragment start")
    expect_true(is.na(resultList$intervals$fragments.end), NA, info = "wrong fragment end")
    expect_equal(resultList$intervals$fragments.duration, 0, 1e-2, info = "wrong fragment duration")
    expect_identical(resultList$intervals$id, c("09a234fc-00ad-4257-b7e1-da5754986c9d"),
                     "AOI interval should have the good id")

    expect_identical(resultList$intervals$text, NA_character_, "AOI interval should have no text")

    # Checking dimensions and class
    expect_identical(resultList$intervals$respondent[[1]], respondent, "should have the correct respondent information")
    expect_equal(ncol(resultList$intervals), 10, info = "`intervals` should have 10 columns")
    expect_equal(nrow(resultList$intervals), 1, info = "should have 1 interval")
})
