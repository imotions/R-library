# privateGetAOIDetails ================================================================================================
context("privateGetAOIDetails()")

library(mockery)

# Load study
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))
study_cloud <- jsonlite::unserializeJSON(readLines("../data/imStudy_cloud.json"))

# Load AOI
AOI <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imAOI.json"))) # local without fileId
AOI_inout <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imAOI_inout.json"))) # local with fileId
AOI_cloud <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imAOI_cloud.json")))

AOIDetailsPath <- "../data/AOIDetails.json"
AOIDetailsRespondentPath <- "../data/AOIDetailsRespondent.json"
AOIDetailForStimulusPath <- "../data/AOIDetailsForStimulus.json"
AOIDetailForStimulusRPath <- "../data/AOIDetailsForStimulusRespondent.json"
AOIDetailsPath_cloud <- "../data/AOIDetails_cloud.json"

# Load respondent and stimulus
respondent <- getRespondents(study)[1, ]
stimulus <- getStimuli(study)[4, ]
respondent_cloud <- getRespondents(study_cloud)[1, ]
stimulus_cloud <- getStimuli(study)[4, ]

mockedPrivateGetAOIDetails <- function(study, imObject, expected_endpoint, respondent = NULL, expectedAOICall = 1) {
    # Replace url to load test data
    mockUrl <- function(study, url) {
        if (!study$connection$localIM) {
            return(AOIDetailsPath_cloud)
        }

        if (grepl("*", url, fixed = TRUE)) {
            if (grepl("respondent", url)) {
                return(AOIDetailForStimulusRPath)
            } else {
                return(AOIDetailForStimulusPath)
            }
        } else {
            if (grepl("respondent", url)) {
                return(AOIDetailsRespondentPath)
            } else {
                return(AOIDetailsPath)
            }
        }
    }

    if (!study$connection$localIM) {
        expectedUrl <- imObject$fileId
    } else {
        expectedUrl <- getAOIDetailsUrl(study, imObject, respondent)
    }

    getJSON_Stub <- mock(jsonlite::fromJSON(mockUrl(study, expectedUrl)))

    AOIdetails <- mockr::with_mock(
        getJSON = getJSON_Stub, {
            privateGetAOIDetails(study, imObject, respondent)
        })

    expect_called(getJSON_Stub, expectedAOICall)

    if (expectedAOICall > 0) {
        expect_args(getJSON_Stub, 1, connection = study$connection, url = expectedUrl,
                    message = paste("Retrieving details for", expected_endpoint))
    }

    return(AOIdetails)
}

expectedNames <- c("aoiId", "respId", "fileId", "resultId", "fileIdIsEmptyPlaceHolder")

test_that("local return - AOI details for a specific AOI", {
    expected_endpoint <- "AOI: New Aoi"
    aoiDetails <- mockedPrivateGetAOIDetails(study, AOI, expected_endpoint)

    expect_equal(nrow(aoiDetails), 3, info = "3 respondents should have the AOI defined")
    expect_named(aoiDetails, expectedNames, info = "aoi details infos not matching")
    expect_identical(unique(aoiDetails$aoiId), AOI$id, "AOI id should be the same for the two respondents")
})

test_that("local return - AOI details for a specific AOI/respondent", {
    expected_endpoint <- "AOI: New Aoi, Respondent: Wendy"
    aoiDetails <- mockedPrivateGetAOIDetails(study, AOI, expected_endpoint, respondent)

    expect_equal(nrow(aoiDetails), 1, info = "only the respondent of interest should be kept")
    expect_named(aoiDetails, expectedNames, info = "aoi details infos not matching")

    # in case a fileId is present, there is no need to call the getAOIDetails function
    aoiDetails <- mockedPrivateGetAOIDetails(study, AOI_inout, expected_endpoint, respondent, expectedAOICall = 0)

    expect_identical(aoiDetails$fileId, AOI_inout$fileId, "fileId stored in AOI should be retrieved directly")
    expect_identical(aoiDetails$resultId, AOI_inout$resultId, "resultId stored in AOI should be retrieved directly")
})

test_that("local return - AOI details for a specific stimulus", {
    expected_endpoint <- "Stimulus: IAAF"
    aoiDetails <- mockedPrivateGetAOIDetails(study, stimulus, expected_endpoint)

    expect_equal(nrow(aoiDetails), 9, info = "3 AOIs should be defined for the 3 respondents")
    expect_named(aoiDetails, expectedNames, info = "aoi details infos not matching")
    expect_identical(unique(aoiDetails$aoiId),
                     c(AOI$id, "c456ff27-1e29-409f-9dcc-1042c489f2b0", "dc1f9a1d-4dc3-4959-b088-fee5fec29f1f"),
                     "AOI id should be the same for the 3 respondents")
})

test_that("local return - AOI details for a specific stimulus/respondent", {
    expected_endpoint <- "Stimulus: IAAF, Respondent: Wendy"
    aoiDetails <- mockedPrivateGetAOIDetails(study, stimulus, expected_endpoint, respondent)

    expect_equal(nrow(aoiDetails), 4, info = "only the respondent of interest should be kept")
    expect_named(aoiDetails, expectedNames, info = "aoi details infos not matching")
})

AOIDetailForStimulusPath <- "../data/AOIDetailsForStimulus_missingfileId.json"

test_that("local return - AOI details with missing fileId should be filtered out", {
    expected_endpoint <- "Stimulus: IAAF"
    aoiDetails <- mockedPrivateGetAOIDetails(study, stimulus, expected_endpoint)

    expect_equal(nrow(aoiDetails), 7, info = "2 respondents without fileId should be removed")
    expect_named(aoiDetails, expectedNames, info = "aoi details infos not matching")
    expect_identical(unique(aoiDetails$aoiId),
                     c("c456ff27-1e29-409f-9dcc-1042c489f2b0", "dc1f9a1d-4dc3-4959-b088-fee5fec29f1f", AOI$id),
                     "AOI id should be the same for the 3 respondents")
})

expectedNames <- c("stimId", "respId", "startMediaOffset", "endMediaOffset", "aoiId", "aoiInOuts")

test_that("remote return - AOI details for a specific AOI", {
    expected_endpoint <- "AOI: El Manuel Area"
    aoiDetails <- mockedPrivateGetAOIDetails(study_cloud, AOI_cloud, expected_endpoint)

    expect_equal(nrow(aoiDetails), 3, info = "3 respondents should have the AOI defined")
    expect_named(aoiDetails, expectedNames, info = "aoi details infos not matching")
})


test_that("remote return - AOI details for a specific AOI/respondent", {
    respondent <- getRespondents(study_cloud)[1, ]
    expected_endpoint <- "AOI: El Manuel Area, Respondent: bab55356-43fc-4c25-a39d-a1d513965614"
    aoiDetails <- mockedPrivateGetAOIDetails(study_cloud, AOI_cloud, expected_endpoint, respondent)

    expect_equal(nrow(aoiDetails), 1, info = "only the respondent of interest should be kept")
    expect_named(aoiDetails, expectedNames, info = "aoi details infos not matching")
})


# getAOIRespondentData ================================================================================================
context("getAOIRespondentData()")

AOIDetailsFile <- jsonlite::fromJSON(AOIDetailsRespondentPath)

mockedGetAOIRespondentData  <- function(study, AOI, respondent, AOIDetailsFile) {
    privateGetAOIDetails_Stub <- mock(AOIDetailsFile)

    listResult <- mockr::with_mock(privateGetAOIDetails = privateGetAOIDetails_Stub, {
                                        getAOIRespondentData(study, AOI, respondent)
                                   })

    expect_args(privateGetAOIDetails_Stub, 1, study = study, imObject = AOI, respondent = respondent)
    return(listResult)
}

test_that("error - arguments are missing or not from the good class", {
    # in case of missing study
    expect_error(getAOIRespondentData(), "Please specify a study loaded with `imStudy()`", fixed = TRUE,
                 info = "missing `study` param not handled properly")

    # in case of missing AOI
    expect_error(getAOIRespondentData(study), "Please specify an AOI loaded with `getAOIs()`", fixed = TRUE,
                 info = "missing `AOI` param not handled properly")

    # in case of missing respondent
    expect_error(getAOIRespondentData(study, AOI), "Please specify a respondent loaded with `getRespondents()`",
                 fixed = TRUE, info = "missing `respondent` param not handled properly")

    # in case of study that is not an imStudy object
    expect_error(getAOIRespondentData(study = "whatever", AOI, respondent), "`study` argument is not an imStudy object",
                 info = "study not being an imStudy object should throw an error")

    # in case of AOI that is not an imAOI object
    expect_error(getAOIRespondentData(study, AOI = "whatever", respondent), "`AOI` argument is not an imAOI object",
                 info = "AOI not being an imAOI object should throw an error")

    # in case of respondent that is not an imRespondent object
    expect_error(getAOIRespondentData(study, AOI, respondent = "whatever"),
                 "`respondent` argument is not an imRespondent object",
                 info = "respondent not being an imRespondent object should throw an error")
})

test_that("warning - AOI has not been defined for this respondent", {
    AOIDetailsFile <- jsonlite::fromJSON("../data/no_scenes_annotations_aoidetails.json")
    expect_warning(AOI <- mockedGetAOIRespondentData(study, AOI, respondent, AOIDetailsFile),
                   "AOI New Aoi was not found for respondent Wendy",
                   info = "no AOI defined for this respondent should throw an error")

    expect_null(AOI, info = "result should be null")
})

# Modify AOIDetailsFile so it fit test data
AOIDetailsFile$respId <- respondent$id
AOIDetailsFile$fileId <- "../data/aoiRespondentData.pbin"

test_that("local return - intervals for a specific AOI/respondent pair", {
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
    expect_s3_class(AOIintervals, "imIntervalList")

    # check that taking only one intervals changes the class of the object
    AOIinterval <- AOIintervals[1, ]
    expect_s3_class(AOIinterval, "imInterval")

    # check that only taking names and type of the list of intervals changes the class of the object
    AOIintervals <- AOIintervals[, c("name", "type")]
    expect_s3_class(AOIintervals, c("data.table", "data.frame"), exact = TRUE)
})

test_that("local return - inOutGaze for a specific AOI/respondent pair", {
    inOutGaze <- mockedGetAOIRespondentData(study, AOI, respondent, AOIDetailsFile)$inOutGaze

    # Check inOutGaze data.table
    expect_named(inOutGaze, c("Timestamp", "IsGazeInAOI"), info = "wrong column names")
    expect_equal(nrow(inOutGaze), 7, info = "should have 7 in/off values")
    expect_identical(inOutGaze$IsGazeInAOI, c(rep(c(FALSE, TRUE), 3), FALSE), "wrong value")
    expect_equal(inOutGaze$Timestamp, c(9181.255, 9195.980, 11400.944, 16016.118, 17195.937, 21816.359, 22655.954),
                 1e-2, info = "wrong in/off Timestamp")
})

test_that("local return - inOutMouseClick for a specific AOI/respondent pair", {
    inOutMouseClick <- mockedGetAOIRespondentData(study, AOI, respondent, AOIDetailsFile)$inOutMouseClick

    # Check inOutMouseClick data.table
    expect_named(inOutMouseClick, c("Timestamp", "IsMouseInAOI"), info = "wrong column names")
    expect_equal(nrow(inOutMouseClick), 1, info = "should have 1 in/off values")
    expect_true(inOutMouseClick$IsMouseInAOI, "wrong value")
    expect_equal(inOutMouseClick$Timestamp, 22109.5, 1e-2, info = "wrong in/off Timestamp")
})

# Modify AOIDetailsFile so it returns an empty event (no gaze, mouse click in AOI)
AOIDetailsFile$fileId <- "../data/aoiEmptyRespondentData.pbin"

test_that("local check - work if no gazes or mouse click in", {
    resultList <- mockedGetAOIRespondentData(study, AOI, respondent, AOIDetailsFile)

    expect_named(resultList, c("inOutGaze", "inOutMouseClick", "intervals"), info = "wrong names")
    expect_equal(nrow(resultList$inOutMouseClick), 0, info = "should be empty (no click)")
    expect_equal(nrow(resultList$inOutGaze), 1, info = "should only have one value (no gaze in)")
    expect_false(resultList$inOutGaze$IsGazeInAOI, info = "wrong value")
})


# Modify AOIDetailsFile so it returns an AOI with 0 ms duration of exposure
AOIDetailsFile$fileId <- "../data/AOInotDefined.pbin"

test_that("local check - work if no AOI exposure", {
    resultList <- mockedGetAOIRespondentData(study, AOI, respondent, AOIDetailsFile)
    expect_named(resultList, c("inOutGaze", "inOutMouseClick", "intervals"), info = "wrong names")

    # Check inOutMouseClick
    expect_named(resultList$inOutMouseClick, c("Timestamp", "IsMouseInAOI"), info = "wrong column names")
    expect_equal(nrow(resultList$inOutMouseClick), 0, info = "should be empty (no exposure)")

    # Check inOutGaze
    expect_named(resultList$inOutGaze, c("Timestamp", "IsGazeInAOI"), info = "wrong column names")
    expect_equal(nrow(resultList$inOutGaze), 0, info = "should be empty (no exposure)")

    # Check interval
    expect_identical(resultList$intervals$type, "AOI", "interval should be of AOI type")
    expect_identical(resultList$intervals$parentId, "1002", "AOI interval should have the good parent")
    expect_identical(resultList$intervals$parentName, "IAAF", "AOI intervals should have the same parent")
    expect_identical(resultList$intervals$name, "New Aoi", "AOI interval should have the good name")
    expect_identical(resultList$intervals$fragments.start, NA_real_, info = "wrong fragment start")
    expect_identical(resultList$intervals$fragments.end, NA_real_, info = "wrong fragment end")
    expect_equal(resultList$intervals$fragments.duration, 0, 1e-2, info = "wrong fragment duration")
    expect_identical(resultList$intervals$id, c("09a234fc-00ad-4257-b7e1-da5754986c9d"),
                     "AOI interval should have the good id")

    expect_identical(resultList$intervals$text, NA_character_, "AOI interval should have no text")

    # Checking dimensions and class
    expect_identical(resultList$intervals$respondent[[1]], respondent, "should have the correct respondent information")
    expect_equal(ncol(resultList$intervals), 10, info = "`intervals` should have 10 columns")
    expect_equal(nrow(resultList$intervals), 1, info = "should have 1 interval")
})

respondent <- getRespondents(study_cloud)[1, ]
expected_endpoint <- "AOI: El Manuel Area, Respondent: bab55356-43fc-4c25-a39d-a1d513965614"
aoiDetails <- mockedPrivateGetAOIDetails(study_cloud, AOI_cloud, expected_endpoint, respondent)

test_that("remote return - intervals for a specific AOI/respondent pair", {
    AOIintervals <- mockedGetAOIRespondentData(study_cloud, AOI_cloud, respondent, aoiDetails)$intervals

    # Check AOI intervals
    expect_identical(unique(AOIintervals$type), "AOI", "intervals should all be of AOI type")
    expect_identical(unique(AOIintervals$parentId), "5ef46004-9ad4-419f-ad4f-139e38faad7e",
                     "AOI intervals should have the same parent")

    expect_identical(unique(AOIintervals$parentName), "Pool-house-driveway",
                     "AOI intervals should have the same parent")

    expect_identical(unique(AOIintervals$name), "El Manuel Area", "AOI intervals should have the same name")
    expect_equal(AOIintervals$fragments.start, c(44639, 48665), 1e-2, info = "wrong fragments start")
    expect_equal(AOIintervals$fragments.end, c(46639, 50665), 1e-2, info = "wrong fragments end")
    expect_equal(AOIintervals$fragments.duration, c(2000, 2000), 1e-2, info = "wrong fragments duration")
    expect_identical(unique(AOIintervals$id), c("93fbaae0-8b6f-45b6-b5dd-9a5d4216d7fd"),
                     "AOI intervals should have the same id")

    expect_identical(unique(AOIintervals$text), NA_character_, "AOI intervals should have no text")

    # Checking dimensions and class
    expect_identical(AOIintervals$respondent[[1]], respondent, "should have the correct respondent information")
    expect_equal(ncol(AOIintervals), 10, info = "`intervals` should have 10 columns")
    expect_equal(nrow(AOIintervals), 2, info = "should have 2 intervals")
    expect_s3_class(AOIintervals, "imIntervalList")

    # check that taking only one intervals changes the class of the object
    AOIinterval <- AOIintervals[1, ]
    expect_s3_class(AOIinterval, "imInterval")

    # check that only taking names and type of the list of intervals changes the class of the object
    AOIintervals <- AOIintervals[, c("name", "type")]
    expect_s3_class(AOIintervals, c("data.table", "data.frame"), exact = TRUE)
})

test_that("remote return - inOutGaze for a specific AOI/respondent pair", {
    inOutGaze <- mockedGetAOIRespondentData(study_cloud, AOI_cloud, respondent, aoiDetails)$inOutGaze

    # Check inOutGaze data.table
    expect_named(inOutGaze, c("Timestamp", "IsGazeInAOI"), info = "wrong column names")
    expect_equal(nrow(inOutGaze), 5, info = "should have 5 in/off values")
    expect_identical(inOutGaze$IsGazeInAOI, c(rep(c(FALSE, TRUE), 2), FALSE), "wrong value")
    expect_equal(inOutGaze$Timestamp, c(44639, 46091, 46639, 49919, 50381), 1e-2, info = "wrong in/off Timestamp")
})

test_that("remote return - inOutMouseClick for a specific AOI/respondent pair", {
    inOutMouseClick <- mockedGetAOIRespondentData(study_cloud, AOI_cloud, respondent, aoiDetails)$inOutMouseClick

    # Check inOutMouseClick data.table
    expect_named(inOutMouseClick, c("Timestamp", "IsMouseInAOI"), info = "wrong column names")
    expect_equal(nrow(inOutMouseClick), 0, info = "should have 0 in/off values")
})
