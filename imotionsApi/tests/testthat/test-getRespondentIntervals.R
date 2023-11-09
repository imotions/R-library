# privateGetIntervalsForStimuli =======================================================================================
context("privateGetIntervalsForStimuli()")

library(mockery)

# Load study, respondent and stimuli
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))
respondent <- getRespondent(study, "09bd22e6-29b6-4a8a-8cc1-4780a5163e63")
stimuli <- getStimuli(study, respondent)

# Load events
slideEvents <- arrow::read_parquet("../data/Native_SlideEvents.csv.pbin")
scenesEvents <- jsonlite::fromJSON("../data/scenes.json")
annotationsEvents <- jsonlite::fromJSON("../data/annotations.json")

# Load sensors
sensors <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imSensorList.json")))

# Load remote study and slideEvents results
study_cloud <- jsonlite::unserializeJSON(readLines("../data/imStudy_cloud.json"))
sensors_cloud <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imSensorList_cloud.json")))
slideEvents_cloud <- jsonlite::fromJSON("../data/Native_SlideEvents_processed.json")

mockedPrivateGetIntervalsForStimuli <- function(study, respondent, stimuli, slideEvents, sensors,
                                                expectedDataCall = 1) {

    getSensors_Stub <- mock(sensors)
    getSensorData_Stub <- mock(slideEvents)

    stimIntervals <- mockr::with_mock(getSensors = getSensors_Stub,
                                      getSensorData = getSensorData_Stub, {
                                          privateGetIntervalsForStimuli(study, respondent, stimuli)
                                      })

    expect_args(getSensors_Stub, 1, study = study, respondent = respondent)

    if (expectedDataCall > 0) {
        expect_args(getSensorData_Stub, 1, study = study, sensor = sensors[sensors$name == "SlideEvents", ])
    }

    return(stimIntervals)
}

test_that("local return - data.table of the good format", {
    # Retrieve stimulus intervals
    stimIntervals <- mockedPrivateGetIntervalsForStimuli(study, respondent, stimuli, slideEvents, sensors)

    expect_equal(ncol(stimIntervals), 9, info = "stimulus intervals should have 9 columns")
    expect_identical(unique(stimIntervals$type), "Stimulus", "intervals should all be of stimulus type")
    expect_identical(unique(stimIntervals$parentId), NA_character_, "stimulus intervals should have no parent")
    expect_identical(unique(stimIntervals$parentName), "", "stimulus intervals should have no parent")

    expect_identical(stimIntervals$name,
                     c("IAAF", "NiceElementTypes", "CHAMONIX_Living_on_the_edge", "AntiSmoking40Sec"),
                     "wrong stimuli name")

    expect_equal(stimIntervals$fragments.start, c(6073.691, 30127.270, 50875.549, 60904.847), 1e-2,
                 info = "wrong fragments start")

    expect_equal(stimIntervals$fragments.end, c(29990.17, 50805.50, 60867.82, 100934.26), 1e-2,
                 info = "wrong fragments end")

    expect_equal(stimIntervals$fragments.duration, c(23916.481, 20678.230, 9992.272, 40029.417), 1e-2,
                 info = "wrong fragments duration")

    expect_identical(stimIntervals$id, c("1002", "1003", "1001", "1000"), "wrong id")
    expect_identical(unique(stimIntervals$text), "", "stimulus intervals should have no text")
})


test_that("remote return - data.table of the good format", {
    respondent <- getRespondents(study_cloud)[1, ]
    stimuli <- getStimuli(study_cloud)

    # Retrieve stimulus intervals
    stimIntervals <- mockedPrivateGetIntervalsForStimuli(study_cloud, respondent, stimuli, slideEvents_cloud,
                                                         sensors_cloud)

    expect_equal(ncol(stimIntervals), 9, info = "stimulus intervals should have 9 columns")
    expect_equal(nrow(stimIntervals), 28, info = "stimulus intervals should have 28 stimuli")
    expect_identical(unique(stimIntervals$type), "Stimulus", "intervals should all be of stimulus type")
    expect_identical(unique(stimIntervals$parentId), NA_character_, "stimulus intervals should have no parent")
    expect_identical(unique(stimIntervals$parentName), "", "stimulus intervals should have no parent")

    # Subset and take only 4 stimuli to check values
    stimIntervals <- stimIntervals [1:4, ]

    expect_identical(stimIntervals$name, c("PREcalib_0_320x180.png", "PREcalib_1_960x180.png",
                                           "PREcalib_2_1600x180.png", "PREcalib_3_320x540.png"), "wrong stimuli name")

    expect_equal(stimIntervals$fragments.start, c(1.2, 4038.1, 8053.4, 12059.4), 1e-2,
                 info = "wrong fragments start")

    expect_equal(stimIntervals$fragments.end, c(4036.6, 8046.0, 12054.0, 16076.5), 1e-2,
                 info = "wrong fragments end")

    expect_equal(stimIntervals$fragments.duration, c(4035.4, 4007.9, 4000.6, 4017.1), 1e-2,
                 info = "wrong fragments duration")

    expect_identical(stimIntervals$id,
                     c("3014c850-f3bd-4d7b-acc7-b7792bb1af68", "0058eb32-f506-4fe6-83b5-ec2ab4b30d5f",
                       "8fff2349-a8d4-4c45-8cf6-4699e85849cd", "ef3c1737-adaf-4491-ab62-434bc221ba04"), "wrong id")

    expect_identical(unique(stimIntervals$text), "", "stimulus intervals should have no text")
})

test_that("local return - NULL if no slideEvents sensors available", {
    # Retrieve stimulus intervals and check returned value
    sensors <- sensors[2, ]
    stimIntervals <- mockedPrivateGetIntervalsForStimuli(study, respondent, stimuli, slideEvents, sensors, 0)

    expect_null(stimIntervals, "no intervals should have been found")
})


test_that("remote return - NULL if no slideEvents sensors available", {
    respondent <- getRespondents(study_cloud)[1, ]
    stimuli <- getStimuli(study_cloud)
    sensors_cloud <- sensors[2, ]

    # Retrieve stimulus intervals and check returned value
    stimIntervals <- mockedPrivateGetIntervalsForStimuli(study_cloud, respondent, stimuli, slideEvents_cloud,
                                                         sensors_cloud, 0)

    expect_null(stimIntervals, "no intervals should have been found")
})

# privateGetIntervalsForScenes ========================================================================================
context("privateGetIntervalsForScenes()")

mockedPrivateGetIntervalsForScenes <- function(study, respondent, stimuli, scenesEvents, expectedDataCall = 1) {
    getJSON_Stub <- mock(scenesEvents)

    sceneIntervals <- mockr::with_mock(getJSON = getJSON_Stub, {
        privateGetIntervalsForScenes(study, respondent, stimuli)
    })

    if (expectedDataCall > 0) {
        expect_args(getJSON_Stub, 1, connection = study$connection, url = getRespondentScenesUrl(study, respondent),
                    message = "Retrieving scenes for respondent Wendy")
    }

    return(sceneIntervals)
}

test_that("local return - data.table only containing scenes included in the analysis", {
    # Retrieve scenes intervals
    scenesIntervals <- mockedPrivateGetIntervalsForScenes(study, respondent, stimuli, scenesEvents)

    expect_equal(ncol(scenesIntervals), 9, info = "scene intervals should have 9 columns")
    expect_identical(unique(scenesIntervals$type), "Scene", "intervals should all be of scene type")
    expect_identical(scenesIntervals$parentId, c("1002", "1002"), "scene intervals should have a parent")
    expect_identical(scenesIntervals$parentName, c("IAAF", "IAAF"), "scene intervals should have a parent")
    expect_identical(scenesIntervals$name, c("IAAF_Scene[1]", "IAAF_Scene[1]"), "wrong scenes name")
    expect_equal(scenesIntervals$fragments.start, c(6075.691, 18440.691), 1e-2, info = "wrong fragments start")
    expect_equal(scenesIntervals$fragments.end, c(13478.69, 25209.69), 1e-2, info = "wrong fragments end")
    expect_equal(scenesIntervals$fragments.duration, c(7403, 6769), 1e-2, info = "wrong fragments duration")
    expect_identical(scenesIntervals$id, c("1008", "1008"), "wrong id")
    expect_identical(unique(scenesIntervals$text), "", "scenes intervals should have no text")
})

test_that("local return - NULL if no scenes available", {
    # Retrieve scenes intervals and check returned value
    scenesEvents <- jsonlite::fromJSON("../data/no_scenes_annotations_aoidetails.json")
    scenesIntervals <- mockedPrivateGetIntervalsForScenes(study, respondent, stimuli, scenesEvents)

    expect_null(scenesIntervals, "no scenes should have been found")
})

test_that("remote return - NULL as not supported", {
    # Retrieve scenes intervals and check returned value
    scenesIntervals <- mockedPrivateGetIntervalsForScenes(study_cloud, respondent, stimuli, scenesEvents, 0)

    expect_null(scenesIntervals, "no scenes should have been found")
})

# privateGetIntervalsForAnnotations ===================================================================================
context("privateGetIntervalsForAnnotations()")

mockedPrivateGetIntervalsForAnnotations <- function(study, respondent, stimuli, annotationsEvents,
                                                    expectedDataCall = 1) {

    getJSON_Stub <- mock(annotationsEvents)

    annotationsIntervals <- mockr::with_mock(getJSON = getJSON_Stub, {
        privateGetIntervalsForAnnotations(study, respondent, stimuli)
    })

    if (expectedDataCall > 0) {
        expect_args(getJSON_Stub, 1, connection = study$connection,
                    url = getRespondentAnnotationsUrl(study, respondent),
                    message = "Retrieving annotations for respondent Wendy")
    }

    return(annotationsIntervals)
}

test_that("local return - data.table only containing annotations included in the analysis", {
    # Retrieve annotations intervals
    annotationsIntervals <- mockedPrivateGetIntervalsForAnnotations(study, respondent, stimuli, annotationsEvents)

    expect_equal(ncol(annotationsIntervals), 9, info = "annotations intervals should have 9 columns")
    expect_identical(unique(annotationsIntervals$type), "Annotation", "intervals should all be of annotation type")

    expect_identical(annotationsIntervals$parentId, c("1001", "1000", "1000", "1000"),
                     "annotation intervals should have a parent")

    expect_identical(annotationsIntervals$parentName,
                     c("CHAMONIX_Living_on_the_edge", "AntiSmoking40Sec", "AntiSmoking40Sec", "AntiSmoking40Sec"),
                     "annotation intervals should have a parent")

    expect_identical(annotationsIntervals$name, c("Test", "Test", "New annotation", "New annotation"),
                     "wrong annotations name")

    expect_equal(annotationsIntervals$fragments.start, c(54001.85, 66672.75, 65775.53, 73817.99), 1e-2,
                 info = "wrong fragments start")

    expect_equal(annotationsIntervals$fragments.end, c(56220.52, 77329.63, 70939.19, 77247.54), 1e-2,
                 info = "wrong fragments end")

    expect_equal(annotationsIntervals$fragments.duration, c(2218.667, 10656.885, 5163.659, 3429.550), 1e-2,
                 info = "wrong fragments duration")

    expect_identical(annotationsIntervals$id, c("1", "2", "3", "3"), "wrong id")
    expect_identical(unique(annotationsIntervals$text), c("", "Test comment"), "annotation intervals should have text")
})

test_that("local return - NULL if no annotations available", {
    # Retrieve scenes intervals and check returned value
    annotationsEvents <- jsonlite::fromJSON("../data/no_scenes_annotations_aoidetails.json")
    annotationsIntervals <- mockedPrivateGetIntervalsForAnnotations(study, respondent, stimuli, annotationsEvents)

    expect_null(annotationsIntervals, "no annotations should have been found")
})

test_that("remote return - NULL as not supported", {
    # Retrieve scenes intervals and check returned value
    annotationsIntervals <- mockedPrivateGetIntervalsForAnnotations(study_cloud, respondent, stimuli,
                                                                    annotationsEvents, 0)

    expect_null(annotationsIntervals, "no annotations should have been found")
})

# getRespondentIntervals ==============================================================================================
context("getRespondentIntervals()")

mockedGetRespondentIntervals <- function(study, respondent, type = c("Stimulus", "Annotation", "Scene"),
                                         stimIntervals = NULL, sceneIntervals = NULL, annotationIntervals = NULL) {

    privateGetIntervalsForStimuli_Stub <- mock(stimIntervals)
    privateGetIntervalsForScenes_Stub <- mock(sceneIntervals)
    privateGetIntervalsForAnnotations_Stub <- mock(annotationIntervals)

    intervals <- mockr::with_mock(privateGetIntervalsForStimuli = privateGetIntervalsForStimuli_Stub,
                                  privateGetIntervalsForScenes = privateGetIntervalsForScenes_Stub,
                                  privateGetIntervalsForAnnotations = privateGetIntervalsForAnnotations_Stub, {
                                      getRespondentIntervals(study, respondent, type)
                                  })

    return(intervals)
}

test_that("error - arguments are missing or not from the good class", {
    # in case of missing study
    expect_error(getRespondentIntervals(), "Please specify a study loaded with `imStudy()`", fixed = TRUE,
                 info = "missing `study` param not handled properly")

    # in case of missing respondent
    expect_error(getRespondentIntervals(study), "Please specify a respondent loaded with `getRespondents()`",
                 fixed = TRUE, info = "missing `respondent` param not handled properly")

    # in case of study that is not an imStudy object
    expect_error(getRespondentIntervals(study = "whatever", respondent), "`study` argument is not an imStudy object",
                 info = "study not being an imStudy object should throw an error")

    # in case of respondent that is not an imRespondent object
    expect_error(getRespondentIntervals(study, respondent = "whatever"),
                 "`respondent` argument is not an imRespondent object",
                 info = "respondent not being an imRespondent object should throw an error")

    # in case of type not in Stimulus, Scene and/or Annotation
    expect_error(getRespondentIntervals(study, respondent, type = c("Stimulus", "whatever")),
                 "`type` argument can only be set to Stimulus, Scene and/or Annotation",
                 info = "type not set properly should throw an error")
})

checkImIntervalList <- function(intervals, respondent, expectNIntervals) {
    # Checking dimensions and class
    testthat::expect_identical(intervals$respondent[[1]], respondent, "should have the correct respondent information")
    testthat::expect_equal(ncol(intervals), 10, info = "`intervals` should have 10 columns")
    testthat::expect_equal(nrow(intervals), expectNIntervals,
                           info = paste("should have", expectNIntervals, "intervals"))

    testthat::expect_s3_class(intervals, "imIntervalList")

    # check that taking only one intervals changes the class of the object
    interval <- intervals[1, ]
    testthat::expect_s3_class(interval, "imInterval")

    # check that only taking names and type of the list of intervals changes the class of the object
    intervals <- intervals[, c("name", "type")]
    testthat::expect_s3_class(intervals, c("data.table", "data.frame"), exact = TRUE)
}

stimIntervals <- mockedPrivateGetIntervalsForStimuli(study, respondent, stimuli, slideEvents, sensors)
sceneIntervals <- mockedPrivateGetIntervalsForScenes(study, respondent, stimuli, scenesEvents)
annotationIntervals <- mockedPrivateGetIntervalsForAnnotations(study, respondent, stimuli, annotationsEvents)

test_that("return - imInterval object with all stimulus intervals", {
    intervals <- mockedGetRespondentIntervals(study, respondent, type = "Stimulus", stimIntervals = stimIntervals)
    checkImIntervalList(intervals, respondent, expectNIntervals = 4)
})

test_that("return - imInterval object with all scenes intervals", {
    intervals <- mockedGetRespondentIntervals(study, respondent, type = "Scene", sceneIntervals = sceneIntervals)
    checkImIntervalList(intervals, respondent, expectNIntervals = 2)
})

test_that("return - imInterval object with all annotations intervals", {
    intervals <- mockedGetRespondentIntervals(study, respondent, type = "Annotation",
                                              annotationIntervals = annotationIntervals)

    checkImIntervalList(intervals, respondent, expectNIntervals = 4)
})

test_that("return - imInterval object with all stimuli, scenes and annotations intervals", {
    intervals <- mockedGetRespondentIntervals(study, respondent, type = c("Stimulus", "Annotation", "Scene"),
                                              stimIntervals, sceneIntervals, annotationIntervals)

    checkImIntervalList(intervals, respondent, expectNIntervals = 10)
})

test_that("return - NULL if no intervals available", {
    # Retrieve intervals
    intervals <- mockedGetRespondentIntervals(study, respondent)
    expect_null(intervals, "no intervals should have been found")
})

test_that("return - imInterval in case of only one interval", {
    stimInterval <- stimIntervals[1, ]
    intervals <- mockedGetRespondentIntervals(study, respondent, type = "Stimulus", stimInterval)

    expect_s3_class(intervals, "imInterval")
    expect_equal(nrow(intervals), 1, info = "intervals should only contain a single interval")
    expect_equal(ncol(intervals), 10, info = "intervals should contain 10 columns")
    expect_identical(intervals$name, "IAAF", "interval name is not matching")
})
