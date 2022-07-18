# privateGetIntervalsForStimuli =======================================================================================
context("privateGetIntervalsForStimuli()")

library("imotionsApi")
library("mockery")
library("data.table")

# Load study, respondent amd stimuli
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))
respondent <- getRespondent(study, "09bd22e6-29b6-4a8a-8cc1-4780a5163e63")
stimuli <- getStimuli(study, respondent)

# Load events
slideEvents <- arrow::read_parquet("../data/Native_SlideEvents.csv.pbin")
scenesEvents <- jsonlite::fromJSON("../data/scenes.json")
annotationsEvents <- jsonlite::fromJSON("../data/annotations.json")

# Load sensors
sensors <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imSensorList.json")))

mockedPrivateGetIntervalsForStimuli <- function(study, respondent,stimuli, slideEvents, sensors, expectedDataCall = 1) {
    getRespondentSensors_Stub <- mock(sensors)
    getSensorData_Stub <- mock(slideEvents)

    stimIntervals <- mockr::with_mock(getRespondentSensors = getRespondentSensors_Stub,
                                      getSensorData = getSensorData_Stub, {
                                          privateGetIntervalsForStimuli(study, respondent, stimuli)
                                      })

    expect_args(getRespondentSensors_Stub, 1, study = study, respondent = respondent)

    if (expectedDataCall > 0) {
        expect_args(getSensorData_Stub, 1, study = study, sensor = sensors[1, ])
    }

    return(stimIntervals)
}

test_that("should return a data.table of the good format", {
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
    expect_identical(unique(stimIntervals$text), NA_character_, "stimulus intervals should have no text")
})

test_that("should return NULL if no slideEvents sensors available", {
    sensors <- sensors[2, ]

    # Retrieve stimulus intervals and check returned value
    stimIntervals <- mockedPrivateGetIntervalsForStimuli(study, respondent, stimuli, slideEvents, sensors, 0)
    expect_null(stimIntervals, "no intervals should have been found")
})

# privateGetIntervalsForScenes ========================================================================================
context("privateGetIntervalsForScenes()");

mockedPrivateGetIntervalsForScenes <- function(study, respondent, stimuli, scenesEvents) {
    getJSON_Stub <- mock(scenesEvents)

    sceneIntervals <- mockr::with_mock(getJSON = getJSON_Stub, {
        privateGetIntervalsForScenes(study, respondent, stimuli)
    })

    expect_args(getJSON_Stub, 1, connection = study$connection, url = getRespondentScenesUrl(study, respondent),
                message = "Retrieving scenes for respondent Wendy")

    return(sceneIntervals)
}

test_that("should return a data.table of the good format and filter for not included scenes", {
    # Retrieve scenes intervals
    scenesIntervals <- mockedPrivateGetIntervalsForScenes(study, respondent, stimuli, scenesEvents)

    expect_equal(ncol(scenesIntervals), 9, info = "scene intervals should have 9 columns")
    expect_identical(unique(scenesIntervals$type), "Scene", "intervals should all be of scene type")
    expect_identical(scenesIntervals$parentId, c("1002", "1002"), "scene intervals should have a parent")
    expect_identical(scenesIntervals$parentName, c("IAAF", "IAAF"), "scene intervals should have a parent")
    expect_identical(scenesIntervals$name, c("IAAF_Scene[1]", "IAAF_Scene[1]"),"wrong scenes name")
    expect_equal(scenesIntervals$fragments.start, c(6075.691, 18440.691), 1e-2, info = "wrong fragments start")
    expect_equal(scenesIntervals$fragments.end, c(13478.69, 25209.69), 1e-2, info = "wrong fragments end")
    expect_equal(scenesIntervals$fragments.duration, c(7403, 6769), 1e-2, info = "wrong fragments duration")
    expect_identical(scenesIntervals$id, c("1008", "1008"), "wrong id")
    expect_identical(unique(scenesIntervals$text), NA_character_, "scenes intervals should have no text")
})

test_that("should return NULL if no scenes available", {
    # Retrieve scenes intervals and check returned value
    scenesEvents <- jsonlite::fromJSON("../data/no_scenes_annotations_aoidetails.json")
    scenesIntervals <- mockedPrivateGetIntervalsForScenes(study, respondent, stimuli, scenesEvents)
    expect_null(scenesIntervals, "no scenes should have been found")
})

# privateGetIntervalsForAnnotations ===================================================================================
context("privateGetIntervalsForAnnotations()");

mockedPrivateGetIntervalsForAnnotations <- function(study, respondent, stimuli, annotationsEvents) {
    getJSON_Stub <- mock(annotationsEvents)

    annotationsIntervals <- mockr::with_mock(getJSON = getJSON_Stub, {
        privateGetIntervalsForAnnotations(study, respondent, stimuli)
    })

    expect_args(getJSON_Stub, 1, connection = study$connection, url = getRespondentAnnotationsUrl(study, respondent),
                message = "Retrieving annotations for respondent Wendy")

    return(annotationsIntervals)
}

test_that("should return a data.table of the good format and filter for not included annotations", {
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

test_that("should return NULL if no annotations available", {
    # Retrieve scenes intervals and check returned value
    annotationsEvents <- jsonlite::fromJSON("../data/no_scenes_annotations_aoidetails.json")
    annotationsIntervals <- mockedPrivateGetIntervalsForAnnotations(study, respondent, stimuli, annotationsEvents)
    expect_null(annotationsIntervals, "no annotations should have been found")
})

# getRespondentIntervals ==============================================================================================
context("getRespondentIntervals()");

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

test_that("should throw errors if arguments are missing or not from the good class", {
    # in case of missing study
    error <- capture_error(getRespondentIntervals())
    expect_identical(error$message, "Please specify a study loaded with `imStudy()`",
                     "missing `study` param not handled properly")

    # in case of missing respondent
    error <- capture_error(getRespondentIntervals(study))
    expect_identical(error$message, "Please specify a respondent loaded with `getRespondents()`",
                     "missing `respondent` param not handled properly")

    # in case of study that is not an imStudy object
    error <- capture_error(getRespondentIntervals(study = "whatever", respondent))
    expect_identical(error$message, "`study` argument is not an imStudy object",
                     "study not being an imStudy object should throw an error")

    # in case of respondent that is not an imRespondent object
    error <- capture_error(getRespondentIntervals(study, respondent = "whatever"))
    expect_identical(error$message, "`respondent` argument is not an imRespondent object",
                     "respondent not being an imRespondent object should throw an error")

    # in case of type not in Stimulus, Scene and/or Annotation
    error <- capture_error(getRespondentIntervals(study, respondent, type = c("Stimulus", "whatever")))
    expect_identical(error$message, "`type` argument can only be set to Stimulus, Scene and/or Annotation",
                     "type not set properly should throw an error")
})

checkImIntervalList <- function(intervals, respondent, expectNIntervals) {
    # Checking dimensions and class
    expect_identical(intervals$respondent[[1]], respondent, "should have the correct respondent information")
    expect_equal(ncol(intervals), 10, info = "`intervals` should have 10 columns")
    expect_equal(nrow(intervals), expectNIntervals, info = paste("should have", expectNIntervals, "intervals"))
    expect_true(inherits(intervals, "imIntervalList"), "`intervals` should be an imIntervalList object")

    # check that taking only one intervals changes the class of the object
    interval <- intervals[1, ]
    expect_true(inherits(interval, "imInterval"), "`interval` should be an imInterval object")

    # check that only taking names and type of the list of intervals changes the class of the object
    intervals <- intervals[, c("name", "type")]
    expect_true(all(class(intervals) == c("data.table", "data.frame")), "truncated intervals should be data.table")
}

stimIntervals <- mockedPrivateGetIntervalsForStimuli(study, respondent, stimuli, slideEvents, sensors)
sceneIntervals <- mockedPrivateGetIntervalsForScenes(study, respondent, stimuli, scenesEvents)
annotationIntervals <- mockedPrivateGetIntervalsForAnnotations(study, respondent, stimuli, annotationsEvents)

test_that("should return a imInterval object with all stimulus intervals", {
    intervals <- mockedGetRespondentIntervals(study, respondent, type = "Stimulus", stimIntervals = stimIntervals)
    checkImIntervalList(intervals, respondent, expectNIntervals = 4)
})

test_that("should return a imInterval object with all scenes intervals", {
    intervals <- mockedGetRespondentIntervals(study, respondent, type = "Scene", sceneIntervals = sceneIntervals)
    checkImIntervalList(intervals, respondent, expectNIntervals = 2)
})

test_that("should return a imInterval object with all annotations intervals", {
    intervals <- mockedGetRespondentIntervals(study, respondent, type = "Annotation",
                                              annotationIntervals = annotationIntervals)

    checkImIntervalList(intervals, respondent, expectNIntervals = 4)
})

test_that("should return a imInterval object with all stimuli, scenes and annotations intervals", {
    intervals <- mockedGetRespondentIntervals(study, respondent, type = c("Stimulus", "Annotation", "Scene"),
                                              stimIntervals, sceneIntervals, annotationIntervals)

    checkImIntervalList(intervals, respondent, expectNIntervals = 10)
})

test_that("should not throw an error if no intervals are found", {
    # Retrieve intervals
    intervals <- mockedGetRespondentIntervals(study, respondent)
    expect_null(intervals, "no intervals should have been found")
})

test_that("getRespondentIntervals() in case of only one interval should return an imInterval object", {
    stimInterval <- stimIntervals[1, ]
    intervals <- mockedGetRespondentIntervals(study, respondent, type = "Stimulus", stimInterval)

    expect_true(inherits(intervals, "imInterval"), "`intervals` should be an imInterval object")
    expect(nrow(intervals) == 1, "intervals should only contain a single interval")
    expect(ncol(intervals) == 10, "intervals should contain 10 columns")
    expect_identical(intervals$name, c("IAAF"), "interval name is not matching")
})
