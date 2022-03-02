library("imotionsApi");
library("stubthat");
library("data.table");

# Load study and respondent
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))
respondent <- getRespondent(study, "09bd22e6-29b6-4a8a-8cc1-4780a5163e63")

# Load events
slideEvents <- arrow::read_parquet("../data/Native_SlideEvents.csv.pbin")
scenesEvents <- jsonlite::fromJSON("../data/scenes.json")
annotationsEvents <- jsonlite::fromJSON("../data/annotations.json")

# Load sensors
sensors <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imSensorList.json")))

# privateGetIntervalsForStimuli =======================================================================================
context("privateGetIntervalsForStimuli()");

mockedPrivateGetIntervalsForStimuli <- function(study, respondent, slideEvents, sensors) {
    getRespondentSensors_Stub <- stub(getRespondentSensors)
    getRespondentSensors_Stub$expects(study = study, respondent = respondent)
    getRespondentSensors_Stub$returns(sensors)

    getSensorData_Stub <- stub(getSensorData)
    getSensorData_Stub$expects(study = study, sensor = sensors[1, ])
    getSensorData_Stub$returns(slideEvents)

    stimIntervals <- mockr::with_mock(getRespondentSensors = getRespondentSensors_Stub$f,
                                      getSensorData = getSensorData_Stub$f,
                                      privateGetIntervalsForStimuli(study, respondent))
}

test_that("should return a data.table of the good format", {
    # Retrieve stimulus intervals
    stimIntervals <- mockedPrivateGetIntervalsForStimuli(study, respondent, slideEvents, sensors)

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

test_that("should return NULL with a warning is no slideEvents sensors available", {
    sensors <- sensors[2, ]

    # Retrieve stimulus intervals and check returned value
    stimIntervals <- suppressWarnings(mockedPrivateGetIntervalsForStimuli(study, respondent, slideEvents, sensors))
    expect_null(stimIntervals, "no intervals should have been found")

    # Verify warning message
    warning <- capture_warning(mockedPrivateGetIntervalsForStimuli(study, respondent, slideEvents, sensors))
    expect_identical(warning$message, "No stimulus events found for this respondent.",
                     "missing stimulus events not handled properly")

})

# privateGetIntervalsForScenes ========================================================================================
context("privateGetIntervalsForScenes()");

mockedPrivateGetIntervalsForScenes <- function(study, respondent, scenesEvents) {
    getJSON_Stub <- stub(getJSON)
    getJSON_Stub$expects(connection = study$connection, message = "Retrieving scenes for respondent Wendy")
    getJSON_Stub$returns(scenesEvents)

    stimIntervals <- mockr::with_mock(getJSON = getJSON_Stub$f,
                                      privateGetIntervalsForScenes(study, respondent))
}

test_that("should return a data.table of the good format", {
    # Retrieve scenes intervals
    scenesIntervals <- mockedPrivateGetIntervalsForScenes(study, respondent, scenesEvents)

    expect_equal(ncol(scenesIntervals), 9, info = "scene intervals should have 9 columns")
    expect_identical(unique(scenesIntervals$type), "Scene", "intervals should all be of scene type")
    expect_identical(scenesIntervals$parentId, c("1000", "1002", "1002"), "scene intervals should have a parent")
    expect_identical(scenesIntervals$parentName, c("AntiSmoking40Sec", "IAAF", "IAAF"),
                     "scene intervals should have a parent")

    expect_identical(scenesIntervals$name, c("AntiSmoking40Sec_Scene[1]", "IAAF_Scene[1]", "IAAF_Scene[1]"),
                     "wrong scenes name")

    expect_equal(scenesIntervals$fragments.start, c(60904.847, 6075.691, 18440.691), 1e-2,
                 info = "wrong fragments start")

    expect_equal(scenesIntervals$fragments.end, c(85104.85, 13478.69, 25209.69), 1e-2,
                 info = "wrong fragments end")

    expect_equal(scenesIntervals$fragments.duration, c(24200, 7403, 6769), 1e-2, info = "wrong fragments duration")
    expect_identical(scenesIntervals$id, c("1004", "1008", "1008"), "wrong id")
    expect_identical(unique(scenesIntervals$text), NA_character_, "scenes intervals should have no text")
})

test_that("should return NULL with a warning is no scenes available", {
    # Retrieve scenes intervals and check returned value
    scenesEvents <- jsonlite::fromJSON("../data/no_scenes_annotations_aoidetails.json")
    scenesIntervals <- suppressWarnings(mockedPrivateGetIntervalsForScenes(study, respondent, scenesEvents))
    expect_null(scenesIntervals, "no scenes should have been found")

    # Verify warning message
    warning <- capture_warning(mockedPrivateGetIntervalsForScenes(study, respondent, scenesEvents))
    expect_identical(warning$message, "No scenes events found for this respondent.",
                     "missing scenes events not handled properly")
})

# privateGetIntervalsForAnnotations ===================================================================================
context("privateGetIntervalsForAnnotations()");

mockedPrivateGetIntervalsForAnnotations <- function(study, respondent, annotationsEvents) {
    getJSON_Stub <- stub(getJSON)
    getJSON_Stub$expects(connection = study$connection, message = "Retrieving annotations for respondent Wendy")
    getJSON_Stub$returns(annotationsEvents)

    stimIntervals <- mockr::with_mock(getJSON = getJSON_Stub$f,
                                      privateGetIntervalsForAnnotations(study, respondent))
}

test_that("should return a data.table of the good format", {
    # Retrieve annotations intervals
    annotationsIntervals <- mockedPrivateGetIntervalsForAnnotations(study, respondent, annotationsEvents)

    expect_equal(ncol(annotationsIntervals), 9, info = "annotations intervals should have 9 columns")
    expect_identical(unique(annotationsIntervals$type), "Annotation", "intervals should all be of annotation type")

    expect_identical(annotationsIntervals$parentId, c("1001", "1000", "1004", "1000", "1000"),
                     "annotation intervals should have a parent")

    expect_identical(annotationsIntervals$parentName,
                     c("CHAMONIX_Living_on_the_edge", "AntiSmoking40Sec", "AntiSmoking40Sec|AntiSmoking40Sec_Scene[1]",
                       "AntiSmoking40Sec", "AntiSmoking40Sec"), "annotation intervals should have a parent")

    expect_identical(annotationsIntervals$name, c("Test", "Test", "Test", "New annotation", "New annotation"),
                     "wrong annotations name")

    expect_equal(annotationsIntervals$fragments.start, c(54001.85, 66672.75, 68286.24, 65775.53, 73817.99), 1e-2,
                 info = "wrong fragments start")

    expect_equal(annotationsIntervals$fragments.end, c(56220.52, 77329.63, 74040.86, 70939.19, 77247.54), 1e-2,
                 info = "wrong fragments end")

    expect_equal(annotationsIntervals$fragments.duration, c(2218.667, 10656.885, 5754.613, 5163.659, 3429.550), 1e-2,
                 info = "wrong fragments duration")

    expect_identical(annotationsIntervals$id, c("1", "2", "3", "4", "4"), "wrong id")
    expect_identical(unique(annotationsIntervals$text), c("", "Test comment"), "annotation intervals should have text")
})

test_that("should return NULL with a warning is no annotations available", {
    # Retrieve scenes intervals and check returned value
    annotationsEvents <- jsonlite::fromJSON("../data/no_scenes_annotations_aoidetails.json")
    annotationsIntervals <- suppressWarnings(mockedPrivateGetIntervalsForAnnotations(study, respondent,
                                                                                     annotationsEvents))

    expect_null(annotationsIntervals, "no annotations should have been found")

    # Verify warning message
    warning <- capture_warning(mockedPrivateGetIntervalsForAnnotations(study, respondent, annotationsEvents))
    expect_identical(warning$message, "No annotations events found for this respondent.",
                     "missing annotations events not handled properly")
})

# getRespondentIntervals ==============================================================================================
context("getRespondentIntervals()");

mockedGetRespondentIntervals <- function(study, respondent, type = c("Stimulus", "Annotation", "Scene"),
                                         stimIntervals = NULL, sceneIntervals = NULL, annotationIntervals = NULL) {

    privateGetIntervalsForStimuli_Stub <- stub(privateGetIntervalsForStimuli)
    privateGetIntervalsForStimuli_Stub$expects(study = study, respondent = respondent)
    privateGetIntervalsForStimuli_Stub$returns(stimIntervals)

    privateGetIntervalsForScenes_Stub <- stub(privateGetIntervalsForScenes)
    privateGetIntervalsForScenes_Stub$expects(study = study, respondent = respondent)
    privateGetIntervalsForScenes_Stub$returns(sceneIntervals)

    privateGetIntervalsForAnnotations_Stub <- stub(privateGetIntervalsForAnnotations)
    privateGetIntervalsForAnnotations_Stub$expects(study = study, respondent = respondent)
    privateGetIntervalsForAnnotations_Stub$returns(annotationIntervals)

    intervals <- mockr::with_mock(privateGetIntervalsForStimuli = privateGetIntervalsForStimuli_Stub$f,
                                  privateGetIntervalsForScenes = privateGetIntervalsForScenes_Stub$f,
                                  privateGetIntervalsForAnnotations = privateGetIntervalsForAnnotations_Stub$f,
                                  getRespondentIntervals(study, respondent, type))

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

stimIntervals <- mockedPrivateGetIntervalsForStimuli(study, respondent, slideEvents, sensors)
sceneIntervals <- mockedPrivateGetIntervalsForScenes(study, respondent, scenesEvents)
annotationIntervals <- mockedPrivateGetIntervalsForAnnotations(study, respondent, annotationsEvents)

test_that("should return a imInterval object with all stimulus intervals", {
    intervals <- mockedGetRespondentIntervals(study, respondent, type = "Stimulus", stimIntervals = stimIntervals)
    checkImIntervalList(intervals, respondent, expectNIntervals = 4)
})

test_that("should return a imInterval object with all scenes intervals", {
    intervals <- mockedGetRespondentIntervals(study, respondent, type = "Scene", sceneIntervals = sceneIntervals)
    checkImIntervalList(intervals, respondent, expectNIntervals = 3)
})

test_that("should return a imInterval object with all annotations intervals", {
    intervals <- mockedGetRespondentIntervals(study, respondent, type = "Annotation",
                                              annotationIntervals = annotationIntervals)

    checkImIntervalList(intervals, respondent, expectNIntervals = 5)
})

test_that("should return a imInterval object with all stimuli, scenes and annotations intervals", {
    intervals <- mockedGetRespondentIntervals(study, respondent, type = c("Stimulus", "Annotation", "Scene"),
                                              stimIntervals, sceneIntervals, annotationIntervals)

    checkImIntervalList(intervals, respondent, expectNIntervals = 12)
})

test_that("should not throw an error if no intervals are found", {
    # Retrieve intervals
    intervals <- suppressWarnings(mockedGetRespondentIntervals(study, respondent))
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
