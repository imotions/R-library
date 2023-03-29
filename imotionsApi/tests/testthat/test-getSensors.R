context("getSensors()")

library("imotionsApi")
library("mockery")

# Load study and respondent
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))
respondent <- getRespondent(study, "09bd22e6-29b6-4a8a-8cc1-4780a5163e63")
segment <- getSegment(study, "1010")
stimulus <- getStimulus(study, "1000")

# Create Stub
sensorsRespondentPath <- "../data/respondentSensors.json"
sensorsSegmentPath <- "../data/segmentSensors.json"
sensorsCloudPath <- "../data/respondentSensorsCloud.json"
sensorsStimulusPath <- "../data/respondentSensorsStimulus.json"

mockedGetSensors <- function(study, target, stimulus = NULL, expectedEndpoint) {
    # Replace url to load test data
    mockUrl <- function(url) {
        if (!study$connection$localIM) {
            return(sensorsCloudPath)
        } else if (grepl("respondent", url)) {
            if (grepl("stimuli", url)) {
                return(sensorsStimulusPath)
            } else {
                return(sensorsRespondentPath)
            }
        } else {
            return(sensorsSegmentPath)
        }
    }

    expectedUrl <- getSensorsUrl(study, target, stimulus)
    getJSON_Stub <- mock(jsonlite::fromJSON(mockUrl(expectedUrl), simplifyDataFrame = FALSE))

    sensors <- mockr::with_mock(
        getJSON = getJSON_Stub, {
            getSensors(study, target, stimulus)
        })

    expect_args(getJSON_Stub, 1, connection = study$connection, url = expectedUrl, message = expectedEndpoint,
                simplifyDataFrame = FALSE)

    return(sensors)
}


test_that("should throw errors if arguments are missing or not from the good class", {
    # in case of missing study
    error <- capture_error(getSensors())
    expect_identical(error$message, "Please specify a study loaded with `imStudy()`",
                     "missing `study` param not handled properly")

    # in case of missing target
    error <- capture_error(getSensors(study))
    expect_identical(error$message,
                     "Please specify a target respondent/segment loaded with `getRespondents()` or `getSegments()`",
                     "missing `target` param not handled properly")

    # in case of study that is not an imStudy object
    error <- capture_error(getSensors(study = "whatever", respondent))
    expect_identical(error$message, "`study` argument is not an imStudy object",
                     "study not being an imStudy object should throw an error")

    # in case of target that is not an imRespondent/imSegment object
    error <- capture_error(getSensors(study, target = "whatever"))
    expect_identical(error$message, "`target` argument is not an imRespondent or imSegment object",
                     "target not being an imRespondent/imSegment object should throw an error")

    # in case of stimulus that is not an imStimulus object
    error <- capture_error(getSensors(study, respondent, stimulus = "whatever"))
    expect_identical(error$message, "`stimulus` argument is not an imStimulus object",
                     "stimulus not being an imStimulus object should throw an error")

    # in case of stimulus missing for a segment target
    error <- capture_error(getSensors(study, segment))
    expect_identical(error$message, "Please specify a stimulus to get sensors available for a segment target",
                     "missing `stimulus` param for segment target not handled properly")
})


test_that("should return a imSensorList object", {
    # Load sensors
    expectedEndpoint <- "Retrieving sensors for respondent: Wendy"
    sensors <- mockedGetSensors(study, respondent, expectedEndpoint = expectedEndpoint)

    expect_true(inherits(sensors, "imSensorList"), "`sensors` should be an imSensorList object")
    expect_equal(nrow(sensors), 4, info = "`sensors` should contain 4 sensors")
    correctColumns <- c("eventSourceType", "name", "signals", "sensor", "instance", "dataUrl", "respondent",
                        "sensorSpecific", "signalsMetaData")
    expect_identical(colnames(sensors), correctColumns, "sensors need to have the correct columns in the correct order")

    # check that taking only one sensor changes the class of the object
    sensor <- sensors[1, ]
    expect_true(inherits(sensor, "imSensor"), "`sensor` should be an imSensor object")

    # check that only taking names of the list of sensors changes the class of the object
    sensors <- sensors[, c("name", "sensor")]
    expect_true(all(class(sensors) == c("data.table", "data.frame")), "truncated sensors should be data.table")
})

test_that("should return a imSensorList object at the stimulus level", {
    # Load sensors
    expectedEndpoint <- "Retrieving sensors for respondent: Wendy, stimulus: AntiSmoking40Sec"
    sensors <- mockedGetSensors(study, respondent, stimulus, expectedEndpoint)

    expect_true(inherits(sensors, "imSensorList"), "`sensors` should be an imSensorList object")
    expect_equal(nrow(sensors), 5, info = "`sensors` should contain 5 sensors")
    correctColumns <- c("eventSourceType", "name", "signals", "sensor", "instance", "dataUrl", "respondent",
                        "signalsMetaData")
    expect_identical(colnames(sensors), correctColumns, "sensors need to have the correct columns in the correct order")

})

test_that("should return a imSensorList object when the object comes from the Cloud", {
    # Load sensors
    study$connection$localIM <- FALSE
    expectedEndpoint <- "Retrieving sensors for respondent: Wendy"
    sensors <- mockedGetSensors(study, respondent, expectedEndpoint = expectedEndpoint)
    expect_true(inherits(sensors, "imSensorList"), "`sensors` should be an imSensorList object")
    expect_equal(nrow(sensors), 3, info = "`sensors` should contain 3 sensors")
    correctColumns <- c("eventSourceType", "name", "signals", "sensor", "instance", "dataUrl", "respondent",
                        "sensorSpecific", "signalsMetaData", "fileName")

    expect_identical(colnames(sensors), correctColumns, "sensors need to have the correct columns in the correct order")

    # check that taking only one sensor changes the class of the object
    sensor <- sensors[1, ]
    expect_true(inherits(sensor, "imSensor"), "`sensor` should be an imSensor object")

    # check that only taking names of the list of sensors changes the class of the object
    sensors <- sensors[, c("name", "sensor")]
    expect_true(all(class(sensors) == c("data.table", "data.frame")), "truncated sensors should be data.table")
})

test_that("should return a imSensorList object for a segment target", {
    # Load sensors
    expectedEndpoint <- "Retrieving sensors for segment: 2 GSR 81-1, stimulus: AntiSmoking40Sec"
    sensors <- mockedGetSensors(study, segment, stimulus, expectedEndpoint)

    expect_true(inherits(sensors, "imSensor"), "`sensors` should be an imSensor object")
    expect_equal(nrow(sensors), 1, info = "`sensors` should contain 1 sensor")
    correctColumns <- c("eventSourceType", "name", "signals", "sensor", "instance", "dataUrl", "segment",
                        "sensorSpecific", "signalsMetaData")
    expect_identical(colnames(sensors), correctColumns, "sensors need to have the correct columns in the correct order")
})

sensorsRespondentPath <- "../data/respondentSensor.json"
test_that("getSensors() in case of only one sensor should return an imSensor object", {
    # Load sensor
    expectedEndpoint <- "Retrieving sensors for respondent: Wendy"
    sensors <- mockedGetSensors(study, respondent, expectedEndpoint = expectedEndpoint)

    expect_true(inherits(sensors, "imSensor"), "`sensors` should be an imSensor object")
    expect(nrow(sensors) == 1, "sensors should only contain a single sensor")
    expect_identical(sensors$name, "Eyetracker", "sensor name is not matching")
})
