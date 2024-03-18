# getSensors ==========================================================================================================
context("getSensors()")

library(mockery)

# Load study and respondent
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))
study_cloud <- jsonlite::unserializeJSON(readLines("../data/imStudy_cloud.json"))
respondent <- getRespondent(study, "09bd22e6-29b6-4a8a-8cc1-4780a5163e63")
segment <- getSegment(study, "1010")
stimulus <- getStimulus(study, "1000")

# Create Stub
sensorsRespondentPath <- "../data/respondentSensors.json"
sensorsSegmentPath <- "../data/segmentSensors.json"
sensorsCloudPath <- "../data/respondentSensors_cloud.json"
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

    sensors <- mockr::with_mock(getJSON = getJSON_Stub, {
        getSensors(study, target, stimulus)
    })

    expect_args(getJSON_Stub, 1, connection = study$connection, url = expectedUrl, message = expectedEndpoint,
                simplifyDataFrame = FALSE)

    return(sensors)
}


test_that("error - arguments are missing or not from the good class", {
    # in case of missing study
    expect_error(getSensors(), "Please specify a study loaded with `imStudy()`", fixed = TRUE,
                 info = "missing `study` param not handled properly")

    # in case of missing target
    expect_error(getSensors(study),
                 "Please specify a target respondent/segment loaded with `getRespondents()` or `getSegments()`",
                 fixed = TRUE, info = "missing `target` param not handled properly")

    # in case of study that is not an imStudy object
    expect_error(getSensors(study = "whatever", respondent), "`study` argument is not an imStudy object",
                 info = "study not being an imStudy object should throw an error")

    # in case of target that is not an imRespondent/imSegment object
    expect_error(getSensors(study, target = "whatever"), "`target` argument is not an imRespondent or imSegment object",
                 info = "target not being an imRespondent/imSegment object should throw an error")

    # in case of stimulus that is not an imStimulus object
    expect_error(getSensors(study, respondent, stimulus = "whatever"),
                 "`stimulus` argument is not an imStimulus object",
                 info = "stimulus not being an imStimulus object should throw an error")

    # in case of stimulus missing for a segment target
    expect_error(getSensors(study, segment), "Please specify a stimulus to get sensors available for a segment target",
                 info = "missing `stimulus` param for segment target not handled properly")
})


test_that("local return - imSensorList object", {
    # Load sensors
    expectedEndpoint <- "Retrieving sensors for respondent: Wendy"
    sensors <- mockedGetSensors(study, respondent, expectedEndpoint = expectedEndpoint)
    expectedColumns <- c("eventSourceType", "name", "signals", "sensor", "instance", "dataUrl", "respondent",
                         "sensorSpecific", "signalsMetaData")

    expect_s3_class(sensors, "imSensorList")
    expect_equal(nrow(sensors), 4, info = "`sensors` should contain 4 sensors")
    expect_named(sensors, expectedColumns, info = "sensors need to have the correct columns in the correct order")

    # check that taking only one sensor changes the class of the object
    sensor <- sensors[1, ]
    expect_s3_class(sensor, "imSensor")

    # check that only taking names of the list of sensors changes the class of the object
    sensors <- sensors[, c("name", "sensor")]
    expect_s3_class(sensors, c("data.table", "data.frame"), exact = TRUE)
})

test_that("remote return - imSensorList object", {
    respondent <- getRespondents(study_cloud)[1, ]
    expectedEndpoint <- "Retrieving sensors for respondent: bab55356-43fc-4c25-a39d-a1d513965614"
    sensors <- mockedGetSensors(study_cloud, respondent, expectedEndpoint = expectedEndpoint)
    expectedColumns <- c("eventSourceType", "name", "signals", "sensor", "instance", "dataUrl", "respondent",
                         "sensorSpecific", "signalsMetaData", "fileName")

    expect_s3_class(sensors, "imSensorList")
    expect_equal(nrow(sensors), 3, info = "`sensors` should contain 3 sensors")
    expect_named(sensors, expectedColumns, info = "sensors need to have the correct columns in the correct order")

    # check that taking only one sensor changes the class of the object
    sensor <- sensors[1, ]
    expect_s3_class(sensor, "imSensor")

    # check that only taking names of the list of sensors changes the class of the object
    sensors <- sensors[, c("name", "sensor")]
    expect_s3_class(sensors, c("data.table", "data.frame"), exact = TRUE)
})

test_that("local return - imSensorList object at the stimulus level (gazemapping for example)", {
    # Load sensors
    expectedEndpoint <- "Retrieving sensors for respondent: Wendy, stimulus: AntiSmoking40Sec"
    sensors <- mockedGetSensors(study, respondent, stimulus, expectedEndpoint)
    expectedColumns <- c("eventSourceType", "name", "signals", "sensor", "instance", "dataUrl", "respondent",
                         "signalsMetaData")

    expect_s3_class(sensors, "imSensorList")
    expect_equal(nrow(sensors), 5, info = "`sensors` should contain 5 sensors")
    expect_named(sensors, expectedColumns, info = "sensors need to have the correct columns in the correct order")
})

test_that("local return - imSensorList object for a segment target", {
    # Load sensors
    expectedEndpoint <- "Retrieving sensors for segment: 2 GSR 81-1, stimulus: AntiSmoking40Sec"
    sensors <- mockedGetSensors(study, segment, stimulus, expectedEndpoint)
    expectedColumns <- c("eventSourceType", "name", "signals", "sensor", "instance", "dataUrl", "segment",
                         "sensorSpecific", "signalsMetaData")

    expect_s3_class(sensors, "imSensor")
    expect_equal(nrow(sensors), 1, info = "`sensors` should contain 1 sensor")
    expect_named(sensors, expectedColumns, info = "sensors need to have the correct columns in the correct order")
})

sensorsRespondentPath <- "../data/respondentSensor.json"
test_that("local return - imSensor in case of only one sensor", {
    # Load sensor
    expectedEndpoint <- "Retrieving sensors for respondent: Wendy"
    sensors <- mockedGetSensors(study, respondent, expectedEndpoint = expectedEndpoint)

    expect_s3_class(sensors, "imSensor")
    expect_equal(nrow(sensors), 1, info = "sensors should only contain a single sensor")
    expect_identical(sensors$name, "Eyetracker", "sensor name is not matching")
})

sensorsRespondentPath <- "../data/no_scenes_annotations_aoidetails.json"

test_that("warning - no sensors was found", {
    # Load sensor
    expectedEndpoint <- "Retrieving sensors for respondent: Wendy"

    expect_warning(sensors <- mockedGetSensors(study, respondent, expectedEndpoint = expectedEndpoint),
                   "No sensors found for respondent: Wendy",
                   info = "not sensors found not handled properly")

    expect_null(sensors, "`sensors` should be NULL")
})
