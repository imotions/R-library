library("imotionsApi");
library("stubthat");

context("getRespondentSensors()");

# Load study and respondent
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))
respondent <- getRespondent(study, "09bd22e6-29b6-4a8a-8cc1-4780a5163e63")
stimulus <- getStimulus(study, "1000")

# Create Stub
sensorsPath <- "../data/respondentSensors.json"
sensorsStimulusPath <- "../data/respondentSensorsStimulus.json"

mockedGetRespondentSensors <- function(study, respondent, stimulus = NULL) {
    getSensorsUrl_Stub <- stub(getSensorsUrl)
    getSensorsUrl_Stub$expects(study = study, imObject = respondent, stimulusId = stimulus$id)

    if (!is.null(stimulus)) {
        endpoint <- paste("respondent:", respondent$name, "stimulus:", stimulus$name)
    } else {
        endpoint <- paste("respondent:", respondent$name)
    }

    getSensorsUrl_Stub$withArgs(stimulusId = stimulus$id)$returns(sensorsStimulusPath)
    getSensorsUrl_Stub$withArgs(stimulusId = NULL)$returns(sensorsPath)

    getJSON_Stub <- stub(getJSON)
    getJSON_Stub$expects(connection = study$connection, message = paste("Retrieving sensors for", endpoint))
    getJSON_Stub$withArgs(url = sensorsStimulusPath)$returns(jsonlite::fromJSON(sensorsStimulusPath))
    getJSON_Stub$withArgs(url = sensorsPath)$returns(jsonlite::fromJSON(sensorsPath))

    sensors <- mockr::with_mock(
        getSensorsUrl = getSensorsUrl_Stub$f,
        getJSON = getJSON_Stub$f,
        getRespondentSensors(study, respondent, stimulus)
    )

    return(sensors)
}


test_that("should throw errors if arguments are missing or not from the good class", {
    # in case of missing study
    error <- capture_error(getRespondentSensors())
    expect_identical(error$message, "Please specify a study loaded with `imStudy()`",
                     "missing `study` param not handled properly")

    # in case of missing respondent
    error <- capture_error(getRespondentSensors(study))
    expect_identical(error$message, "Please specify a respondent loaded with `getRespondents()`",
                     "missing `respondent` param not handled properly")

    # in case of study that is not an imStudy object
    error <- capture_error(getRespondentSensors(study = "whatever", respondent))
    expect_identical(error$message, "`study` argument is not an imStudy object",
                     "study not being an imStudy object should throw an error")

    # in case of respondent that is not an imRespondent object
    error <- capture_error(getRespondentSensors(study, respondent = "whatever"))
    expect_identical(error$message, "`respondent` argument is not an imRespondent object",
                     "respondent not being an imRespondent object should throw an error")

    # in case of stimulus that is not an imStimulus object
    error <- capture_error(getRespondentSensors(study, respondent, stimulus = "whatever"))
    expect_identical(error$message, "`stimulus` argument is not an imStimulus object",
                     "stimulus not being an imStimulus object should throw an error")
})


test_that("should return a imSensorList object", {
    # Load sensors
    sensors <- mockedGetRespondentSensors(study, respondent)

    expect_true(inherits(sensors, "imSensorList"), "`sensors` should be an imSensorList object")
    expect_equal(nrow(sensors), 8, info = "`sensors` should contain 8 sensors")
    correctColumns <- c("eventSourceType", "name", "signals", "sensor", "instance", "dataUrl", "respondent")
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
    sensors <- mockedGetRespondentSensors(study, respondent, stimulus)

    expect_true(inherits(sensors, "imSensorList"), "`sensors` should be an imSensorList object")
    expect_equal(nrow(sensors), 5, info = "`sensors` should contain 5 sensors")
    correctColumns <- c("eventSourceType", "name", "signals", "sensor", "instance", "dataUrl", "respondent")
    expect_identical(colnames(sensors), correctColumns, "sensors need to have the correct columns in the correct order")

})

sensorsPath <- "../data/respondentSensor.json"
test_that("getRespondentSensors() in case of only one sensor should return an imSensor object", {
    # Load sensor
    sensors <- mockedGetRespondentSensors(study, respondent)

    expect_true(inherits(sensors, "imSensor"), "`sensors` should be an imSensor object")
    expect(nrow(sensors) == 1, "sensors should only contain a single sensor")
    expect_identical(sensors$name, "Eyetracker", "sensor name is not matching")
})
