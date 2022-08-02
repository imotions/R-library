context("getSensorsMetadata()")

library("imotionsApi")
library("mockery")
library("arrow")

# Load study and respondent
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))
respondent <- getRespondent(study, "09bd22e6-29b6-4a8a-8cc1-4780a5163e63")

mockedGetRespondentSensors <- function(study, respondent, stimulus = NULL) {
    getJSON_Stub <- mock(jsonlite::fromJSON("../data/respondentSensors.json", simplifyDataFrame = FALSE))

    sensors <- mockr::with_mock(
        getJSON = getJSON_Stub, {
            getRespondentSensors(study, respondent, stimulus)
        })

    return(sensors)
}


test_that("should throw errors if arguments are missing or not from the good class", {
    # in case of missing sensor
    error <- capture_error(getSensorsMetadata())
    expect_identical(error$message, "Please specify sensors loaded with `getRespondentSensors()`",
                     "missing `sensor` param not handled properly")

    # in case of sensor that is not an imSensor object
    error <- capture_error(getSensorsMetadata(sensor = "whatever"))
    expect_identical(error$message, "`sensors` argument is not an imSensor or imSensorList object",
                     "sensor not being an imSensor object should throw an error")
})

test_that("should return sensor metadata for a single sensor", {
    sensors <- mockedGetRespondentSensors(study, respondent)
    sensor <- sensors[3, ]
    metadata <- getSensorsMetadata(sensor)

    # check sensor metadata
    expect_equal(ncol(metadata), 8, info = "sensor metadata should contain 9 elements")
    expect_identical(metadata$ManuallyConfiguredChannelMapProfileName, "EmotivProfile",
                     "the LSL profile name in the sensor metadata should be correct")
})

test_that("should return sensors metadata for all sensors", {
    sensors <- mockedGetRespondentSensors(study, respondent)
    metadata <- getSensorsMetadata(sensors)

    # check sensor metadata
    expect_equal(ncol(metadata), 9, info = "sensor metadata should contain 9 elements")
    expect_equal(nrow(metadata), nrow(sensors), info = "sensor metadata should have the same number of row")
    expect_identical(metadata$Manufacturer, c(NA, NA, "Emotiv", "Emotiv"), "the manufacturer should be correct")
})
