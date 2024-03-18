# getSensorsMetadata ==================================================================================================
context("getSensorsMetadata()")

library(mockery)

# Load study and respondent
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))
respondent <- getRespondent(study, "09bd22e6-29b6-4a8a-8cc1-4780a5163e63")

mockedGetSensors <- function(study, respondent, stimulus = NULL) {
    getJSON_Stub <- mock(jsonlite::fromJSON("../data/respondentSensors.json", simplifyDataFrame = FALSE))

    sensors <- mockr::with_mock(getJSON = getJSON_Stub, {
        getSensors(study, respondent, stimulus)
    })

    return(sensors)
}


test_that("error - arguments are missing or not from the good class", {
    # in case of missing sensor
    expect_error(getSensorsMetadata(), "Please specify sensors loaded with `getSensors()`", fixed = TRUE,
                 info = "missing `sensor` param not handled properly")

    # in case of sensor that is not an imSensor object
    expect_error(getSensorsMetadata(sensor = "whatever"),
                 "`sensors` argument is not an imSensor or imSensorList object",
                 info = "sensor not being an imSensor object should throw an error")
})

test_that("local return - sensor metadata for a single sensor", {
    sensors <- mockedGetSensors(study, respondent)
    sensor <- sensors[3, ]
    metadata <- getSensorsMetadata(sensor)

    # check sensor metadata
    expect_equal(ncol(metadata), 8, info = "sensor metadata should contain 9 elements")
    expect_identical(metadata$ManuallyConfiguredChannelMapProfileName, "EmotivProfile",
                     "the LSL profile name in the sensor metadata should be correct")
})

test_that("local return - sensors metadata for all sensors", {
    sensors <- mockedGetSensors(study, respondent)
    metadata <- getSensorsMetadata(sensors)

    # check sensor metadata
    expect_equal(ncol(metadata), 9, info = "sensor metadata should contain 9 elements")
    expect_equal(nrow(metadata), nrow(sensors), info = "sensor metadata should have the same number of row")
    expect_identical(metadata$Manufacturer, c(NA, NA, "Emotiv", "Emotiv"), "the manufacturer should be correct")
})
