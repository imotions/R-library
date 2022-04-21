context("getSensorMetaData()")

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
    error <- capture_error(getSensorMetaData())
    expect_identical(error$message, "Please specify a sensor loaded with `getRespondentSensors()`",
                     "missing `sensor` param not handled properly")

    # in case of sensor that is not an imSensor object
    error <- capture_error(getSensorMetaData(sensor = "whatever"))
    expect_identical(error$message, "`sensor` argument is not an imSensor object",
                     "sensor not being an imSensor object should throw an error")
})

test_that("should return sensor metadata for this sensor", {
    sensors <- mockedGetRespondentSensors(study, respondent)
    sensor <- sensors[3, ]
    metadata <- getSensorMetaData(sensor)

    expect_identical(names(metadata), c("sensor", "signals"),
                     "metadata should have two elements with the specified names")

    # check sensor metadata
    sensorMetaData <- metadata$sensor
    expect_equal(length(sensorMetaData), 9, info = "sensor metadata should contain 9 elements")
    expect_identical(sensorMetaData$ManuallyConfiguredChannelMapProfileName, "EmotivProfile",
                     "the LSL profile name in the sensor metadata should be correct")

    # check signal metadata
    signalsMetaData <- metadata$signals
    expect_true(all(class(signalsMetaData) == c("data.table", "data.frame")), "signals metadata should be a data frame")
    expect_equal(ncol(signalsMetaData), 14, info = "signals metadata should have the correct number of columns")
    expect_true(all(sensor$signals[[1]] == signalsMetaData$name),
                "signals metadata should have all the signals of this sensor in the name column")

})
