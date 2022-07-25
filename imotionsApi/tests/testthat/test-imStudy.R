context("imStudy()")

library("imotionsApi")
library("mockery")

# Dummy connection
connection <- jsonlite::unserializeJSON(readLines("../data/imConnection.json"))

# Load first study
studyId <- "af8c2165-4389-4cc3-8b1e-b2d3a4bd8be1"
studyJSON <- jsonlite::read_json("../data/study.json", simplifyVector = TRUE)

# Load second study
studyId2 <- "2f8d3175-1234-4cc3-8b1e-b2d3a4bd8be1"
studyJSON2 <- jsonlite::read_json("../data/study_oneofeach.json", simplifyVector = TRUE)

mockedGetStudy <- function(connection, studyId, studyJSON, expectedJSONcall = 1) {
    getJSON_Stub <- mock(studyJSON)
    expectedUrl <- getStudyUrlById(connection, studyId)

    study <- mockr::with_mock(
        getJSON = getJSON_Stub, {
            imStudy(connection, studyId)
        })

    expect_called(getJSON_Stub, expectedJSONcall)

    if (expectedJSONcall > 0) {
        expect_args(getJSON_Stub, 1, connection, expectedUrl,
                    paste("Retrieving study with ID:", studyId))
    }

    return(study)
}

test_that("should throw errors if arguments are missing or not from the good class", {
    # in case of missing connection
    error <- capture_error(imStudy(studyId = studyId))
    expect_identical(error$message,
                     "imotionsApi: no open connection to iMotions. Please connect with `imConnection()`",
                     "missing `connection` param not handled properly")

    # in case of missing studyId
    error <- capture_error(imStudy(connection = connection))
    expect_identical(error$message,
                     "Please specify a studyId. Available studies can be found with `listStudies()`",
                     "missing `studyId` param not handled properly")

    # in case of connection that is not an imConnection object
    error <- capture_error(imStudy(connection = "whatever", studyId))
    expect_identical(error$message, "`connection` argument is not an imConnection object",
                     "connection not being an imConnection object should throw an error")
})

test_that("should return a imStudy object with respondents/stimuli/segments info", {
    # We should get a loading message the first time the study is loaded
    tmp <- capture_message(mockedGetStudy(connection, studyId, studyJSON))

    expect_identical(tmp$message,
                     "Loading the study 2 GSR 81 (id = af8c2165-4389-4cc3-8b1e-b2d3a4bd8be1) from the server...\n",
                     "wrong loading output")


    study <- mockedGetStudy(connection, studyId, studyJSON)

    expect_true(inherits(study, "imStudy"), "`study` should be an imStudy object")
    expect(nrow(study$stimuli) == 5, "the data.frame should contain 5 stimuli")
    expect(nrow(study$respondents) == 3, "the data.frame should contain 3 respondents")
    expect(nrow(study$segments) == 2, "the data.frame should contain 3 segments")

    # print should work as expected
    expect_output(print(study), "iMotions Study: 2 GSR 81\nStudy id: af8c2165-4389-4cc3-8b1e-b2d3a4bd8be1")
})

# create a new environment to check that the study is well saved
imotionsApiEnvironment <- new.env()
imotionsApiEnvironment$loadedStudies <- list()

test_that("environment saving of studies should work as expected", {
    # Loading the first study
    environment(mockedGetStudy) <- imotionsApiEnvironment
    study <- mockedGetStudy(connection, studyId, studyJSON)
    expect_identical(names(imotionsApiEnvironment$loadedStudies), studyId, "study should have been saved")

    # Checking that the first study has been saved and loaded correctly, getJSON should NOT be called
    study <-  mockedGetStudy(connection, studyId, studyJSON, expectedJSONcall = 0)

    # Loading the second study
    study2 <- mockedGetStudy(connection, studyId2, studyJSON2)
    expect_identical(names(imotionsApiEnvironment$loadedStudies), c(studyId, studyId2),
                     "both studies should been saved")
})

context("listStudies()")

studiesJSON <- jsonlite::read_json("../data/studies.json", simplifyVector = TRUE)

test_that("should throw errors if arguments are missing or not from the good class", {
    # in case of missing connection
    error <- capture_error(listStudies())
    expect_identical(error$message,
                     "imotionsApi: no open connection to iMotions. Please connect with `imConnection()`",
                     "missing `connection` param not handled properly")

    # in case of connection that is not an imConnection object
    error <- capture_error(listStudies(connection = "whatever"))
    expect_identical(error$message, "`connection` argument is not an imConnection object",
                     "connection not being an imConnection object should throw an error")
})

test_that("listing studies available should return two studies", {
    getJSON_Stub <- mock(studiesJSON)
    expectedUrl <- getStudiesUrl(connection)

    studies <- mockr::with_mock(
        getJSON = getJSON_Stub, {
            listStudies(connection)
        })

    expect_args(getJSON_Stub, 1, connection, expectedUrl, "Retrieving study list")

    # Checking that 2 studies are returned
    expect(nrow(studies) == 2, "the data.frame should contain 2 studies")
    expect_identical(colnames(studies), c("name", "id"), "the data.frame should have 2 columns (name, id)")
    expect_identical(studies$name, c("Great Commercials", "Mindshare Commercials"), "Wrong studies name output")
    expect_identical(studies$id, c("48b9109b-fd27-4151-a62d-b987d53934bc", "2f8d3175-1234-4cc3-8b1e-b2d3a4bd8be1"),
                     "Wrong studies id output")
})


context("listLoadedStudies()")

test_that("listing loaded studies should return two studies", {
    environment(listLoadedStudies) <- imotionsApiEnvironment
    loadedStudies <- listLoadedStudies()
    expect_identical(loadedStudies$id, c(studyId, studyId2), "listLoadedStudies studies should return two studies")
})


context("unloadStudies()")

test_that("unloading studies should empty the environment variables", {
    environment(unloadStudies) <- imotionsApiEnvironment
    environment(listLoadedStudies) <- imotionsApiEnvironment
    expect_equal(length(names(imotionsApiEnvironment$loadedStudies)), 2, info = "two studies should been saved")
    expect_message(unloadStudies(), "Studies successfully removed from the current session")
    loadedStudies <- listLoadedStudies()
    expect_identical(loadedStudies, NULL, "listLoadedStudies studies should not return any study anymore")
})
