# imStudy =============================================================================================================
context("imStudy()")

library(mockery)

# Dummy connection
connection <- jsonlite::unserializeJSON(readLines("../data/imConnection.json"))
connection_cloud <- jsonlite::unserializeJSON(readLines("../data/imConnection_cloud.json"))

# Load first study
studyId <- "af8c2165-4389-4cc3-8b1e-b2d3a4bd8be1"
studyJSON <- jsonlite::read_json("../data/study.json", simplifyVector = TRUE)

# Load second study
studyId2 <- "2f8d3175-1234-4cc3-8b1e-b2d3a4bd8be1"
studyJSON2 <- jsonlite::read_json("../data/study_oneofeach.json", simplifyVector = TRUE)

# Load cloud study
studyId_cloud <- "9dd8bb91-14de-4a1f-9179-0d7700ec7a1d"
studyJSON_cloud <- jsonlite::read_json("../data/study_cloud.json", simplifyVector = TRUE)

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

test_that("error - arguments are missing or not from the good class", {
    # in case of missing connection
    expect_error(imStudy(studyId = studyId),
                 "imotionsApi: no open connection to iMotions. Please connect with `imConnection()`",
                 fixed = TRUE, info = "missing `connection` param not handled properly")

    # in case of missing studyId
    expect_error(imStudy(connection = connection),
                 "Please specify a studyId. Available studies can be found with `listStudies()`",
                 fixed = TRUE, info = "missing `studyId` param not handled properly")

    # in case of connection that is not an imConnection object
    expect_error(imStudy(connection = "whatever", studyId), "`connection` argument is not an imConnection object",
                 info = "connection not being an imConnection object should throw an error")
})

test_that("local return - imStudy object with respondents/stimuli/segments info", {
    # We should get a loading message the first time the study is loaded
    expect_message(study <- mockedGetStudy(connection, studyId, studyJSON),
                   "Loading the study 2 GSR 81 (id = af8c2165-4389-4cc3-8b1e-b2d3a4bd8be1) from the server...\n",
                   fixed = TRUE, info = "wrong loading output")

    expect_s3_class(study, "imStudy")
    expect_equal(nrow(study$stimuli), 6, info = "the data.frame should contain 6 stimuli")
    expect_equal(nrow(study$respondents), 3, info = "the data.frame should contain 3 respondents")
    expect_equal(nrow(study$segments), 2, info = "the data.frame should contain 2 segments")

    # print should work as expected
    expect_output(print(study), "iMotions Study: 2 GSR 81\nStudy id: af8c2165-4389-4cc3-8b1e-b2d3a4bd8be1")
})


test_that("remote return - imStudy object with respondents/stimuli/segments info", {
    # We should get a loading message the first time the study is loaded
    expect_message(study <- mockedGetStudy(connection_cloud, studyId_cloud, studyJSON_cloud),
                   "Loading the study RRRock The R (id = 9dd8bb91-14de-4a1f-9179-0d7700ec7a1d) from the server...\n",
                   fixed = TRUE, info = "wrong loading output")

    expect_s3_class(study, "imStudy")
    expect_equal(nrow(study$stimuli), 28, info = "the data.frame should contain 28 stimuli")
    expect_equal(nrow(study$respondents), 7, info = "the data.frame should contain 7 respondents")
    expect_equal(nrow(study$segments), 1, info = "the data.frame should contain 1 segments")

    # print should work as expected
    expect_output(print(study), "iMotions Study: RRRock The R\nStudy id: 9dd8bb91-14de-4a1f-9179-0d7700ec7a1d")
})

# create a new environment to check that the study is well saved
imotionsApiEnvironment <- new.env()
imotionsApiEnvironment$loadedStudies <- list()

test_that("check - environment saving of studies should work as expected", {
    # Loading the first study
    environment(mockedGetStudy) <- imotionsApiEnvironment
    study <- mockedGetStudy(connection, studyId, studyJSON)
    expect_named(imotionsApiEnvironment$loadedStudies, studyId, info = "study should have been saved")

    # Checking that the first study has been saved and loaded correctly, getJSON should NOT be called
    study <-  mockedGetStudy(connection, studyId, studyJSON, expectedJSONcall = 0)

    # Loading the second study
    study2 <- mockedGetStudy(connection, studyId2, studyJSON2)
    expect_named(imotionsApiEnvironment$loadedStudies, c(studyId, studyId2), info = "both studies should been saved")
})

# listStudies =========================================================================================================
context("listStudies()")

studiesJSON <- jsonlite::read_json("../data/studies.json", simplifyVector = TRUE)

test_that("error - arguments are missing or not from the good class", {
    # in case of missing connection
    expect_error(listStudies(), "imotionsApi: no open connection to iMotions. Please connect with `imConnection()`",
                 fixed = TRUE, info = "missing `connection` param not handled properly")

    # in case of connection that is not an imConnection object
    expect_error(listStudies(connection = "whatever"), "`connection` argument is not an imConnection object",
                 info = "connection not being an imConnection object should throw an error")
})

test_that("return - list with all the studies available", {
    getJSON_Stub <- mock(studiesJSON)
    expectedUrl <- getStudiesUrl(connection)

    studies <- mockr::with_mock(
        getJSON = getJSON_Stub, {
            listStudies(connection)
        })

    expect_args(getJSON_Stub, 1, connection, expectedUrl, "Retrieving study list")

    # Checking that 2 studies are returned
    expect_equal(nrow(studies), 2, info = "the data.frame should contain 2 studies")
    expect_named(studies, c("name", "id"), info = "the data.frame should have 2 columns (name, id)")
    expect_identical(studies$name, c("Great Commercials", "Mindshare Commercials"), "Wrong studies name output")
    expect_identical(studies$id, c("48b9109b-fd27-4151-a62d-b987d53934bc", "2f8d3175-1234-4cc3-8b1e-b2d3a4bd8be1"),
                     "Wrong studies id output")
})

# listLoadedStudies ===================================================================================================
context("listLoadedStudies()")

test_that("return - list of loaded studies", {
    environment(listLoadedStudies) <- imotionsApiEnvironment
    loadedStudies <- listLoadedStudies()
    expect_identical(loadedStudies$id, c(studyId, studyId2), "listLoadedStudies studies should return two studies")
})

# unloadStudies =======================================================================================================
context("unloadStudies()")

test_that("check - unloading studies should empty the environment variables", {
    environment(unloadStudies) <- imotionsApiEnvironment
    environment(listLoadedStudies) <- imotionsApiEnvironment
    expect_equal(length(names(imotionsApiEnvironment$loadedStudies)), 2, info = "two studies should been saved")
    expect_message(unloadStudies(), "Studies successfully removed from the current session")
    loadedStudies <- listLoadedStudies()
    expect_identical(loadedStudies, NULL, "listLoadedStudies studies should not return any study anymore")
})
