library("imotionsApi");
library("stubthat");

context("Testing `getUrls` functions");

# Load study and respondent
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))
respondentId <- "09bd22e6-29b6-4a8a-8cc1-4780a5163e63"
respondent <- getRespondent(study, respondentId)
stimulusId <- "1000"
stimulus <- getStimulus(study, stimulusId)
studyId <- "af8c2165-4389-4cc3-8b1e-b2d3a4bd8be1"

# Load sensor
sensors <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imSensorList.json")))
sensor <- sensors[1, ]

# Load AOI
AOI <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imAOI.json")))

test_that("all getUrl function should work as expected", {
    #getStudyBaseUrl
    expect_identical(getStudyBaseUrl(study), "https://my.imotions.com/testcustom")

    #getStudiesUrl
    expect_identical(getStudiesUrl(study$connection), "https://my.imotions.com/testcustom/studies")

    #getStudyUrl
    studyUrl <- "https://my.imotions.com/testcustom/studies/af8c2165-4389-4cc3-8b1e-b2d3a4bd8be1"
    expect_identical(getStudyUrl(study), studyUrl)

    #getStudyUrlById
    expect_identical(getStudyUrlById(study$connection, studyId), studyUrl)

    #getStudySpecificUrl
    expect_identical(getStudySpecificUrl(study), studyUrl)
    remoteStudy <- copy(study)
    remoteStudy$connection$localIM <- FALSE
    expect_identical(getStudySpecificUrl(remoteStudy), "https://my.imotions.com/testcustom")

    #getSensorsUrl
    sensorsUrl <- paste0(studyUrl, "/respondent/09bd22e6-29b6-4a8a-8cc1-4780a5163e63/samples")
    sensorStimulusUrl <- paste0(studyUrl, "/respondent/09bd22e6-29b6-4a8a-8cc1-4780a5163e63/stimuli/1000/samples")
    expect_identical(getSensorsUrl(study, respondent), sensorsUrl)
    expect_identical(getSensorsUrl(study, respondent, stimulus), sensorStimulusUrl)

    #getSensorDataUrl
    sensorDataUrl <- paste0("https://my.imotions.com/testcustom", sensor$dataUrl)
    expect_identical(getSensorDataUrl(study, sensor), sensorDataUrl)

    #getUploadSensorsUrl
    expect_identical(getUploadSensorsUrl(study, respondent), paste0(sensorsUrl, "/data"))
    expect_identical(getUploadSensorsUrl(study, respondent, stimulus), paste0(sensorStimulusUrl, "/data"))

    #getAOIsUrl
    AOIUrl <- "https://my.imotions.com/testcustom/aois/af8c2165-4389-4cc3-8b1e-b2d3a4bd8be1"
    expect_identical(getAOIsUrl(study), AOIUrl)
    expect_identical(getAOIsUrl(study, stimulusId), paste0(AOIUrl, "/stimuli/1000"))
    expect_identical(getAOIsUrl(study, respondentId = respondentId),
                     paste0(AOIUrl, "/respondent/09bd22e6-29b6-4a8a-8cc1-4780a5163e63"))

    error <- capture_error(getAOIsUrl(study, stimulusId, respondentId))
    expect_identical(error$message, "Please provide either stimulusId or respondentId, not both.",
                     "too many params not handled properly")

    #getAOIDetailsForStimulusUrl
    expect_identical(getAOIDetailsUrl(study, stimulus), paste0(AOIUrl, "/stimuli/", stimulusId, "/*"))
    expect_identical(getAOIDetailsUrl(study, stimulus, respondent),
                     paste0(AOIUrl, "/stimuli/", stimulusId, "/respondent/", respondentId, "/*"))

    #getAOIDetailsUrl
    expect_identical(getAOIDetailsUrl(study, AOI), paste0(AOIUrl, "/stimuli/", AOI$stimulusId, "/", AOI$id))
    expect_identical(getAOIDetailsUrl(study, AOI, respondent),
                     paste0(AOIUrl, "/stimuli/", AOI$stimulusId, "/respondent/", respondentId, "/", AOI$id))

    #getRespondentScenesUrl
    sceneUrl <- paste0("https://my.imotions.com/testcustom/scenes/af8c2165-4389-4cc3-8b1e-b2d3a4bd8be1/",
                       "respondent/09bd22e6-29b6-4a8a-8cc1-4780a5163e63")
    expect_identical(getRespondentScenesUrl(study, respondent), sceneUrl)

    #getRespondentAnnotationsUrl
    annotationsUrl <- paste0("https://my.imotions.com/testcustom/annotations/af8c2165-4389-4cc3-8b1e-b2d3a4bd8be1/",
                             "respondent/09bd22e6-29b6-4a8a-8cc1-4780a5163e63")
    expect_identical(getRespondentAnnotationsUrl(study, respondent), annotationsUrl)
})
