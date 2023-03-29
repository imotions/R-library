context("Testing `getUrls` functions")

library("imotionsApi")
library("mockery")

# Load study, respondent, stimulus and segment
studyId <- "af8c2165-4389-4cc3-8b1e-b2d3a4bd8be1"
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))
respondentId <- "09bd22e6-29b6-4a8a-8cc1-4780a5163e63"
respondent <- getRespondent(study, respondentId)
stimulusId <- "1000"
stimulus <- getStimulus(study, stimulusId)
segmentId <- "1010"
segment <- getSegment(study, segmentId)


# Load sensor
sensors <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imSensorList.json")))
sensor <- sensors[1, ]

# Load AOI
AOI <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imAOI.json")))

# Also checking remote study path
remoteStudy <- copy(study)
remoteStudy$connection$localIM <- FALSE

# Load sensor
sensorsCloud <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imSensorListCloud.json")))
sensorCloud <- sensorsCloud[1, ]

test_that("all getUrl function should work as expected for local connection", {

    #getStudyBaseUrl
    expect_identical(getStudyBaseUrl(study), "baseUrl")
    expect_identical(getStudyBaseUrl(remoteStudy), "baseUrl")

    #getStudyS3BaseUrl
    expect_identical(getStudyS3BaseUrl(study), "baseUrl")
    expect_identical(getStudyS3BaseUrl(remoteStudy), "s3BaseUrl")

    #getStudiesUrl
    expect_identical(getStudiesUrl(study$connection), "baseUrl/studies")
    expect_identical(getStudiesUrl(remoteStudy$connection), "baseUrl/studies")

    #getStudyUrl
    studyUrl <- "baseUrl/studies/af8c2165-4389-4cc3-8b1e-b2d3a4bd8be1"
    expect_identical(getStudyUrl(study), studyUrl)
    expect_identical(getStudyUrl(remoteStudy), studyUrl)

    #getStudyUrlById
    expect_identical(getStudyUrlById(study$connection, studyId), studyUrl)
    expect_identical(getStudyUrlById(remoteStudy$connection, studyId), studyUrl)

    #getSensorsUrl
    sensorsUrl <- paste0(studyUrl, "/respondent/09bd22e6-29b6-4a8a-8cc1-4780a5163e63/samples")
    sensorStimulusUrl <- paste0(studyUrl, "/respondent/09bd22e6-29b6-4a8a-8cc1-4780a5163e63/stimuli/1000/samples")
    sensorsSegmentUrl <- paste0(studyUrl, "/segment/1010/stimuli/1000/samples")
    expect_identical(getSensorsUrl(study, respondent), sensorsUrl)
    expect_identical(getSensorsUrl(study, respondent, stimulus), sensorStimulusUrl)
    expect_identical(getSensorsUrl(study, segment, stimulus), sensorsSegmentUrl)
    expect_identical(getSensorsUrl(remoteStudy, respondent), sensorsUrl)
    expect_identical(getSensorsUrl(remoteStudy, respondent, stimulus), sensorStimulusUrl)

    #getSensorDataUrl
    sensorDataUrl <- paste0("baseUrl", sensor$dataUrl)
    expect_identical(getSensorDataUrl(study, sensor), sensorDataUrl)
    sensorDataUrl <- file.path("s3BaseUrl", sensorCloud$dataUrl)
    expect_identical(getSensorDataUrl(remoteStudy, sensorCloud), sensorDataUrl)

    #getUploadSensorsUrl
    expect_identical(getUploadSensorsUrl(study, respondent), paste0(sensorsUrl, "/data"))
    expect_identical(getUploadSensorsUrl(study, respondent, stimulus), paste0(sensorStimulusUrl, "/data"))
    expect_identical(getUploadSensorsUrl(study, segment, stimulus), paste0(sensorsSegmentUrl, "/data"))
    uploadCloudUrl <- "baseUrl/reportruns/placeholder_reportId/respondents/09bd22e6-29b6-4a8a-8cc1-4780a5163e63"
    expect_identical(getUploadSensorsUrl(remoteStudy, respondent), uploadCloudUrl)

    #getAOIsUrl
    AOIUrl <- "baseUrl/aois/af8c2165-4389-4cc3-8b1e-b2d3a4bd8be1"
    expect_identical(getAOIsUrl(study), AOIUrl)
    expect_identical(getAOIsUrl(study, stimulusId), paste0(AOIUrl, "/stimuli/1000"))
    expect_identical(getAOIsUrl(study, respondentId = respondentId),
                     paste0(AOIUrl, "/respondent/09bd22e6-29b6-4a8a-8cc1-4780a5163e63"))

    error <- capture_error(getAOIsUrl(study, stimulusId, respondentId))
    expect_identical(error$message, "Please provide either stimulusId or respondentId, not both.",
                     "too many params not handled properly")

    #getUploadEventsUrl
    eventUrl <- "baseUrl/revents/af8c2165-4389-4cc3-8b1e-b2d3a4bd8be1"
    expect_identical(getUploadEventsUrl(study, respondent),
                     paste0(eventUrl, "/respondent/09bd22e6-29b6-4a8a-8cc1-4780a5163e63/data"))

    #getUploadMetricsUrl
    metricsUrl <- "baseUrl/rmetrics/af8c2165-4389-4cc3-8b1e-b2d3a4bd8be1"
    expect_identical(getUploadMetricsUrl(study, respondent),
                     paste0(metricsUrl, "/respondent/09bd22e6-29b6-4a8a-8cc1-4780a5163e63/data"))

    #getAOIDetailsForStimulusUrl
    expect_identical(getAOIDetailsUrl(study, stimulus), paste0(AOIUrl, "/stimuli/", stimulusId, "/*"))
    expect_identical(getAOIDetailsUrl(study, stimulus, respondent),
                     paste0(AOIUrl, "/stimuli/", stimulusId, "/respondent/", respondentId, "/*"))

    #getAOIDetailsUrl
    expect_identical(getAOIDetailsUrl(study, AOI), paste0(AOIUrl, "/stimuli/", AOI$stimulusId, "/", AOI$id))
    expect_identical(getAOIDetailsUrl(study, AOI, respondent),
                     paste0(AOIUrl, "/stimuli/", AOI$stimulusId, "/respondent/", respondentId, "/", AOI$id))

    #getRespondentScenesUrl
    sceneUrl <- paste0("baseUrl/scenes/af8c2165-4389-4cc3-8b1e-b2d3a4bd8be1/",
                       "respondent/09bd22e6-29b6-4a8a-8cc1-4780a5163e63")
    expect_identical(getRespondentScenesUrl(study, respondent), sceneUrl)

    #getRespondentAnnotationsUrl
    annotationsUrl <- paste0("baseUrl/annotations/af8c2165-4389-4cc3-8b1e-b2d3a4bd8be1/",
                             "respondent/09bd22e6-29b6-4a8a-8cc1-4780a5163e63")
    expect_identical(getRespondentAnnotationsUrl(study, respondent), annotationsUrl)
})
