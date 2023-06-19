# getUrls functions ===================================================================================================
context("Testing `getUrls` functions")

library(mockery)

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

test_that("local - all getUrl function should work as expected", {
    #getStudyBaseUrl
    baseUrl <- "http://localhost:8086"
    expect_identical(getStudyBaseUrl(study), baseUrl)

    #getStudyS3BaseUrl
    expect_identical(getStudyS3BaseUrl(study), baseUrl)

    #getStudiesUrl
    expect_identical(getStudiesUrl(study$connection), "http://localhost:8086/studies")

    #getStudyUrl
    studyUrl <- "http://localhost:8086/studies/af8c2165-4389-4cc3-8b1e-b2d3a4bd8be1"
    expect_identical(getStudyUrl(study), studyUrl)

    #getStudyUrlById
    expect_identical(getStudyUrlById(study$connection, studyId), studyUrl)

    #getSensorsUrl
    sensorsUrl <- paste0(studyUrl, "/respondent/09bd22e6-29b6-4a8a-8cc1-4780a5163e63/samples")
    sensorStimulusUrl <- paste0(studyUrl, "/respondent/09bd22e6-29b6-4a8a-8cc1-4780a5163e63/stimuli/1000/samples")
    sensorsSegmentUrl <- paste0(studyUrl, "/segment/1010/stimuli/1000/samples")

    expect_identical(getSensorsUrl(study, respondent), sensorsUrl)
    expect_identical(getSensorsUrl(study, respondent, stimulus), sensorStimulusUrl)
    expect_identical(getSensorsUrl(study, segment, stimulus), sensorsSegmentUrl)

    #getSensorDataUrl
    sensorDataUrl <- paste0("http:/localhost:8086", sensor$dataUrl)
    expect_identical(getSensorDataUrl(study, sensor), sensorDataUrl)

    #getUploadSensorsUrl
    expect_identical(getUploadSensorsUrl(study, respondent), paste0(sensorsUrl, "/data"))
    expect_identical(getUploadSensorsUrl(study, respondent, stimulus), paste0(sensorStimulusUrl, "/data"))
    expect_identical(getUploadSensorsUrl(study, segment, stimulus), paste0(sensorsSegmentUrl, "/data"))

    #getAOIsUrl
    AOIUrl <- paste0(baseUrl, "/aois/af8c2165-4389-4cc3-8b1e-b2d3a4bd8be1")
    expect_identical(getAOIsUrl(study), AOIUrl)
    expect_identical(getAOIsUrl(study, stimulusId), paste0(AOIUrl, "/stimuli/1000"))
    expect_identical(getAOIsUrl(study, respondentId = respondentId),
                     paste0(AOIUrl, "/respondent/09bd22e6-29b6-4a8a-8cc1-4780a5163e63"))

    expect_error(getAOIsUrl(study, stimulusId, respondentId),
                 "Please provide either stimulusId or respondentId, not both.",
                 info = "too many params not handled properly")

    #getUploadEventsUrl
    eventUrl <- paste0(baseUrl, "/revents/af8c2165-4389-4cc3-8b1e-b2d3a4bd8be1")
    expect_identical(getUploadEventsUrl(study, respondent),
                     paste0(eventUrl, "/respondent/09bd22e6-29b6-4a8a-8cc1-4780a5163e63/data"))

    #getUploadMetricsUrl
    metricsUrl <- paste0(baseUrl, "/rmetrics/af8c2165-4389-4cc3-8b1e-b2d3a4bd8be1")
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
    sceneUrl <- paste0(baseUrl, "/scenes/af8c2165-4389-4cc3-8b1e-b2d3a4bd8be1/",
                       "respondent/09bd22e6-29b6-4a8a-8cc1-4780a5163e63")

    expect_identical(getRespondentScenesUrl(study, respondent), sceneUrl)

    #getRespondentAnnotationsUrl
    annotationsUrl <- paste0(baseUrl, "/annotations/af8c2165-4389-4cc3-8b1e-b2d3a4bd8be1/",
                             "respondent/09bd22e6-29b6-4a8a-8cc1-4780a5163e63")

    expect_identical(getRespondentAnnotationsUrl(study, respondent), annotationsUrl)
})

# Also checking remote study path
study_cloud <- jsonlite::unserializeJSON(readLines("../data/imStudy_cloud.json"))
studyId <- "9dd8bb91-14de-4a1f-9179-0d7700ec7a1d"

# Load sensor
sensors_cloud <- suppressWarnings(jsonlite::unserializeJSON(readLines("../data/imSensorList_cloud.json")))
sensor_cloud <- sensors_cloud[1, ]

test_that("remote - all getUrl function should work as expected", {
    #getStudyBaseUrl
    baseUrl <- "https://my.imotions.com/testcustom"
    expect_identical(getStudyBaseUrl(study_cloud), baseUrl)

    #getStudyS3BaseUrl
    s3BaseUrl <- "https://test"
    expect_identical(getStudyS3BaseUrl(study_cloud), s3BaseUrl)

    #getStudiesUrl
    expect_identical(getStudiesUrl(study_cloud$connection), "https://my.imotions.com/testcustom/studies")

    #getStudyUrl
    studyUrl <- "https://my.imotions.com/testcustom/studies/9dd8bb91-14de-4a1f-9179-0d7700ec7a1d"
    expect_identical(getStudyUrl(study_cloud), studyUrl)

    #getStudyUrlById
    expect_identical(getStudyUrlById(study_cloud$connection, studyId), studyUrl)

    #getSensorsUrl
    sensorsUrl <- paste0(studyUrl, "/respondent/09bd22e6-29b6-4a8a-8cc1-4780a5163e63/samples")
    sensorStimulusUrl <- paste0(studyUrl, "/respondent/09bd22e6-29b6-4a8a-8cc1-4780a5163e63/stimuli/1000/samples")

    expect_identical(getSensorsUrl(study_cloud, respondent), sensorsUrl)
    expect_identical(getSensorsUrl(study_cloud, respondent, stimulus), sensorStimulusUrl)

    #getSensorDataUrl
    sensorDataUrl <- file.path("https:/test", sensor_cloud$dataUrl)
    expect_identical(getSensorDataUrl(study_cloud, sensor_cloud), sensorDataUrl)

    #getUploadSensorsUrl
    uploadCloudUrl <- paste0(baseUrl, "/reportruns/placeholder_reportId/respondents",
                             "/09bd22e6-29b6-4a8a-8cc1-4780a5163e63")

    expect_identical(getUploadSensorsUrl(study_cloud, respondent), uploadCloudUrl)

    #getAOIsUrl
    AOIUrl <- paste0(studyUrl, "/aois/definitions")
    expect_identical(getAOIsUrl(study_cloud), AOIUrl)
    expect_identical(getAOIsUrl(study_cloud, stimulusId), AOIUrl)
    expect_identical(getAOIsUrl(study_cloud, respondentId = respondentId), AOIUrl)

    expect_error(getAOIsUrl(study_cloud, stimulusId, respondentId),
                 "Please provide either stimulusId or respondentId, not both.",
                 info = "too many params not handled properly")

    #getUploadAoiMetricsUrl
    uploadAOIUrl <- paste0(AOIUrl, "/09a234fc-00ad-4257-b7e1-da5754986c9d/segments/1010/stats")
    expect_identical(getUploadAoiMetricsUrl(study_cloud, segment, AOI), uploadAOIUrl)

    #getUploadAoiMetadataUrl
    uploadMetadataUrl <- paste0(baseUrl, "/aoi/sets/fd7891cf-3f51-4fdd-bf48-0ee2f75d53b9/metadata")
    expect_identical(getUploadAoiMetadataUrl(study_cloud), uploadMetadataUrl)

    #getUploadEventsUrl
    #getUploadMetricsUrl
    #getRespondentScenesUrl
    #getRespondentAnnotationsUrl
})
