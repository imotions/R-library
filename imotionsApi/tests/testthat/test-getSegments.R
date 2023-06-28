# getSegments =========================================================================================================
context("getSegments()")

library(mockery)

# Load first study
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))

# Load second study
study_one_segment <- jsonlite::unserializeJSON(readLines("../data/imStudy_oneofeach.json"))

# Load cloud study
study_cloud <- jsonlite::unserializeJSON(readLines("../data/imStudy_cloud.json"))


test_that("error - arguments are missing or not from the good class", {
    # in case of missing study
    expect_error(getSegments(), "Please specify a study loaded with `imStudy()`", fixed = TRUE,
                 info = "missing `study` param not handled properly")

    # in case of study that is not an imStudy object
    expect_error(getSegments(study = "whatever"), "`study` argument is not an imStudy object",
                 info = "study not being an imStudy object should throw an error")
})

test_that("local return - imSegmentList from a study", {
    segments <- getSegments(study)

    expect_s3_class(segments, "imSegmentList")
    expect_equal(nrow(segments), 2, info = "segments should contain two segments")
    expect_identical(segments$name, c("2 GSR 81-1", "2 GSR 81-2"), "segments names are not matching")
    expect_identical(segments$id, c("1010", "1011"), "segments ids are not matching")

    # check that taking only one segment changes the class of the object
    segment <- segments[1, ]
    expect_s3_class(segment, "imSegment")

    # check that only taking ids of the list of segments changes the class of the object
    segments <- segments[, c("name", "id")]
    expect_s3_class(segments, c("data.table", "data.frame"), exact = TRUE)
})


test_that("local return - imSegment in case of only one segment", {
    segments <- getSegments(study_one_segment)

    expect_s3_class(segments, "imSegment")
    expect_equal(nrow(segments), 1, info = "segments should only contain a single segment")
    expect_identical(segments$name, "2 GSR 81-1", "segment name is not matching")
    expect_identical(segments$id, "1010", "segment id is not matching")
})


test_that("remote return - imSegment in case of only one segment", {
    segments <- getSegments(study_cloud)

    expect_s3_class(segments, "imSegment")
    expect_equal(nrow(segments), 1, info = "segments should contain 1 segment")
    expect_identical(segments$name, "All Respondents", "segments names are not matching")
    expect_identical(segments$id, "d4400a3c-391c-431f-9261-f5ea07334412", "segments ids are not matching")

    # check that only taking ids of the list of segments changes the class of the object
    segments <- segments[, c("name", "id")]
    expect_s3_class(segments, c("data.table", "data.frame"), exact = TRUE)
})

# getSegment ==========================================================================================================
context("getSegment()")

test_that("error - arguments are missing or not from the good class", {
    # in case of missing segmentId
    expect_error(getSegment(study), "Please specify a segmentId. Available segments can be found with `getSegments()`",
                 fixed = TRUE, info = "missing `segment` param not handled properly")

    # in case of wrong segmentId
    expect_error(getSegment(study, "1000"), "No segment found matching id: 1000",
                 info = "wrong segmentId not handled properly")
})

test_that("local return - specific segment from a study", {
    segmentId <- "1011"
    segment <- getSegment(study, segmentId)

    expect_s3_class(segment, "imSegment")
    expect_equal(nrow(segment), 1, info = "should only contain a single segment")
    expect_identical(segment$id, segmentId, "segment id is not matching")

    # print should work as expected
    expect_output(print(segment), "iMotions Segment `2 GSR 81-2` with ID = 1011")
    expect_output(print(segment[name == "Test", ]), "No iMotions Segment found")
})
