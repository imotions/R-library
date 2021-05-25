library("imotionsApi");
library("stubthat");

context("getSegments()");

# Load first study
study <- jsonlite::unserializeJSON(readLines("../data/imStudy.json"))

# Load second study
study_one_segment <- jsonlite::unserializeJSON(readLines("../data/imStudy_oneofeach.json"))

test_that("should throw errors if arguments are missing or not from the good class", {
    # in case of missing study
    error <- capture_error(getSegments())
    expect_identical(error$message,
                     "Please specify a study loaded with `imStudy()`",
                     "missing `study` param not handled properly")

    # in case of study that is not an imStudy object
    error <- capture_error(getSegments(study = "whatever"))
    expect_identical(error$message, "`study` argument is not an imStudy object",
                     "study not being an imStudy object should throw an error")
})

test_that("should return all segments from this study", {
    segments <- getSegments(study)

    expect_true(inherits(segments, "imSegmentList"), "`segments` should be an imSegmentList object")
    expect(nrow(segments) == 2, "segments should contain two segments")
    expect_identical(segments$name, c("2 GSR 81-1", "2 GSR 81-2"), "segments names are not matching")
    expect_identical(segments$id, c("1010", "1011"), "segments ids are not matching")

    # check that taking only one segment changes the class of the object
    segment <- segments[1, ]
    expect_true(inherits(segment, "imSegment"), "`segment` should be an imSegment object")

    # check that only taking ids of the list of segments changes the class of the object
    segments <- segments[, c("name", "id")]
    expect_true(all(class(segments) == c("data.table", "data.frame")), "truncated segments should be data.table")
})

test_that("getSegments() in case of only one segment should return an imSegment object", {
    segments <- getSegments(study_one_segment)

    expect_true(inherits(segments, "imSegment"), "`segments` should be an imSegment object")
    expect(nrow(segments) == 1, "segments should only contain a single segment")
    expect_identical(segments$name, c("2 GSR 81-1"), "segment name is not matching")
    expect_identical(segments$id, c("1010"), "segment id is not matching")
})

context("getSegment()")

test_that("should throw errors if arguments are missing or wrong", {
    # in case of missing segmentId
    error <- capture_error(getSegment(study))
    expect_identical(error$message,
                     "Please specify a segmentId. Available segments can be found with `getSegments()`",
                     "missing `segment` param not handled properly")

    # in case of wrong segmentId
    error <- capture_error(getSegment(study, "1000"))
    expect_identical(error$message, "No segment found matching id: 1000", "wrong segmentId not handled properly")
})

test_that("should return one segment from the study", {
    segmentId <- "1011"
    segment <- getSegment(study, segmentId)
    expect_true(inherits(segment, "imSegment"), "`segment` should be an imSegment object")
    expect(nrow(segment) == 1, "should only contain a single segment")
    expect_identical(segment$id, segmentId, "segment id is not matching")

    # print should work as expected
    expect_output(print(segment), "iMotions Segment `2 GSR 81-2` with ID = 1011")
    expect_output(print(segment[name == "Test", ]), "No iMotions Segment found")
})
