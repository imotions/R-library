#' iMotions R API package
#'
#' A client for accessing data from the iMotions Biometrics Research Platform.
#'
#' Use tokens (found on a study's R Analysis page) to load information from the
#' iMotions API
#'
#' @examples
#' myToken <- "xxxxx-xxxx-xxxxx-xxxxx"
#' connection <- imotionsApi::imConnection(myToken)
"_PACKAGE"

# Make sure data.table knows we're using it
.datatable.aware <- TRUE

imotionsApiEnvironment <- new.env()
imotionsApiEnvironment$loadedStudies <- list()


## Connection =========================================================================================================

#' Create a connection with the iMotions API.
#'
#' Tokens can be found on a study's R Analysis page or given directly by the iMotions software
#'
#' @param token The token to be used for authentication.
#' @param baseUrl The iMotions server to connect to, normally you do not need to change the default.
#'
#' @return An imConnection object to be passed to other methods.
#' @import methods
#' @export
#' @examples
#' \dontrun{
#' myToken <- "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"
#' connection <- imotionsApi::imConnection(myToken)
#' }
imConnection <- function(token, baseUrl = "https://my.imotions.com/api") {
    assert(hasArg(token), "You need a token to connect. Please refer to https://my.imotions.com for instructions.")

    # token starting with xxxxxxxx is assumed to be a local "manual" session
    # xxxxxxxx_<base64string> is local session  launched from imotions analysis
    localIM <- imAnalysis <- grepl("^xxxxxxxx(_.+)?$", token)

    if (localIM && missing(baseUrl)) {
        # Assume connecting locally
        envUrl <- Sys.getenv("IMOTIONS_R_SERVER")
        baseUrl <- ifelse(nchar(envUrl) > 0, envUrl, "http://localhost:8086")
    }

    connection <- list(token = token, baseUrl = baseUrl, localIM = localIM, imAnalysis = imAnalysis)
    attr(connection, "class") <- c("imConnection", "list")

    infoMessage <- paste("Connecting to iMotions API...",
                         ifelse(baseUrl != "https://my.imotions.com/api", paste0("(custom server: ", baseUrl, ")"), ""))

    message(infoMessage)
    return(connection)
}






## Study  =============================================================================================================

#' Load an iMotions study by id.
#'
#' Retrieves detailed information about a study including stimuli, respondents, segments, and more.
#'
#' Available studies can be found with \code{\link{listStudies}}.
#'
#' @param connection An imConnection object as returned from \code{\link{imConnection}}.
#' @param studyId The id of the study you would like to retrieve.
#'
#' @return An imStudy object to be passed to other methods.
#' @export
#' @examples
#' \dontrun{
#' connection <- imotionsApi::imConnection("xxxxxxxx")
#' studies <- imotionsApi::listStudies(connection)
#' studyId <- studies$id[1]
#' study <- imotionsApi::imStudy(connection, studyId)
#' }
imStudy <- function(connection, studyId) {
    assert(hasArg(connection), "imotionsApi: no open connection to iMotions. Please connect with `imConnection()`")
    assert(hasArg(studyId), "Please specify a studyId. Available studies can be found with `listStudies()`")
    assertClass(connection, "imConnection", "`connection` argument is not an imConnection object")

    if (studyId %in% names(imotionsApiEnvironment$loadedStudies)) {
        study <- imotionsApiEnvironment$loadedStudies[[studyId]]

        message(paste0("Loading the study ", study$name, " (id = ", studyId, ") from loaded studies..."))
    } else {
        study <- getJSON(connection, getStudyUrlById(connection, studyId),
                         message = paste("Retrieving study with ID:", studyId))

        message(paste0("Loading the study ", study$name, " (id = ", studyId, ") from the server..."))

        study$connection <- connection
        attr(study, "class") <- c("imStudy", "list")
        imotionsApiEnvironment$loadedStudies[[studyId]] <- study
    }

    return(study)
}


#' List available studies.
#'
#' More detailed information about a study can be retrieved using \code{\link{imStudy}}.
#'
#' @param connection An imConnection object as returned from \code{\link{imConnection}}.
#'
#' @return A data frame containing names and ids of studies.
#' @export
#' @examples
#' \dontrun{
#' connection <- imotionsApi::imConnection("xxxxxxxx")
#' studies <- imotionsApi::listStudies(connection)
#' }
listStudies <- function(connection) {
    assert(hasArg(connection), "imotionsApi: no open connection to iMotions. Please connect with `imConnection()`")
    assertClass(connection, "imConnection", "`connection` argument is not an imConnection object")

    studies <- getJSON(connection, getStudiesUrl(connection), message = "Retrieving study list")
    df <- data.frame(name = sapply(studies$name, as.character), id = studies$id, stringsAsFactors = FALSE)

    rownames(df) <- NULL
    return(df)

}


#' List studies that have been loaded in the current session.
#'
#' More detailed information about a study can be retrieved using \code{\link{imStudy}}.
#'
#' @return A list of the studies that have been loaded in the current session.
#' @export
#' @examples
#' \dontrun{
#' connection <- imotionsApi::imConnection("xxxxxxxx")
#' studies <- imotionsApi::listStudies(connection)
#' studyId <- studies$id[1]
#' study <- imotionsApi::imStudy(connection, studyId)
#' listLoadedStudies()
#' }
listLoadedStudies <- function() {
    name <- id <- NULL #removing R Check Warnings
    info <- sapply(imotionsApiEnvironment$loadedStudies, with, c(name, id))

    if (length(info) == 0) {
        return(NULL)
    }

    loadedStudies <- data.frame(name = info[1, ], id = info[2, ], stringsAsFactors = F)
    row.names(loadedStudies) <- NULL
    loadedStudies
}


#' Remove studies that have been loaded in the current session.
#'
#' This function can be used when new changes have been made to already loaded studies.
#'
#' @return "Studies successfully removed from the current session" if success.
#' @export
#' @examples
#' \dontrun{
#' connection <- imotionsApi::imConnection("xxxxxxxx")
#' studies <- imotionsApi::listStudies(connection)
#' studyId <- studies$id[1]
#' study <- imotionsApi::imStudy(connection, studyId)
#' unloadStudies()
#' }
unloadStudies <- function() {
    imotionsApiEnvironment$loadedStudies <- list()
    message("Studies successfully removed from the current session")
}





## Segment ===========================================================================================================

#' Get all segments from a study.
#'
#' Retrieves detailed information about segments in the study.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#'
#' @return An imSegmentList object (data.table) containing all segments from the study.
#' @export
#' @examples
#' \dontrun{
#' connection <- imotionsApi::imConnection("xxxxxxxx")
#' studies <- imotionsApi::listStudies(connection)
#' study <- imotionsApi::imStudy(connection, studies$id[1])
#' segments <- imotionsApi::getSegments(study)
#' }
getSegments <- function(study) {
    assert(hasArg(study), "Please specify a study loaded with `imStudy()`")
    assertClass(study, "imStudy", "`study` argument is not an imStudy object")

    segments <- study$segments
    assert(length(segments) > 0, "No segment found for this study.")
    segments[, c("processingState", "histogramUrl")] <- NULL

    segments <- createImObject(segments, "Segment")
    return(segments)
}


#' Get a specific segment from a study.
#'
#' Available segments can be found with \code{\link{getSegments}}.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param segmentId The id of the segment you would like to retrieve.
#'
#' @return An imSegment object (data.table) containing the segment of interest.
#' @export
#' @examples
#' \dontrun{
#' connection <- imotionsApi::imConnection("xxxxxxxx")
#' studies <- imotionsApi::listStudies(connection)
#' study <- imotionsApi::imStudy(connection, studies$id[1])
#' segments <- imotionsApi::getSegments(study)
#' segment <- imotionsApi::getSegment(study, segments$id[1])
#' }
getSegment <- function(study, segmentId) {
    assert(hasArg(segmentId), "Please specify a segmentId. Available segments can be found with `getSegments()`")

    segments <- getSegments(study)
    segment <- segments[segments$id == segmentId, ]
    assert(nrow(segment) == 1, paste("No segment found matching id:", segmentId))

    return(segment)
}





## Stimulus ===========================================================================================================

#' Get all stimuli from a study.
#'
#' Retrieves detailed information about stimuli in the study.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param relevant A boolean indicating whether only relevant stimuli should be kept, by default non-relevant stimuli
#'                 are discarded.
#'
#' @param respondent Optional - An imRespondent object as returned from \code{\link{getRespondents}}.
#'
#' @return An imStimulusList object (data.table) containing all stimuli from the study.
#' @export
#' @examples
#' \dontrun{
#' connection <- imotionsApi::imConnection("xxxxxxxx")
#' studies <- imotionsApi::listStudies(connection)
#' study <- imotionsApi::imStudy(connection, studies$id[1])
#' respondents <- imotionsApi::getRespondents(study)
#'
#' ## Get all stimuli in the study
#' stimuli <- imotionsApi::getStimuli(study)
#'
#' ## Get all stimuli for a specific respondent
#' stimuli <- imotionsApi::getStimuli(study, respondents[1, ])
#' }
getStimuli <- function(study, respondent = NULL, relevant = TRUE) {
    assert(hasArg(study), "Please specify a study loaded with `imStudy()`")
    assertClass(study, "imStudy", "`study` argument is not an imStudy object")
    assertClass(respondent, "imRespondent", "`respondent` argument is not an imRespondent object")
    stimuli <- study$stimuli

    if (!is.null(respondent)) {
        stimuli <- stimuli[apply(stimuli, 1, function(x) any(x$respondentData$respondent$id %like% respondent$id)), ]
    }

    stimuli$relevant <- !unlist(grepl("non-relevant", stimuli$tags, fixed = T))
    stimuli[, c("respondentData", "tags")] <- NULL

    # Make sure stimuli attached to another stimulus don't have a parent id
    stimuli$parentStimuli[!stimuli$type %like% "_SCENE"] <- NA_character_

    stimuli <- createImObject(stimuli, "Stimulus")

    if (relevant) stimuli <- stimuli[relevant == TRUE, ]
    return(stimuli)
}


#' Get a specific stimulus from a study.
#'
#' Available stimuli can be found with \code{\link{getStimuli}}.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param stimulusId The id of the stimulus you would like to retrieve.
#'
#' @return An imStimulus object (data.table) containing the stimulus of interest.
#' @export
#' @examples
#' \dontrun{
#' connection <- imotionsApi::imConnection("xxxxxxxx")
#' studies <- imotionsApi::listStudies(connection)
#' study <- imotionsApi::imStudy(connection, studies$id[1])
#' stimuli <- imotionsApi::getStimuli(study)
#' stimulus <- imotionsApi::getStimulus(study, stimuli$id[1])
#' }
getStimulus <- function(study, stimulusId) {
    assert(hasArg(stimulusId), "Please specify a stimulusId. Available stimuli can be found with `getStimuli()`")

    stimuli <- getStimuli(study)

    stimulus <- stimuli[stimuli$id == stimulusId, ]
    assert(nrow(stimulus) == 1, paste("No stimulus found matching id:", stimulusId))

    return(stimulus)
}






## AOI ================================================================================================================

#' Get AOIs from a study.
#'
#' Generic getAOIs function that takes as parameter a study object and optionally a respondent/stimulus object.
#' In case no AOIs is defined for the combination, return NULL.
#'
#' Important to note: to speed up the computation of gazes falling in/out AOIs at the respondent/stimulus level, an
#' optional parameter generateInOutFiles can be enabled (both stimulus AND respondent arguments need to be provided).
#' When enabled, a file will be generated for each AOI of this stimulus/respondent combination containing information
#' regarding AOI activation/deactivation and gazes/clicks falling in. Filepaths to these newly generated files
#' will be stored in their corresponding AOI object. If available, these filepaths will then directly be used by the
#' \code{\link{getAOIRespondentData}} function instead of re-generating the files. This is particularly useful in case
#' multiple AOIs are defined for the same stimulus.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param stimulus Optional - An imStimulus object as returned from \code{\link{getStimuli}}.
#' @param respondent Optional - An imRespondent object as returned from \code{\link{getRespondents}}.
#' @param generateInOutFiles A boolean indicating whether the corresponding InOut files should be generated and linked
#'                           to each imAOI object.
#'
#' @importFrom tidyr unnest
#' @return An imAOIList object (data.table) with all AOIs of interest.
#' @export
#' @examples
#' \dontrun{
#' connection <- imotionsApi::imConnection("xxxxxxxx")
#' studies <- imotionsApi::listStudies(connection)
#' study <- imotionsApi::imStudy(connection, studies$id[1])
#' stimuli <- imotionsApi::getStimuli(study)
#' respondents <- imotionsApi::getRespondents(study)
#'
#' ## Get all AOIs in the study
#' AOIs <- imotionsApi::getAOIs(study)
#'
#' ## Get all AOIs defined for a specific stimulus
#' AOIs <- imotionsApi::getAOIs(study, stimulus = stimuli[1, ])
#'
#' ## Get all AOIs defined for a specific respondent
#' AOIs <- imotionsApi::getAOIs(study, respondent = respondents[1, ])
#'
#' ## Get all AOIs defined for a specific stimulus/respondent combination
#' AOIs <- imotionsApi::getAOIs(study, respondent = respondents[1, ], stimulus = stimuli[1, ])
#'
#' ## Get all AOIs defined for a specific stimulus/respondent combination and process their InOut data
#' AOIs <- imotionsApi::getAOIs(study, respondent = respondents[1, ], stimulus = stimuli[1, ],
#'                              generateInOutFiles = T)
#'
#' print(AOIs$fileId) # a field "fileId" should have been added with the path to the InOut file
#' }
getAOIs <- function(study, stimulus = NULL, respondent = NULL, generateInOutFiles = FALSE) {
    assert(hasArg(study), "Please specify a study loaded with `imStudy()`")
    assertClass(study, "imStudy", "`study` argument is not an imStudy object")
    assertClass(stimulus, "imStimulus", "`stimulus` argument is not an imStimulus object")
    assertClass(respondent, "imRespondent", "`respondent` argument is not an imRespondent object")

    if (generateInOutFiles & (is.null(stimulus) | is.null(respondent))) {
        warning("InOut files can only be generated when both respondent and stimulus argument are provided.")
    }

    if (is.null(stimulus) && is.null(respondent)) {
        UseMethod("getAOIs", object = study)
    } else if (!is.null(stimulus) && is.null(respondent)) {
        UseMethod("getAOIs", object = stimulus)
    } else if (!is.null(respondent)) {
        UseMethod("getAOIs", object = respondent)
    }
}


#' Get all AOIs from a study.
#'
#' S3 method default method to get all AOIs for a study.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param ... Further arguments passed from the \code{\link{getAOIs}} method.
#'
#' @return An imAOIList object (data.table) with all AOIs for the study.
#' @export
getAOIs.imStudy <- function(study, ...) {
    endpoint <- paste("study:", study$name)

    # Retrieving AOI definitions for this study
    AOIs <- getJSON(study$connection, getAOIsUrl(study), message = paste("Retrieving AOIs for", endpoint))

    if (all(lengths(AOIs$aois) == 0)) {
        warning(paste("No AOI defined for", endpoint))
        return(NULL)
    }

    # Expanding the data.table to have all AOI information displayed in the same table
    names(AOIs)[1:2] <- c("stimulusId", "stimulusName")
    AOIs <- unnest(AOIs, cols = c("aois"))
    names(AOIs)[5] <- "type"
    AOIs$stimulusId <- as.character(AOIs$stimulusId)
    AOIs$group <- as.character(AOIs$group)
    AOIs$area[AOIs$area == 0] <- NA_real_

    AOIs <- createImObject(AOIs, "AOI")
    return(AOIs)
}


#' Get AOIs for a specific stimulus.
#'
#' S3 method to get all AOIs for a specific stimulus.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param stimulus An imStimulus object as returned from \code{\link{getStimuli}} for which you want to get the AOIs.
#' @param ... Further arguments passed from the \code{\link{getAOIs}} method.
#'
#' @return An imAOIList object (data.table) with all AOIs defined for a specific stimulus.
#' @export
getAOIs.imStimulus <- function(study, stimulus, ...) {
    endpoint <- paste("stimulus:", stimulus$name)

    # Retrieving AOI definitions for this stimulus
    AOIs <- getJSON(study$connection, getAOIsUrl(study, stimulusId = stimulus$id),
                    message = paste("Retrieving AOIs for", endpoint))

    if (all(lengths(AOIs$aois) == 0)) {
        warning(paste("No AOI defined for", endpoint))
        return(NULL)
    }

    # Expanding the data.table to have all AOI information displayed in the same table
    names(AOIs)[1:2] <- c("stimulusId", "stimulusName")
    AOIs <- unnest(AOIs, cols = c("aois"))
    names(AOIs)[5] <- "type"
    AOIs$stimulusId <- as.character(AOIs$stimulusId)
    AOIs$group <- as.character(AOIs$group)
    AOIs$area[AOIs$area == 0] <- NA_real_

    AOIs <- createImObject(AOIs, "AOI")
    return(AOIs)
}


#' Get AOIs for a specific respondent.
#'
#' S3 method to get all AOIs for a specific respondent.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param stimulus Optional - An imStimulus object as returned from  \code{\link{getStimuli}}.
#' @param respondent An imRespondent object as returned from \code{\link{getRespondents}} for which you want to get the
#'                   AOIs.
#'
#' @param generateInOutFiles A boolean indicating whether the corresponding InOut files should be generated and linked
#'                           to the AOI object (both respondent and stimulus must be provided).
#'
#' @return An imAOIList object (data.table) with all AOIs defined for a specific respondent.
#' @export
getAOIs.imRespondent <- function(study, stimulus = NULL, respondent, generateInOutFiles = FALSE) {
    endpoint <- paste("respondent:", respondent$name)

    # Retrieving AOI definitions for this respondent
    AOIs <- getJSON(study$connection, getAOIsUrl(study, respondentId = respondent$id),
                    message = paste("Retrieving AOIs for", endpoint))

    if (all(lengths(AOIs$aois) == 0)) {
        warning(paste("No AOI defined for", endpoint))
        return(NULL)
    }

    # Expanding the data.table to have all AOI information displayed in the same table
    names(AOIs)[1:2] <- c("stimulusId", "stimulusName")
    AOIs <- unnest(AOIs, cols = c("aois"))
    names(AOIs)[5] <- "type"
    AOIs$stimulusId <- as.character(AOIs$stimulusId)
    AOIs$group <- as.character(AOIs$group)
    AOIs$area[AOIs$area == 0] <- NA_real_

    # Retrieve AOI specific to the stimulus if provided
    if (!is.null(stimulus)) {
        AOIs <- AOIs[AOIs$stimulusId %like% stimulus$id, ]

        if (nrow(AOIs) == 0) {
            warning(paste("No AOI defined for", endpoint, "stimulus:", stimulus$name))
            return(NULL)
        }

        if (generateInOutFiles) {
            # Generate InOut files and link them to the AOI object
            AOIDetails <- privateGetAOIDetails(study, stimulus, respondent)
            AOIs <- merge(AOIs, AOIDetails[, c("aoiId", "fileId", "resultId")], by.x = "id", by.y = "aoiId")
            setcolorder(AOIs, c("stimulusId", "stimulusName"))
        }
    }

    AOIs <- createImObject(AOIs, "AOI")
    return(AOIs)
}


#' Get a specific AOI from a study.
#'
#' Available AOIs can be found with \code{\link{getAOIs}}. In case no AOI is found, return NULL.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param AOIId The id of the AOI you would like to retrieve.
#'
#' @return An imAOI object (data.table) containing the AOI of interest.
#' @export
#' @examples
#' \dontrun{
#' connection <- imotionsApi::imConnection("xxxxxxxx")
#' studies <- imotionsApi::listStudies(connection)
#' study <- imotionsApi::imStudy(connection, studies$id[1])
#' AOIs <- imotionsApi::getAOIs(study)
#' AOI <- imotionsApi::getAOI(study, AOIs$id[1])
#' }
getAOI <- function(study, AOIId) {
    assert(hasArg(AOIId), "Please specify an AOIId. Available AOIs can be found with `getAOIs()`")

    AOIs <- getAOIs(study)

    if (is.null(AOIs)) {
        return(NULL)
    }

    AOI <- AOIs[AOIs$id == AOIId, ]

    if (nrow(AOI) == 0) {
        warning(paste("No AOIs found matching id:", AOIId))
        return(NULL)
    }

    return(AOI)
}





## Respondent =========================================================================================================

#' Get respondents from a study.
#'
#' Generic getRespondents function that takes as parameter a study object and optionally a stimulus/AOI/segment
#' object. As AOIs are linked to a stimulus, it is not possible to provide both of them.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param stimulus Optional - An imStimulus object as returned from \code{\link{getStimuli}}.
#' @param AOI Optional - An imAOI object as returned from \code{\link{getAOIs}}.
#' @param segment Optional - An imSegment object as returned from \code{\link{getSegments}}.
#'
#' @return An imRespondentList object (data.table) with all respondents of interest.
#' @export
#' @examples
#' \dontrun{
#' connection <- imotionsApi::imConnection("xxxxxxxx")
#' studies <- imotionsApi::listStudies(connection)
#' study <- imotionsApi::imStudy(connection, studies$id[1])
#' stimuli <- imotionsApi::getStimuli(study)
#' segments <- imotionsApi::getSegments(study)
#' AOIs <- imotionsApi::getAOIs(study)
#'
#' ## Get all respondents in the study
#' respondents <- imotionsApi::getRespondents(study)
#'
#' ## Get all respondents exposed to a specific stimulus
#' respondents <- imotionsApi::getRespondents(study, stimulus = stimuli[1, ])
#'
#' ## Get all respondents in a specific segment
#' respondents <- imotionsApi::getRespondents(study, segment = segments[1, ])
#'
#' ## Get all respondents for whom a specific AOI has been defined
#' respondents <- imotionsApi::getRespondents(study, AOI = AOIs[1, ])
#'
#' ## Get all respondents in a specific segment exposed to a specific stimulus
#' respondents <- imotionsApi::getRespondents(study, stimulus = stimuli[1, ], segment = segments[1, ])
#'
#' ## Get all respondents in a specific segment for whom a specific AOI has been defined
#' respondents <- imotionsApi::getRespondents(study, AOI = AOIs[1, ], segment = segments[1, ])
#' }
getRespondents <- function(study, stimulus = NULL, AOI = NULL, segment = NULL) {
    assert(hasArg(study), "Please specify a study loaded with `imStudy()`")
    assertClass(study, "imStudy", "`study` argument is not an imStudy object")
    assertClass(stimulus, "imStimulus", "`stimulus` argument is not an imStimulus object")
    assertClass(AOI, "imAOI", "`AOI` argument is not an imAOI object")
    assertClass(segment, "imSegment", "`segment` argument is not an imSegment object")

    if (nargs() == 1) {
        UseMethod("getRespondents", object = study)
    } else if (nargs() == 2 && !is.null(stimulus)) {
        UseMethod("getRespondents", object = stimulus)
    } else if (nargs() == 2 && !is.null(AOI)) {
        UseMethod("getRespondents", object = AOI)
    } else if (!is.null(AOI) && !is.null(stimulus)) {
        stop("AOIs are linked to a stimulus, please provide either stimulus or AOI, not both.")
    } else if (nargs() == 2 || nargs() == 3) {
        UseMethod("getRespondents", object = segment)
    }
}


#' Get all respondents from a study.
#'
#' S3 method default method to get all respondents for a study.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param ... Further arguments passed from the \code{\link{getRespondents}} method.
#'
#' @return An imRespondentList object (data.table) with all respondents for the study.
#' @export
getRespondents.imStudy <- function(study, ...) {
    respondents <- study$respondents

    if (!exists("Group", respondents$variables)) {
        respondents$group <- "Default"
    } else {
        respondents$group <- respondents$variables$Group
    }

    respondents <- respondents[, c("label", "id", "group", "age", "gender")]
    names(respondents)[1] <- "name"

    respondents$gender <- formatGender(respondents$gender)

    respondents <- createImObject(respondents, "Respondent")
    return(respondents)
}


#' Get respondents for a specific stimulus.
#'
#' S3 method to get all respondents for a specific stimulus.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param stimulus An imStimulus object as returned from \code{\link{getStimuli}} for which you would like to get
#'                 the respondents.
#' @param ... Further arguments passed from the \code{\link{getRespondents}} method.
#'
#' @return An imRespondentList object (data.table) with all respondents exposed to a specific stimulus.
#' @export
getRespondents.imStimulus <- function(study, stimulus, ...) {
    respondents <- getRespondents.imStudy(study)

    stimulus <- study$stimuli[study$stimuli$id == stimulus$id, ]
    assert(nrow(stimulus) == 1, paste("No stimuli found matching id:", stimulus$id))
    respondentIds <- unlist(lapply(stimulus$respondentData, function(data) data$respondent$id))
    respondents <- respondents[respondents$id %in% respondentIds, ]

    return(respondents)
}


#' Get respondents for a specific AOI.
#'
#' S3 method to get all respondents for a specific AOI.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param ... Further arguments passed from the \code{\link{getRespondents}} method.
#' @param AOI An imAOI object as returned from \code{\link{getAOIs}} for which you would like to get the respondents.
#'
#' @return An imRespondentList object (data.table) with all respondents for whom a specific AOI has been defined.
#' @export
getRespondents.imAOI <- function(study, ..., AOI) {
    # Retrieve AOI specific information
    AOIRespondents <- privateGetAOIDetails(study, AOI)

    respondents <- getRespondents.imStudy(study)
    AOIRespondentsIds <- intersect(respondents$id, AOIRespondents$respId)

    AOIRespondents <- respondents[respondents$id %in% AOIRespondentsIds, ]

    return(AOIRespondents)
}


#' Get respondents for a specific segment.
#'
#' S3 method to get all respondents for a specific segment.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param stimulus Optional - An imStimulus object as returned from  \code{\link{getStimuli}}.
#' @param AOI Optional - An imAOI object as returned from \code{\link{getAOIs}}.
#' @param segment An imSegment object as returned from \code{\link{getSegments}} for which you would like to get
#'                the respondents.
#'
#' @return An imRespondentList object (data.table) listing all respondents in the segment specified.
#' @rdname getRespondents.imSegment
#' @export
getRespondents.imSegment <- function(study, stimulus = NULL, AOI = NULL, segment) {
    if (!is.null(stimulus)) {
        respondents <- getRespondents.imStimulus(study, stimulus)
    } else if (!is.null(AOI)) {
        respondents <- getRespondents.imAOI(study, AOI = AOI)
    } else {
        respondents <- getRespondents.imStudy(study)
    }

    segmentRespondentsIds <- segment$respondents[[1]]$id
    segmentStimulusRespondentsId <- intersect(respondents$id, segmentRespondentsIds)
    segmentRespondents <- respondents[respondents$id %in% segmentStimulusRespondentsId, ]

    return(segmentRespondents)
}


#' Get a specific respondent from a study.
#'
#' Available respondents can be found with \code{\link{getRespondents}}.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param respondentId The id of the respondent you would like to retrieve.
#'
#' @return An imRespondent object (data.table) containing the respondent of interest.
#' @export
#' @examples
#' \dontrun{
#' connection <- imotionsApi::imConnection("xxxxxxxx")
#' studies <- imotionsApi::listStudies(connection)
#' study <- imotionsApi::imStudy(connection, studies$id[1])
#' respondents <- imotionsApi::getRespondents(study)
#' respondent <- imotionsApi::getRespondent(study, respondents$id[1])
#' }
getRespondent <- function(study, respondentId) {
    assert(hasArg(respondentId),
           "Please specify a respondentId. Available respondents can be found with `getRespondents()`")

    respondents <- getRespondents(study)

    respondent <- respondents[respondents$id == respondentId, ]
    assert(nrow(respondent) == 1, paste("No respondent found matching id:", respondentId))

    return(respondent)
}





## Sensor =============================================================================================================

#' Get all sensors available for a given respondent.
#'
#' Retrieves detailed information about sensors available for a given respondent.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param respondent An imRespondent object as returned from \code{\link{getRespondents}}.
#' @param stimulus Optional - an imStimulus object as returned from \code{\link{getStimuli}} to retrieve sensors
#'        specific to this stimulus.
#'
#' @return An imSensorList object (data.table) with all sensors collected for the respondent of interest.
#' @export
#' @examples
#' \dontrun{
#' connection <- imotionsApi::imConnection("xxxxxxxx")
#' studies <- imotionsApi::listStudies(connection)
#' study <- imotionsApi::imStudy(connection, studies$id[1])
#' respondents <- imotionsApi::getRespondents(study)
#' sensors <- imotionsApi::getRespondentSensors(study, respondents[1, ])
#'
#' # Get sensors for a specific stimulus
#' stimuli <- imotionsApi::getStimuli(study)
#' sensors <- imotionsApi::getRespondentSensors(study, respondents[1, ], stimuli[1, ])
#' }
getRespondentSensors <- function(study, respondent, stimulus = NULL) {
    assert(hasArg(study), "Please specify a study loaded with `imStudy()`")
    assert(hasArg(respondent), "Please specify a respondent loaded with `getRespondents()`")
    assertClass(study, "imStudy", "`study` argument is not an imStudy object")
    assertClass(respondent, "imRespondent", "`respondent` argument is not an imRespondent object")
    assertClass(stimulus, "imStimulus", "`stimulus` argument is not an imStimulus object")

    endpoint <- paste("respondent:", respondent$name)
    if (!is.null(stimulus)) {
        endpoint <- paste(endpoint, "stimulus:", stimulus$name)
    }

    sensors <- getJSON(study$connection, getSensorsUrl(study, respondent, stimulus$id),
                       message = paste("Retrieving sensors for", endpoint))

    assert(length(sensors) > 0, paste("No sensors found for", endpoint))
    sensors$signalsMetaData <- NULL
    sensors$respondent <- list(respondent)
    sensors <- reorderSensorColumns(sensors)

    sensors <- createImObject(sensors, "Sensor")
    return(sensors)
}





## Intervals (Stimulus, Scene, Annotation) ============================================================================

#' Get the list of time intervals (imIntervalList) for a given respondent.
#'
#' This imIntervalList is composed of stimuli/scenes/annotations intervals.
#' Note that AOIs intervals can be retrieved using \code{\link{getAOIRespondentData}}.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param respondent An imRespondent object as returned from \code{\link{getRespondents}}.
#' @param type The type of intervals to retrieve (can be set to Stimulus, Scene and/or Annotation).
#'
#' @return An imIntervalList object (data.table) composed of the start, end, duration, parent stimulus, id and name of
#'         each stimulus/scene/annotation. Annotations comments are also included.
#' @export
#' @examples
#' \dontrun{
#' connection <- imotionsApi::imConnection("xxxxxxxx")
#' studies <- imotionsApi::listStudies(connection)
#' study <- imotionsApi::imStudy(connection, studies$id[1])
#' respondents <- imotionsApi::getRespondents(study)
#' intervals <- imotionsApi::getRespondentIntervals(study, respondents[1, ])
#'
#' # Get only the stimuli intervals
#' intervals <- imotionsApi::getRespondentIntervals(study, respondents[1, ], type = "Stimulus")
#' }
getRespondentIntervals <- function(study, respondent, type = c("Stimulus", "Scene", "Annotation")) {
    assert(hasArg(study), "Please specify a study loaded with `imStudy()`")
    assert(hasArg(respondent), "Please specify a respondent loaded with `getRespondents()`")
    assertClass(respondent, "imRespondent", "`respondent` argument is not an imRespondent object")
    stimulusIntervals <- sceneIntervals <- annotationIntervals <- NULL

    if (any(!grepl("Stimulus|Scene|Annotation", type))) {
        stop("`type` argument can only be set to Stimulus, Scene and/or Annotation")
    }

    if ("Stimulus" %in% type) stimulusIntervals <- privateGetIntervalsForStimuli(study, respondent)
    if ("Scene" %in% type) sceneIntervals <- privateGetIntervalsForScenes(study, respondent)
    if ("Annotation" %in% type) annotationIntervals <- privateGetIntervalsForAnnotations(study, respondent)

    intervals <- rbind(stimulusIntervals, sceneIntervals, annotationIntervals)

    if (length(intervals) == 0) {
        return(NULL)
    }

    intervals <- cbind(intervals, "respondent" = list(respondent))
    intervals <- createImObject(intervals, "Interval")

    return(intervals)
}


#' Get time intervals for stimuli.
#'
#' Return the start, end, duration, id and name of each stimulus.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param respondent An imRespondent object as returned from \code{\link{getRespondents}}.
#'
#' @return A data.table composed of the start, end, duration, id and name of each stimulus.
#' @keywords internal
privateGetIntervalsForStimuli <- function(study, respondent) {
    sensors <- getRespondentSensors(study, respondent)
    sensor <- sensors[which(sensors$name == "SlideEvents"), ]

    if (nrow(sensor) != 1) {
        warning("No stimulus events found for this respondent.")
        return(NULL)
    }

    slideEvents <- getSensorData(study, sensor)
    stimuli <- getStimuli(study)

    start <- slideEvents[slideEvents$SlideEvent == "StartMedia", ]$Timestamp
    end <- slideEvents[slideEvents$SlideEvent == "EndMedia", ]$Timestamp
    duration <- end - start
    name <- slideEvents[slideEvents$SlideEvent == "StartMedia", ]$SourceStimuliName

    intervals <- data.table::data.table("fragments" = data.frame(start, end, duration), "name" = name,
                                        "type" = "Stimulus", "parentStim" = NA_character_, "text" = NA_character_)

    intervals$id <- stimuli[intervals, "id", on = "name"]$id

    return(intervals)
}


#' Get time intervals for scenes.
#'
#' Return the start, end, duration, parent stimulus, id and name of each scene.
#' Scene with multiple fragments will be return as multiple intervals (start, end, duration) with same id and name.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param respondent An imRespondent object as returned from \code{\link{getRespondents}}.
#'
#' @return A data.table composed of the start, end, duration, parent stimulus, id and name of each scene.
#' @keywords internal
privateGetIntervalsForScenes <- function(study, respondent) {
    stimuli <- getStimuli(study)
    scenes <- getJSON(study$connection, getRespondentScenesUrl(study, respondent),
                      message = paste("Retrieving scenes for respondent", respondent$name))

    if (length(scenes) == 0) {
        warning("No scenes events found for this respondent.")
        return(NULL)
    }

    start <- scenes$fragmentStart
    end <- scenes$fragmentEnd
    duration <- end - start
    name <- stimuli[match(scenes$sceneId, stimuli$id), ]$name
    parentStim <- stimuli[match(scenes$sceneId, stimuli$id), ]$parentStimuli
    intervals <- data.table::data.table("fragments" = data.frame(start, end, duration), "name" = name, "type" = "Scene",
                                        "parentStim" = parentStim, "text" = NA_character_, "id" = scenes$sceneId)

    return(intervals)
}


#' Get time intervals for annotations.
#'
#' Return the start, end, duration, parent stimulus, id, name and comment of each annotation.
#' Annotation with multiple fragments will be return as multiple intervals (start, end, duration) with same id and name.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param respondent An imRespondent object as returned from \code{\link{getRespondents}}.
#'
#' @return A data.table composed of the start, end, duration, parent stimulus, id, name and comment of each annotation.
#' @keywords internal
privateGetIntervalsForAnnotations <- function(study, respondent) {
    annotations <- getJSON(study$connection, getRespondentAnnotationsUrl(study, respondent),
                           message = paste("Retrieving annotations for respondent", respondent$name))

    if (length(annotations) == 0) {
        warning("No annotations events found for this respondent.")
        return(NULL)
    }

    # Expanding the data.table to have all annotations information displayed in the same table
    annotations$id <- NULL
    annotations <- unnest(annotations, cols = c("fragments"))

    start <- annotations$rangeStart
    end <- annotations$rangeEnd
    duration <- end - start
    name <- annotations$name
    parentStim <- annotations$stimuli
    text <- annotations$text
    intervals <- data.table::data.table("fragments" = data.frame(start, end, duration), "name" = name,
                                        "type" = "Annotation", "parentStim" = parentStim, "text" = text)

    # Annotation have no id so we only do a sequence by parent stimulus and name of annotation
    intervals[, "id" := as.character(.GRP), by = c("parentStim", "name")]

    return(intervals)
}



#' Truncate signals data based on given intervals.
#'
#' Any interval combination can be asked. Timestamps falling between an interval start/end will be kept, others will be
#' discarded. If dropIntervals is set to TRUE, Timestamps falling between an interval start/end will be discarded.
#'
#' @param signals An imSignals object as returned by \code{\link{getSensorData}} or a data.table including a
#'                "Timestamp" column that needs to be cut.
#'
#' @param intervals An imInterval or imIntervalList object with start/end of data to subset as given by
#'                  \code{\link{getRespondentIntervals}} or \code{\link{getAOIRespondentData}}.
#'
#' @param dropIntervals A boolean indicating whether Timestamps falling between an interval start/end should be
#'                      discarded, by default there are kept and other Timestamps are removed.
#'
#' @return A truncated imSignals object.
#' @export
#' @examples
#' \dontrun{
#' connection <- imotionsApi::imConnection("xxxxxxxx")
#' studies <- imotionsApi::listStudies(connection)
#' study <- imotionsApi::imStudy(connection, studies$id[1])
#' respondents <- imotionsApi::getRespondents(study)
#' sensors <- imotionsApi::getRespondentSensors(study, respondents[1, ])
#' signals <- imotionsApi::getSensorData(study, sensors[3, ])
#' intervals <- imotionsApi::getRespondentIntervals(study, respondents[1, ])
#'
#' # get the 3 first intervals
#' dataSubset <- imotionsApi::truncateSignalsByIntervals(signals, intervals[1:3, ])
#'
#' # remove the second interval
#' dataSubset <- imotionsApi::truncateSignalsByIntervals(signals, intervals[2, ], dropIntervals = TRUE)
#' }
truncateSignalsByIntervals <- function(signals, intervals, dropIntervals = FALSE) {
    assert(hasArg(signals), paste("Please specify an imSignals object as returned by `getSensorData()`",
                                  "or a data.table including a `Timestamp` column"))
    assert(hasArg(intervals), paste("Please specify intervals loaded with `getRespondentIntervals()`",
                                    "or `getAOIRespondentData()`"))

    assertClass(intervals, c("imInterval", "imIntervalList"),
                "`intervals` argument is not an imInterval or imIntervalList object")

    # Verify that signals is either a data.table of the good format or an imSignals object
    signals <- checkDataFormat(signals)

    # Checking that signals is an imSignals and not an imMetrics object
    assertClass(signals, "imSignals", "`signals` argument is an imMetrics object, not an imSignals object")

    idxInIntervals <- which(signals$Timestamp %inrange% intervals[, c("fragments.start", "fragments.end")])
    allIdx <- row.names(signals)

    if (dropIntervals) {
        signals <- signals[!idxInIntervals, ]

        # Keeping original signals row number (can be used to detect gap in data later)
        row.names(signals) <- which(!allIdx %in% idxInIntervals)
    } else {
        signals <- signals[idxInIntervals, ]

        # Keeping original signals row number (can be used to detect gap in data later)
        row.names(signals) <- idxInIntervals
    }

    return(signals)
}



#' Convert recording's timestamps (relative to data recording start) into stimulus/scene/AOI timestamps (relative to the
#' interval first fragment start). Fragments are concatenated to give new array of timestamps in range [0,
#' concatenated duration of stimulus/scene/AOI].
#'
#' Timestamps falling between an interval start/end will be kept, others will be discarded.
#'
#' @param recordingTs An array of recording's timestamps (relative to data recording start). Scalar, imSignals object as
#'                    returned by \code{\link{getSensorData}} or data.table with a column Timestamp are also accepted.
#'
#'
#' @param intervals An imInterval or imIntervalList object with start/end of a stimulus/scene/AOI as given by
#'                  \code{\link{getRespondentIntervals}} or \code{\link{getAOIRespondentData}}.
#'
#' @return A new array/scalar/data.table with timestamps in range [0, concatenated duration of stimulus/scene/AOI].
#' @export
#' @examples
#' \dontrun{
#' connection <- imotionsApi::imConnection("xxxxxxxx")
#' studies <- imotionsApi::listStudies(connection)
#' study <- imotionsApi::imStudy(connection, studies$id[1])
#' respondents <- imotionsApi::getRespondents(study)
#' sensors <- imotionsApi::getRespondentSensors(study, respondents[1, ])
#' signals <- imotionsApi::getSensorData(study, sensors[3, ])
#' intervals <- imotionsApi::getRespondentIntervals(study, respondents[1, ])
#'
#' # get a new signal with timestamps in range [0, concatenated duration of stimulus/scene/AOI].
#' signals <- imotionsApi::convertRecordingTsToIntervals(signals, intervals[1, ])
#' }
convertRecordingTsToIntervals <- function(recordingTs, intervals) {
    assert(hasArg(recordingTs), "Please specify an array, scalar or data.table with timestamps to modify")
    assert(hasArg(intervals), paste("Please specify intervals loaded with `getRespondentIntervals()`",
                                    "or `getAOIRespondentData()`"))

    assertClass(intervals, c("imInterval", "imIntervalList"),
                "`intervals` argument is not an imInterval or imIntervalList object")

    # Assert that intervals provided have the same id
    assert(length(unique(intervals$id)) == 1, paste("`intervals` argument contain more than one stimulus/scene/aoi"))

    # In case a data.table has been provided, looking for the Timestamp column and truncate it.
    if (inherits(recordingTs, "data.frame")) {
        recordingTs <- truncateSignalsByIntervals(recordingTs, intervals)
        timestamps <- recordingTs$Timestamp
    } else {
        # Assert that recordingTs provided is of type numeric
        assert(is.numeric(recordingTs), "`recordingTs` array or scalar must be of numeric type")
        timestamps <- recordingTs[which(recordingTs %inrange% intervals[, c("fragments.start", "fragments.end")])]
    }

    # Converting timestamps to intervals fragments ranges
    invisible(lapply(seq(nrow(intervals)), function(x) {
        isIn <- timestamps %inrange% intervals[x, c("fragments.start", "fragments.end")]
        timestamps[isIn] <<- timestamps[isIn] - intervals[x, ]$fragments.start +
            sum(intervals[0:(x - 1), ]$fragments.duration)
    }))

    if (inherits(recordingTs, "data.frame")) {
        recordingTs$Timestamp <- timestamps
    } else {
        recordingTs <- timestamps
    }

    return(recordingTs)
}


## Downloading Data ===================================================================================================

#' Download data corresponding to a specific sensor (signals/metrics).
#'
#' Available sensors in your study can be listed using the \code{\link{getRespondentSensors}}.
#'
#' Signals always have a "Timestamp" column and are unique to a given respondent and a given sensor source.
#' Metrics are stored as a special sensor, also specific to a given respondent.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param sensor An imSensor object as returned from \code{\link{getRespondentSensors}}.
#' @param signalsName Optional - A vector of specific signals name you would like to return.
#' @param intervals Optional - An imInterval or imIntervalList object with start/end of data to subset as given by
#'                  \code{\link{getRespondentIntervals}}.
#'
#' @return An imData object (data.table) containing the signals (imSignals) or metrics (imMetrics) for the sensor of
#'         interest.
#' @export
#' @examples
#' \dontrun{
#' connection <- imotionsApi::imConnection("xxxxxxxx")
#' studies <- imotionsApi::listStudies(connection)
#' study <- imotionsApi::imStudy(connection, studies$id[1])
#' respondents <- imotionsApi::getRespondents(study)
#' sensors <- imotionsApi::getRespondentSensors(study, respondents[1, ])
#' data <- imotionsApi::getSensorData(study, sensors[1, ])
#' }
getSensorData <- function(study, sensor, signalsName = NULL, intervals = NULL) {
    assert(hasArg(study), "Please specify a study loaded with `imStudy()`")
    assert(hasArg(sensor), "Please specify a sensor loaded with `getRespondentSensors()`")
    assertClass(study, "imStudy", "`study` argument is not an imStudy object")
    assertClass(sensor, "imSensor", "`sensor` argument is not an imSensor object")

    data <- privateDownloadData(study, sensor, signalsName = signalsName)

    # Detect the type of the data downloaded (imSignals, imMetrics)
    data <- checkDataFormat(data)

    if (inherits(data, "imSignals")) {
        # in case an imInterval or imIntervalList object has been given - filter the data accordingly
        if (!is.null(intervals)) {
            assert(intervals[1, ]$respondent[[1]]$id == sensor$respondent[[1]]$id,
                   "sensor and intervals must correspond to the same respondent")

            data <- truncateSignalsByIntervals(data, intervals)
        }
    }

    return(data)
}


#' Get the inOutGaze information, inOutMouseClick information and AOI's intervals for a specific AOI/respondent
#' combination. Note that imAOI object, by definition, are linked to a specific stimulus.
#'
#' The inOutGaze data.table has a IsGazeInAOI column that is TRUE when a gaze was recorded inside the AOI and FALSE if
#' outside (timestamps correspond to the actual gazepoint Timestamp). To reduce the size of the file created,
#' only timestamps where a change of value occur are given. If the AOI was never active, the table is empty.
#'
#' The inOutMouseClick data.table has a IsMouseInAOI column that is TRUE when a click was recorded inside the AOI and
#' FALSE if outside (timestamps correspond to the actual Timestamp of each click). If no click was recorded or if the
#' AOI was never active, the table is empty.
#'
#' @param study An imStudy object as returned from imStudy()
#' @param AOI An imAOI object as returned from \code{\link{getAOIs}}.
#' @param respondent An imRespondent object as returned from \code{\link{getRespondents}}.
#'
#' @importFrom dplyr mutate_at %>%
#' @return A list with inOutGaze/inOutMouseClick information for the specific AOI/respondent combination and an
#'         imIntervalList object (data.table) composed of the start, end, duration, id and name of this AOI.
#' @export
#' @examples
#' \dontrun{
#' connection <- imotionsApi::imConnection("xxxxxxxx")
#' studies <- imotionsApi::listStudies(connection)
#' study <- imotionsApi::imStudy(connection, studies$id[1])
#' AOIs <- imotionsApi::getAOIs(study)
#' respondents <- imotionsApi::getRespondents(study, AOI = AOIs[1, ])
#' AOIData <- imotionsApi::getAOIRespondentData(study, AOIs[1, ], respondents[1, ])
#'
#' # Retrieving list items
#' inOutData <- AOIData$inOutData
#' intervals <- AOIData$intervals
#' }
getAOIRespondentData <- function(study, AOI, respondent) {
    IsActiveAOI <- IsGazeInAOI <- NULL # set local variable to remove warnings in `devtools::check()`

    assert(hasArg(study), "Please specify a study loaded with `imStudy()`")
    assert(hasArg(AOI), "Please specify an AOI loaded with `getAOIs()`")
    assert(hasArg(respondent), "Please specify a respondent loaded with `getRespondents()`")

    assertClass(study, "imStudy", "`study` argument is not an imStudy object")
    assertClass(AOI, "imAOI", "`AOI` argument is not an imAOI object")
    assertClass(respondent, "imRespondent", "`respondent` argument is not an imRespondent object")

    AOIDetails <- privateGetAOIDetails(study, AOI, respondent)

    if (length(AOIDetails) == 0) {
        warning(paste("AOI", AOI$name, "was not found for respondent", respondent$name))
        return(NULL)
    }

    if (file.info(AOIDetails$fileId)$size == 0) {
        # Case where the AOI is never active
        intervals <- data.table(fragments.start = NA_real_, fragments.end = NA_real_)
        inOutGaze <- data.table(matrix(data = NA_integer_, ncol = 2, nrow = 0))
        names(inOutGaze) <- c("Timestamp", "IsGazeInAOI")
        inOutMouseClick <- data.table(matrix(data = NA_integer_, ncol = 2, nrow = 0))
        names(inOutMouseClick) <- c("Timestamp", "IsMouseInAOI")
    } else {
        data <- read_parquet(AOIDetails$fileId)
        data <- data %>% mutate_at(c("IsActiveAOI", "IsGazeInAOI", "IsMouseInAOI", "IsMouseDown"), as.logical)
        setDT(data)
        data$id <- AOI$id

        # Get AOI time intervals based on the isActive data for this respondent/AOI combination
        activityChange <- data[, c(IsActiveAOI = unique(IsActiveAOI), .SD[1]), by = rleid(IsActiveAOI)]
        intervals <- data.table(fragments.start = activityChange[(activityChange$IsActiveAOI), ]$Timestamp,
                                fragments.end = activityChange[(!activityChange$IsActiveAOI), ]$Timestamp)

        # Get gazes events
        inOutGaze <- data[, c(IsGazeInAOI = unique(IsGazeInAOI), .SD[1]), by = rleid(IsGazeInAOI)]
        inOutGaze <- inOutGaze[, c("Timestamp", "IsGazeInAOI")]

        # Get clicks events
        inOutMouseClick <- data[(data$IsMouseDown), c("Timestamp", "IsMouseInAOI")]
    }

    intervals <- intervals[, `:=`(fragments.duration = intervals$fragments.end - intervals$fragments.start,
                                  name = AOI$name, type = "AOI", parentStim = AOI$stimulusId, id = AOI$id,
                                  text = NA_character_, respondent = list(respondent))]

    intervals[is.na(intervals$fragments.duration), ]$fragments.duration <- 0
    intervals <- createImObject(intervals, "Interval")
    return(list(inOutGaze = inOutGaze, inOutMouseClick = inOutMouseClick, intervals = intervals))
}

#' Get the metrics for a specific AOI/respondent combination.
#'
#' @param study An imStudy object as returned from imStudy()
#' @param AOI An imAOI object as returned from \code{\link{getAOIs}}.
#' @param respondent An imRespondent object as returned from \code{\link{getRespondents}}.
#'
#' @return A data.table of one row (imMetrics object) with metrics for the AOI /respondent combination of interest.
#' @export
#' @examples
#' \dontrun{
#' connection <- imotionsApi::imConnection("xxxxxxxx")
#' studies <- imotionsApi::listStudies(connection)
#' study <- imotionsApi::imStudy(connection, studies$id[1])
#' AOIs <- imotionsApi::getAOIs(study)
#' respondents <- imotionsApi::getRespondents(study, AOI = AOIs[1, ])
#' AOImetrics <- imotionsApi::getAOIRespondentMetrics(study, AOIs[1, ], respondents[1, ])
#' }
getAOIRespondentMetrics <- function(study, AOI, respondent) {
    assert(hasArg(study), "Please specify a study loaded with `imStudy()`")
    assert(hasArg(AOI), "Please specify an AOI loaded with `getAOIs()`")
    assert(hasArg(respondent), "Please specify a respondent loaded with `getRespondents()`")

    assertClass(study, "imStudy", "`study` argument is not an imStudy object")
    assertClass(AOI, "imAOI", "`AOI` argument is not an imAOI object")
    assertClass(respondent, "imRespondent", "`respondent` argument is not an imRespondent object")

    AOIDetails <- privateGetAOIDetails(study, AOI, respondent)

    if (length(AOIDetails) == 0) {
        warning(paste("AOI", AOI$name, "was not found for respondent", respondent$name))
        return(NULL)
    }

    if (is.na(AOIDetails$resultId)) {
        warning(paste0("No metrics found for AOI: ", AOI$name, ", respondent: ", respondent$name))
        return(NULL)
    }

    metrics <- setDT(fread(AOIDetails$resultId))
    metrics <- checkDataFormat(metrics)
    return(metrics)
}


#' Return metrics / signals (with modified Timestamp if needed) for the sensor of interest.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param sensor An imSensor object as returned from \code{\link{getRespondentSensors}}.
#' @param signalsName Optional - A vector of specific signals name you would like to return.
#'
#' @importFrom arrow read_parquet
#' @importFrom tidyselect any_of
#' @return A data.table with all signals (or specified signals) from the sensor of interest.
#' @keywords internal
privateDownloadData <- function(study, sensor, signalsName = NULL) {
    if (study$connection$localIM) {
        # Accessing filesPath for local imotions files corresponding to this sensor
        dataUrl <- getSensorDataUrl(study, sensor)
        filesPath <- getJSON(study$connection, dataUrl, message = paste("Retrieving data for sensor:", sensor$name))

        # Downloading data of interest
        if (!is.null(signalsName)) {
            # ensure timestamps will always be retrieved
            signalsName <- c("Timestamp", signalsName)
            data <- read_parquet(filesPath$binFile, col_select = any_of(signalsName))
        } else {
            data <- read_parquet(filesPath$binFile)
        }

        # if corrected timestamps are available - correct the original timestamps
        if (nchar(filesPath$timestampBinFile) > 0) {
            tmp <- read_parquet(filesPath$timestampBinFile)
            data$Timestamp <- tmp$Timestamp
        }

        attr(data, "fileDependency") <- filesPath$binFile
    }

    setDT(data)
    return(data)
}


#' Generic privateGetAOIDetails function that takes as parameter a study object, an imObject(imAOI or imStimulus) object
#' and, optionally an imRespondent object.
#'
#' Return details for a specific AOI or all AOIs defined for a stimulus (i.e. respondents for whom it was defined and
#' file paths to their corresponding IsGazeInAOI data).
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param imObject An imAOI or imStimulus object as returned from \code{\link{getAOIs}} or \code{\link{getStimuli}}.
#' @param respondent Optional - An imRespondent object as returned from \code{\link{getRespondents}}.
#'
#' @return A data.frame with details about the AOI of interest (i.e. respondents for whom it was defined and file paths
#'         to their IsGazeInAOI data).
#'
#' @keywords internal
privateGetAOIDetails <- function(study, imObject, respondent = NULL) {
    if (study$connection$localIM) {
        UseMethod("privateGetAOIDetails", object = imObject)
    }
}


#' S3 method default method to get AOIs' details for a specific AOI.
#'
#' Return details for a specific AOI (i.e. respondents for whom it was defined and file paths to their corresponding
#' IsGazeInAOI data).
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param imObject An imAOI object as returned from \code{\link{getAOIs}}.
#' @param respondent Optional - An imRespondent object as returned from \code{\link{getRespondents}}.
#'
#' @return A data.frame with details about the AOI of interest (i.e. respondents for whom it was defined and file paths
#'         to their IsGazeInAOI data).
#'
#' @keywords internal
privateGetAOIDetails.imAOI <- function(study, imObject, respondent = NULL) {
    if (exists("fileId", imObject)) {
        # in case the InOut file was already generated, we just return its path
        return(imObject[, c("fileId", "resultId")])
    }

    endpoint <- paste("AOI:", imObject$name)
    if (is.null(respondent)) {
        dataUrl <- getAOIDetailsUrl(study, imObject)
    } else {
        dataUrl <- getAOIDetailsUrl(study, imObject, respondent$id)
        endpoint <- paste(endpoint, "respondent:", respondent$name)
    }

    AOIDetails <- getJSON(study$connection, dataUrl, message = paste("Retrieving details for", endpoint))
    return(AOIDetails)
}

#' S3 method default method to get AOIs' details for a specific stimulus.
#'
#' Return details for all AOIs defined for a specific stimulus (i.e. respondents for whom they were defined and file
#' paths to their corresponding IsGazeInAOI data).
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param imObject An imStimulus object as returned from \code{\link{getStimuli}}.
#' @param respondent Optional - An imRespondent object as returned from \code{\link{getRespondents}}.
#'
#' @return A data.frame with details about AOIs of interest (i.e. respondents for whom they were defined and file paths
#'         to their IsGazeInAOI data).
#'
#' @keywords internal
privateGetAOIDetails.imStimulus <- function(study, imObject, respondent = NULL) {
    endpoint <- paste("all AOIs in stimulus:", imObject$name)
    if (is.null(respondent)) {
        dataUrl <- getAOIDetailsForStimulusUrl(study, imObject$id)
    } else {
        dataUrl <- getAOIDetailsForStimulusUrl(study, imObject$id, respondent$id)
        endpoint <- paste(endpoint, "respondent:", respondent$name)
    }

    AOIDetails <- getJSON(study$connection, dataUrl, message = paste("Retrieving details for", endpoint))
    return(AOIDetails)
}




## Uploading Data =====================================================================================================

#' Create a new sensor for a specific respondent in a study.
#'
#' Either signals (with a Timestamp column) or metrics (single values, with no Timestamp) can be uploaded.
#' After processing, the sensor can then be viewed and exported locally through the iMotions Desktop.
#'
#' Params required field are "iMotionsVersion" and "flowName" (flow name will be use as "instance" of the new sensor)
#'
#' @param params The list of parameters provided to the script - specific parameters value will be stored as metadata.
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param data A data.table containing the signals/metrics to upload (imData object are also accepted).
#' @param target The target respondent for the sensor (an imRespondent object as returned from
#'               \code{\link{getRespondents}}).
#'
#' @param sensorName The name of the new sensor you would like to create.
#' @param scriptName The name of the script used to produce these signals/metrics.
#' @param metadata Optional - a data.table with metadata information. Column names will be converted to metadata headers
#'                 and there must be a row corresponding to each data column.
#'
#' @param stimulus Optional - an imStimulus object as returned from \code{\link{getStimuli}} to upload data specific to
#'                 this stimulus.
#'
#' @export
#' @examples
#' \dontrun{
#' connection <- imotionsApi::imConnection("xxxxxxxx")
#' studies <- imotionsApi::listStudies(connection)
#' study <- imotionsApi::imStudy(connection, studies$id[1])
#' respondents <- imotionsApi::getRespondents(study)
#' data <- data.frame("Timestamp" = seq(1:100), "Thresholded value" = rep(0, 100))
#' params <- list("iMotionsVersion" = 8, "flowName" = "Test")
#' uploadSensorData(params, study, data, respondents[1, ], sensorName = "New sensor",
#'                  scriptName = "Example Script")
#'
#' # Uploading data to a specific stimulus
#' stimuli <- imotionsApi::getStimuli(study)
#' uploadSensorData(params, study, data, respondents[1, ], sensorName = "New sensor",
#'                  scriptName = "Example Script", stimulus = stimuli[1, ])
#'
#' # Adding some metadata to the data
#' metadata <- data.table("Units" = c("ms", ""), "Show" = c("FALSE", "TRUE"))
#' uploadSensorData(params, study, data, respondents[1, ], sensorName = "New sensor",
#'                  scriptName = "Example Script", metadata)
#' }
uploadSensorData <- function(params, study, data, target, sensorName, scriptName, metadata = NULL, stimulus = NULL) {
    assert(hasArg(params), "Please specify parameters used for your script")
    assert(hasArg(study), "Please specify a study loaded with `imStudy()`")
    assert(hasArg(data), "Please specify a data.table with signals/metrics to upload")
    assert(hasArg(target), "Please specify a target respondent loaded with `getRespondents()`")
    assert(hasArg(sensorName), "Please specify a name for the new sensor to upload")
    assert(hasArg(scriptName), "Please specify the name of the script used to produce this data")

    assertClass(study, "imStudy", "`study` argument is not an imStudy object")
    assertClass(target, "imRespondent", "`target` argument is not an imRespondent object")
    assertClass(stimulus, "imStimulus", "`stimulus` argument is not an imStimulus object")


    assert(exists("iMotionsVersion", params), "Required `iMotionsVersion` field in params")
    assert(exists("flowName", params), "Required `flowName` field in params")

    # Verify that data is a data.table of the good format
    data <- checkDataFormat(data)
    assertUploadFormat(data)

    if (inherits(data, "imSignals")) {
        privateUploadSignals(params, study, data, target, sensorName, scriptName, metadata, stimulus)
    }

}


#' Upload metrics for a specific respondent and AOI in a study.
#'
#' @param study An imStudy object as returned from imStudy()
#' @param AOI An imAOI object as returned from \code{\link{getAOIs}}.
#' @param respondent An imRespondent object as returned from \code{\link{getRespondents}}.
#' @param metrics A data.table containing the metrics to upload.
#'
#' @export
#' @examples
#' \dontrun{
#' connection <- imotionsApi::imConnection("xxxxxxxx")
#' studies <- imotionsApi::listStudies(connection)
#' study <- imotionsApi::imStudy(connection, studies$id[1])
#' AOIs <- imotionsApi::getAOIs(study)
#' respondents <- imotionsApi::getRespondents(study, AOI = AOIs[1, ])
#' metrics <- data.frame("metric1" = 2, "metric2" = 234, "metric3" = 1234)
#' uploadAOIRespondentMetrics(study, AOIs[1, ], respondents[1, ], metrics)
#' }
uploadAOIRespondentMetrics <- function(study, AOI, respondent, metrics) {
    assert(hasArg(study), "Please specify a study loaded with `imStudy()`")
    assert(hasArg(AOI), "Please specify an AOI loaded with `getAOIs()`")
    assert(hasArg(respondent), "Please specify a respondent loaded with `getRespondents()`")
    assert(hasArg(metrics), "Please specify a data.table with metrics to upload")

    assertClass(study, "imStudy", "`study` argument is not an imStudy object")
    assertClass(AOI, "imAOI", "`AOI` argument is not an imAOI object")
    assertClass(respondent, "imRespondent", "`respondent` argument is not an imRespondent object")

    # Verify that data is a data.table of the good format
    metrics <- checkDataFormat(metrics)
    assertUploadFormat(metrics)

    AOIDetails <- privateGetAOIDetails(study, AOI, respondent)

    if (length(AOIDetails) == 0) {
        warning(paste("AOI", AOI$name, "was not found for respondent", respondent$name))
        return(NULL)
    }

    if (inherits(metrics, "imMetrics")) {
        dataFileName <- paste0(tools::file_path_sans_ext(AOIDetails$fileId), "metrics.csv")
        fwrite(x = metrics, file = dataFileName, col.names = TRUE)
    } else {
        warning("Metrics should be a data.frame/data.table composed of only one row")
    }
}


#' Upload signals to a given respondent/segment.
#'
#' @inheritParams uploadSensorData
#'
#' @keywords internal
privateUploadSignals <- function(params, study, data, target, sensorName, scriptName, metadata = NULL,
                                 stimulus = NULL) {

    # Create a temporary file with the data/metadata that needs to be uploaded
    tempFileName <- privateSaveSignalsToFile(params, data, sensorName, scriptName, metadata)

    # Prepare the http request and move the file to the right location
    postData <- toJSON(list(flowName = params$flowName, sampleName = sensorName, fileName = tempFileName))
    uploadUrl <- getUploadSensorsUrl(study, target, stimulus$id)

    endpoint <- paste("target:", target$name)
    if (!is.null(stimulus)) {
        endpoint <- paste(endpoint, "stimulus:", stimulus$name)
    }

    filePath <- postJSON(study$connection, uploadUrl, postData, message = paste("Uploading sensor data for", endpoint))
    return(filePath)
}


#' Create a temporary file with the data/metadata that needs to be uploaded.
#'
#' @inheritParams uploadSensorData
#'
#' @keywords internal
privateSaveSignalsToFile <- function(params, data, sensorName, scriptName, metadata = NULL) {
    # Create script specific data header
    dataHeader <- privateCreateDataHeader(params, sensorName, scriptName, fileDependency = attr(data, "fileDependency"))

    # Create basic metadata (FieldName / DataType) for each signal column. If additional metadata are provided,
    #  we also add these ones
    signalsMetadata <- privateCreateSignalsMetadata(data, metadata)

    # Create the temporary file
    if (exists("scratchFolder", params)) {
        # AttentionTool will always give this scratchFolder path
        dataFileName <- file.path(params$scratchFolder, "result.csv")
    } else {
        # In case of a local upload without scratchFolder path given - we will use the user tempdir location
        warning("params$scratchFolder not provided - using user tempdir location")
        dataFileName <- file.path(tempdir(check = TRUE), "result.csv")
    }

    cat(c(dataHeader, signalsMetadata, "#DATA"), file = dataFileName, sep = "\n")

    # Format data for upload (need to add a RowNumber column)
    data <- cbind(RowNumber = seq(0, nrow(data) - 1), data)
    data.table::fwrite(data, file = dataFileName, append = TRUE, col.names = TRUE)

    return(dataFileName)
}


#' Create data header for the file that needs to be uploaded.
#'
#' @inheritParams uploadSensorData
#'
#' @keywords internal
privateCreateDataHeader <- function(params, sensorName, scriptName, fileDependency = NULL) {
    # No need to save the following into the signal file metadata
    ignoreParams <- c("token", "iMotionsVersion", "flowName", "scratchFolder", "studyId", "respondentId", "segmentId",
                      "stimulusId")

    # Signals file metadata should contain script specific parameters, they will be used by the sensor data export
    metadata <- list(sampleName = sensorName, script = scriptName, fileDependency = fileDependency,
                     parameters = params[!(names(params) %in% ignoreParams)])

    # script specific parameters needs to be URL encoded
    metadata <- utils::URLencode(toJSON(metadata), reserved = TRUE)
    dataHeader <- c(params$iMotionsVersion, "#HEADER", "iMotions.RAPIData", params$flowName, "ET_RExtAPI", "", metadata)

    return(dataHeader)
}


#' Create metadata for the file that needs to be uploaded.
#'
#' @inheritParams uploadSensorData
#'
#' @keywords internal
privateCreateSignalsMetadata <- function(data, metadata = NULL) {
    # Fieldname metadata is mandatory, otherwise we won't load the data
    mandatoryMetadata <- list(c("FieldName", names(data)))

    # DataType metadata can be retrieved easily so we add it by default (integer is not supported)
    dataType <- tools::toTitleCase(c("DataType", sapply(data, typeof)))
    dataType <- sub("Integer", "Double", dataType)

    signalsMetadata <- list(dataType)

    if (!is.null(metadata)) {
        if (ncol(data) == nrow(metadata)) {
            setDT(metadata)
            signalsMetadata <- append(signalsMetadata, rbind(t(names(metadata)), metadata, use.names = FALSE))
        } else {
            warning("Wrong additional metadata format - ignoring it...")
        }
    }

    metadata <- append(mandatoryMetadata, signalsMetadata)
    metadata <- c("#METADATA", purrr::map_chr(metadata, paste, collapse = ","))
    return(unname(metadata))
}

## Exporting Data =====================================================================================================

#' Create an export file at a specific location and append metadata to it if provided. Note that a column with the
#' study name will be appended to each export.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param data A data.table containing the export metrics to save.
#' @param outputDirectory The path where the file should be created.
#' @param fileName The name of the file to create (should finish with .csv).
#' @param metadata Optional - a data.table with metadata information. Column names will be converted to metadata headers
#'                 and there must be a row corresponding to each data column.
#'
#' @export
#' @examples
#' \dontrun{
#' connection <- imotionsApi::imConnection("xxxxxxxx")
#' studies <- imotionsApi::listStudies(connection)
#' study <- imotionsApi::imStudy(connection, studies$id[1])
#' data <- data.frame("Respondent Name" = "Test", "Metric1" = seq(1:100), "Metric2" = rep(0, 100))
#' createExport(study, data, outputDirectory = "C:/Documents", fileName = "textExport.csv")
#'
#' # Adding some metadata to the data
#' metadata <- data.table("Units" = c("", "ms", ""), "Description" = c("Desc1", "Desc2", "Desc"))
#' createExport(study, data, outputDirectory = "C:/Documents", fileName = "textExport.csv", metadata)
#' }
createExport <- function(study, data, outputDirectory, fileName, metadata = NULL) {
    assert(hasArg(study), "Please specify a study loaded with `imStudy()`")
    assert(hasArg(data), "Please specify a data.table to export")
    assert(hasArg(outputDirectory), "Please specify an outputDirectory filepath to export the file")
    assert(hasArg(fileName), "Please specify the name of the file to create")

    # Verify that data is a data.table of the good format
    data <- checkDataFormat(data)
    assertExportFormat(data)

    if (!is.null(metadata)) {
        if (ncol(data) == nrow(metadata)) {
            setDT(metadata)
            metadata <- rbind(t(paste0("#", names(metadata))), metadata, use.names = FALSE)
            metadata <- purrr::map_chr(metadata, paste, collapse = ",")
        } else {
            warning("Wrong additional metadata format - ignoring it...")
        }
    }

    if (!dir.exists(outputDirectory)) dir.create(path = outputDirectory)
    dataFileName <- file.path(outputDirectory, fileName)

    # Appending study name to the export
    data <- cbind("Study Name" = study$name, data)
    writeLines(text = c("\ufeff#METADATA", unname(metadata), "#DATA"), con = dataFileName, useBytes = TRUE)
    fwrite(x = data, file = dataFileName, append = TRUE, col.names = TRUE, na = "NA")
}


## imObject specific methods ==========================================================================================

#' Create an iMotions object.
#'
#' Based on a data.table and the type of object wanted, create an imObject or imObjectList.
#'
#' @param data a list/data.frame that needs to be changed into an iMotions object.
#' @param typeOfObject a string that specifies the type of object wanted ("Stimulus","Respondent", "Segment", "Sensor",
#'         "Interval").
#'
#' @return An iMotions object of the specified type (data.table).
#' @import data.table
#' @keywords internal
#' @examples
#' \dontrun{
#' object <- list(name = "example")
#' respondentsObject <- imotionsApi:::createImObject(object, "Respondent")
#' stimuliObject <- imotionsApi:::createImObject(object, "Stimulus")
#' segmentsObject <- imotionsApi:::createImObject(object, "Segment")
#' sensorsObject <- imotionsApi:::createImObject(object, "Sensor")
#' }
createImObject <- function(data, typeOfObject) {
    setDT(data)

    if (nrow(data) > 1) {
        attr(data, "class") <- c("imObjectList", paste0("im", typeOfObject, "List"), class(data))
    } else {
        attr(data, "class") <- c("imObject", paste0("im", typeOfObject), class(data))
    }

    row.names(data) <- NULL
    return(data)
}


#' @export
`[.imObjectList` <- function(x, i, j, ...) {
    tmpClass <- class(x)
    class(x) <- c("data.table", "data.frame")

    if (!missing(i)) {
        i <- eval(substitute(i), x, parent.frame())
        x <- x[i, ...]
    }

    if (!missing(j)) {
        x <- subset(x, select = eval(j), ...)
        tmpClass <- class(x)
    }

    if (nrow(x) > 1) {
        class(x) <- tmpClass
    } else {
        class(x) <- gsub("List", "", tmpClass)
    }

    return(x)
}


#' @export
`[.imObject` <- function(x, i, j, ...) {
    tmpClass <- class(x)
    class(x) <- c("data.table", "data.frame")

    if (!missing(i)) {
        i <- eval(substitute(i), x, parent.frame())
        x <- x[i, ...]
    }

    if (!missing(j)) {
        x <- subset(x, select = eval(j), ...)
        tmpClass <- class(x)
    }

    class(x) <- tmpClass
    return(x)
}


#' @export
print.imConnection <- function(x, ...) {
    cat("Connected to iMotions at", x$baseUrl, "\n")
}


#' @export
print.imStudy <- function(x, ...) {
    cat("iMotions Study:", x$name)
    cat("\nStudy id:", x$id)
    cat("\n")
}


#' @export
print.imSegment <- function(x, ...) {
    if (nrow(x) == 0) {
        cat("No iMotions Segment found")
    } else {
        cat("iMotions Segment `", x$name, "` with ID = ", x$id, sep = "")
    }
}


#' @export
print.imStimulus <- function(x, ...) {
    if (nrow(x) == 0) {
        cat("No iMotions Stimulus found")
    } else {
        cat("iMotions Stimulus `", x$name, "` with ID = ", x$id, sep = "")
    }
}


#' @export
print.imRespondent <- function(x, ...) {
    if (nrow(x) == 0) {
        cat("No iMotions Respondent found")
    } else {
        cat("iMotions Respondent `", x$name, "` with ID = ", x$id, sep = "")
    }
}


#' @export
print.imAOI <- function(x, ...) {
    if (nrow(x) == 0) {
        cat("No iMotions AOI found")
    } else {
        cat("iMotions AOI `", x$name, "` with ID = ", x$id, sep = "")
    }
}


## Get URLs functions =================================================================================================

#' Return the basePath/baseUrl to the iMotions server based on a specific study object.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#'
#' @keywords internal
getStudyBaseUrl <- function(study) {
    file.path(study$connection$baseUrl)
}


#' Return the path/url to the studies available.
#'
#' @param connection An imConnection object as returned from \code{\link{imConnection}}.
#'
#' @keywords internal
getStudiesUrl <- function(connection) {
    file.path(connection$baseUrl, "studies")
}


#' Return the path/url of a specific study object when connecting locally.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#'
#' @keywords internal
getStudyUrl <- function(study) {
    file.path(getStudiesUrl(study$connection), study$id)
}


#' Return the path/url to a specific study based of its studyId.
#'
#' @param connection An imConnection object as returned from \code{\link{imConnection}}.
#' @param studyId The id of the study of interest.
#'
#' @keywords internal
getStudyUrlById <- function(connection, studyId) {
    file.path(getStudiesUrl(connection), studyId)
}


#' Based on the kind of connection, return the path/url to a specific study object.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#'
#' @keywords internal
getStudySpecificUrl <- function(study) {
    if (study$connection$localIM) {
        studyUrl <- getStudyUrl(study)
    } else {
        studyUrl <- getStudyBaseUrl(study)
    }

    return(studyUrl)
}


#' Generic getSensorsUrl function that takes as parameter a study object and a respondent/segment object.
#'
#' Return the path/url to the sensors for this respondent/segment object.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param imObject An imRespondent or imSegment object of interest.
#' @param stimulusId Optional - the id of the stimulus of interest.
#'
#' @keywords internal
getSensorsUrl <- function(study, imObject, stimulusId = NULL) {
    UseMethod("getSensorsUrl", object = imObject)
}


#' getSensorsUrl method to get the path/url to the sensors of a respondent object.
#'
#' @inheritParams getSensorsUrl
#'
#' @keywords internal
getSensorsUrl.imRespondent <- function(study, imObject, stimulusId = NULL) {
    if (!is.null(stimulusId)) {
        file.path(getStudySpecificUrl(study), "respondent", imObject$id, "stimuli", stimulusId, "samples")
    } else {
        file.path(getStudySpecificUrl(study), "respondent", imObject$id, "samples")
    }
}


#' Return the path/url to the signals of a specific sensor.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param sensor An imSensor object as returned from getSensors().
#'
#' @keywords internal
getSensorDataUrl <- function(study, sensor) {
    paste0(getStudyBaseUrl(study), sensor$dataUrl)
}


#' Generic getUploadSensorsUrl function that takes as parameter a study object and a respondent/segment object.
#' Return the path/url to upload a sensor to this respondent/segment object.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param imObject An imRespondent or imSegment object of interest.
#' @param stimulusId Optional - the id of the stimulus of interest.
#'
#' @keywords internal
getUploadSensorsUrl <- function(study, imObject, stimulusId = NULL) {
    UseMethod("getUploadSensorsUrl", object = imObject)
}


#' getUploadSensorsUrl method to get the path/url to upload a sensor to this respondent object.
#'
#' @inheritParams getUploadSensorsUrl
#'
#' @keywords internal
getUploadSensorsUrl.imRespondent <- function(study, imObject, stimulusId = NULL) {
    file.path(getSensorsUrl(study, imObject, stimulusId), "data")
}


#' getAOIUrl function that takes as parameter a study object and optionally a respondent or a stimulus id.
#'
#' Return the path/url to the AOIs for this study/respondent/stimulus.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param stimulusId Optional - the id of the stimulus of interest.
#' @param respondentId Optional - the id of the respondent of interest.
#'
#' @keywords internal
getAOIsUrl <- function(study, stimulusId = NULL, respondentId = NULL) {
    if (!is.null(stimulusId) & !is.null(respondentId)) {
        stop("Please provide either stimulusId or respondentId, not both.")
    }

    if (is.null(stimulusId) && is.null(respondentId)) {
        file.path(getStudyBaseUrl(study), "aois", study$id)
    } else if (!is.null(stimulusId)) {
        file.path(getStudyBaseUrl(study), "aois", study$id, "stimuli", stimulusId)
    } else if (!is.null(respondentId)) {
        file.path(getStudyBaseUrl(study), "aois", study$id, "respondent", respondentId)
    }
}


#' Return the path/url to all AOIs' details information for a specific stimulus (and optionally for a specific
#' respondent).
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param stimulusId The id of the stimulus of interest.
#' @param respondentId Optional - the id of the respondent of interest.
#'
#' @keywords internal
getAOIDetailsForStimulusUrl <- function(study, stimulusId, respondentId = NULL) {
    if (is.null(respondentId)) {
        file.path(getAOIsUrl(study, stimulusId), "*")
    } else {
        file.path(getAOIsUrl(study, stimulusId), "respondent", respondentId, "*")
    }
}


#' Return the path/url to a specific AOI's details information.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param AOI An imAOI object as returned from \code{\link{getAOIs}}.
#' @param respondentId Optional - the id of the respondent of interest.
#'
#' @keywords internal
getAOIDetailsUrl <- function(study, AOI, respondentId = NULL) {
    if (is.null(respondentId)) {
        file.path(getAOIsUrl(study, AOI$stimulusId), AOI$id)
    } else {
        file.path(getAOIsUrl(study, AOI$stimulusId), "respondent", respondentId, AOI$id)
    }
}

#' Return the path/url to the scenes of a specific respondent.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param respondent An imRespondent object as returned from \code{\link{getRespondents}}.
#'
#' @keywords internal
getRespondentScenesUrl <- function(study, respondent) {
    file.path(getStudyBaseUrl(study), "scenes", study$id, "respondent", respondent$id)
}


#' Return the path/url to the annotations of a specific respondent.
#'
#' @param study An imStudy object as returned from \code{\link{imStudy}}.
#' @param respondent An imRespondent object as returned from \code{\link{getRespondents}}.
#'
#' @keywords internal
getRespondentAnnotationsUrl <- function(study, respondent) {
    file.path(getStudyBaseUrl(study), "annotations", study$id, "respondent", respondent$id)
}


## HTTP request methods ===============================================================================================

#' Retrieve a JSON file from the indicated path/url - optionally allow the user to add more information about to know
#' when an error occurred.
#'
#' @param connection An imConnection object as returned from \code{\link{imConnection}}.
#' @param url The url/path where the JSON file is located.
#' @param message Optional - a short message indicating which steps are getting performed to get a more indicative
#'                error message.
#'
#' @return The retrieved JSON file.
#' @keywords internal
getJSON <- function(connection, url, message = NULL) {
    response <- getHttr(connection, url)
    response_status <- getHttrStatusCode(response)
    stopOnHttpError(response_status, message)

    text <- content(x = response, as = "text", encoding = "UTF-8")
    return(fromJSON(txt = text))
}


#' Post a JSON file to the indicated path/url - optionally allow the user to add more information about to know
#' when an error occurred.
#'
#' @param connection An imConnection object as returned from \code{\link{imConnection}}.
#' @param url The url/path where the JSON file will be located.
#' @param postData The JSON file that needs to be uploaded.
#' @param message Optional - a short message indicating which steps are getting performed to get a more indicative
#'                error message.
#'
#' @return The url/path where the JSON file has been uploaded.
#' @keywords internal
postJSON <- function(connection, url, postData, message = NULL) {
    response <- postHttr(connection, url, reqBody = postData)
    response_status <- getHttrStatusCode(response)
    stopOnHttpError(response_status, message)

    text <- content(x = response, as = "text", encoding = "UTF-8")
    return(fromJSON(txt = text))
}

#' Return token authentication header
#'
#' @param token The token to be used for authentication.
#'
#' @keywords internal
tokenHeaders <- function(token) httr::add_headers(Authorization = paste("Bearer", token))


#' Return json content-type specific header
#'
#' @keywords internal
jsonHeaders <- function() httr::add_headers("Content-Type" = "application/json")


#' Perform a GET HTTP request with authentication.
#'
#' The request will be retried up to 3 times if an error is encountered.
#'
#' @param connection An imConnection object as returned from \code{\link{imConnection}}.
#' @param url The url/path where the JSON file is located.
#'
#' @return The last response.
#' @keywords internal
getHttr <- function(connection, url) {
    if (connection$localIM) {
        # Locally there is no point to retry request if we get a 404 not found error
        terminate_on <- 404
    } else {
        terminate_on <- NULL
    }

    res <- RETRY("GET", url, tokenHeaders(connection$token), simplifyVector = TRUE, simplifyDataFrame = TRUE,
                 terminate_on = terminate_on)

    return(res)
}


#' Perform a POST HTTP request with authentication.
#'
#' The request will be retried up to 3 times if an error is encountered.
#'
#' @param connection An imConnection object as returned from \code{\link{imConnection}}.
#' @param url The url/path where the JSON file is located.
#' @param reqBody The body of the request.
#'
#' @return The last HTTP response.
#' @keywords internal
postHttr <- function(connection, url, reqBody) {
    res <- RETRY("POST", url, body = reqBody, tokenHeaders(connection$token), jsonHeaders(), encode = "json")
    return(res)
}

#' Perform a PUT HTTP request with authentication.
#'
#' The request will be retried up to 3 times if an error is encountered.
#'
#' @param connection An imConnection object as returned from \code{\link{imConnection}}.
#' @param url The url/path where the JSON file is located.
#' @param reqBody The body of the request.
#'
#' @return The last HTTP response.
#' @keywords internal
putHttr <- function(connection, url, reqBody) {
    res <- RETRY("PUT", url, body = reqBody, tokenHeaders(connection$token), jsonHeaders())
    return(res)
}



#' Analyse a status code from an HTTP response and stop if an error was encountered.
#'
#' @param response An HTTP response.
#' @param message Optional - a short message indicating which steps are getting performed to get a more indicative
#'                error message.
#'
#' @return The last HTTP response.
#' @keywords internal
stopOnHttpError <- function(response, message = NULL) {
    response_status <- getHttrStatusCode(response)
    if (response_status == 401) stop(paste(message, "- Token not authorized to access requested resource"))
    if (response_status == 404) stop(paste(message, "- Resource not found"))
    if (response_status != 200) stop(paste0(message, " - unexpected response status (", response_status, ")"))
}






## Test helper and data reformating functions =========================================================================

#' Stop if a condition is not fulfilled and print an error message.
#'
#' @param condition R expressions which should evaluate to TRUE.
#' @param message An error message.
#'
#' @keywords internal
assert <- function(condition, message) {
    if (!condition) stop(message)
}


#' Stop if an object is not from the good class and print an error message.
#'
#' @param object An object which class needs to be checked.
#' @param className A string indicating the class to check.
#' @param message An error message.
#'
#' @keywords internal
assertClass <- function(object, className, message) {
    if (!is.null(object)) {
        assert(inherits(object, className), message)
    }
}

#' Stop if the data to upload is in a wrong format.
#'
#' @param data An imSignals or imMetrics object of the good format.
#'
#' @keywords internal
assertUploadFormat <- function(data) {
    assert(nrow(data) > 0, "Do not upload an empty dataset")

    if (inherits(data, "imSignals")) {
        # Signals detected
        assert(ncol(data) > 1, "Dataset must contain at least two columns (Timestamp included)")
    } else if (inherits(data, "imMetrics")) {
        # Metrics detected
        assert(nrow(data) == 1, "Metrics must have exactly one row")
    } else {
        stop("Wrong data format for upload (must be imSignals or imMetrics)")
    }
}

#' Stop if the data to export is in a wrong format.
#'
#' @param data An imExport object of the good format.
#'
#' @keywords internal
assertExportFormat <- function(data) {
    if (!(inherits(data, "imExport") | inherits(data, "imMetrics"))) {
        stop("Wrong data format for export (must be imMetrics or imExport)")
    }
}

#' Check the format of a data.table and classify it as an imMetrics, imSignal or, imExport object.
#'
#' @param data A data.table containing the signals/metrics.
#'
#' @return An imSignals, imMetrics or imExport object if data was well formatted.
#' @keywords internal
checkDataFormat <- function(data) {
    if (!inherits(data, "data.frame")) {
        stop("Signals / metrics object should be data.frame or data.table")
    }

    if (!inherits(data, "data.table")) {
        setDT(data)
    }

    if (inherits(data, "imData")) {
        return(data)
    } else if (grepl("METRIC_SUMMARY=true", names(data)[2], fixed = TRUE)) {
        # Metrics from desktop detected
        data[, "Timestamp"] <- NULL
        names(data) <- sub("(METRIC_SUMMARY=true)", "", names(data), fixed = TRUE)
        class(data) <- append(class(data), c("imData", "imMetrics"))
    } else if (any(names(data) == "Timestamp")) {
        # We should not have negative timestamps
        data <- data[data$Timestamp >= 0, , drop = FALSE]

        # Signals detected
        class(data) <- append(class(data), c("imData", "imSignals"))
    } else if (nrow(data) == 1 & all(sapply(data, class) %like% "numeric|integer")) {
        # data.table metrics detected
        class(data) <- append(class(data), c("imData", "imMetrics"))
    } else {
        # data.table export detected
        class(data) <- append(class(data), c("imData", "imExport"))
    }

    return(data)
}

#' Reorder columns of a data.table based on a vector of column names.
#'
#' @param data A data.table with columns to reorder.
#' @param explicitlyOrdered An vector of column names explicitly ordered.
#'
#' @return A data.table with reordered columns.
#' @keywords internal
reorderColnames <- function(data, explicitlyOrdered) {
    explicitlyOrdered <- explicitlyOrdered[explicitlyOrdered %in% names(data)]
    data[, c(explicitlyOrdered, setdiff(names(data), explicitlyOrdered))]
}

#' Make sensors columns order a bit more intuitive, and reliable.
#'
#' @param sensors A sensors data.table to reoder.
#'
#' @return A sensors data.table with reordered columns.
#' @keywords internal
reorderSensorColumns <- function(sensors) {
    reorderColnames(sensors, c("eventSourceType", "name", "signals", "sensor", "instance", "dataUrl", "respondent"))
}


#' Reformat the gender column.
#'
#' Converts 0/1 values into MALE/FEMALE values.
#'
#' @param gender The gender column to reformat.
#'
#' @return The gender column coded as Male/Female
#' @keywords internal
formatGender <- function(gender) {
    # in case the gender is coded as "0" or "1" we update them to Male/Female
    gender[which(gender == "0")] <- "MALE"
    gender[which(gender == "1")] <- "FEMALE"

    return(gender)
}


#' Indicate that R markdown knitting should be stopped since the current stimulus/respondent/segment is not applicable
#' for this notebook.
#'
#' @param message Optional - a message explaining why this respondent was not applicable.
#'
#' @export
#' @examples
#' \dontrun{
#' stopSubjectNotApplicable("This respondent has no data.")
#' }
stopSubjectNotApplicable <- function(message = NULL) {
    if (is.null(message)) {
        stop("imotionsApi subject not applicable")
    } else {
        stop(message)
    }
}





## Unit test mocking functions ========================================================================================

content <- function(...) {
    httr::content(...)
}


fromJSON <- function(...) {
    jsonlite::fromJSON(...)
}


toJSON <- function(...) {
    jsonlite::toJSON(..., auto_unbox = TRUE)
}


fwrite <- function(...) {
    data.table::fwrite(...)
}


writeLines <- function(...) {
    base::writeLines(...)
}


dir.create <- function(...) {
    base::dir.create(...)
}


RETRY <- function(...) {
    httr::RETRY(..., times = 3, pause_base = 2, pause_cap = 60, pause_min = 1, quiet = FALSE)
}


getHttrStatusCode <- function(response) {
    httr::status_code(response)
}
