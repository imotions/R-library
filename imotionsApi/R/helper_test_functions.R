# Wrapper functions for tests =========================================================================================

content <- function(...) {
    httr::content(...)
}


fromJSON <- function(...) {
    jsonlite::fromJSON(...)
}


toJSON <- function(...) {
    jsonlite::toJSON(..., auto_unbox = TRUE)
}

file.exists <- function(...) {
    base::file.exists(...)
}

fwrite <- function(...) {
    data.table::fwrite(...)
}

fread <- function(...) {
    data.table::fread(...)
}

writeLines <- function(...) {
    base::writeLines(...)
}


dir.create <- function(...) {
    base::dir.create(...)
}


RETRY <- function(...) {
    httr::RETRY(...)
}


status_code <- function(...) {
    httr::status_code(...)
}
