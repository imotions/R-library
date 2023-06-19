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


fwrite <- function(...) {
    data.table::fwrite(...)
}


write.csv <- function(...) {
    utils::write.csv(...)
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
