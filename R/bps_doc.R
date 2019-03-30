#' bps_doc
#'
#' Open BPS documentation (PDF) in system default PDF viewer
#'
#' @name bps_doc
#' @param geography Character, length 1. One of "state", "county", "msa", or
#'     "place".
#' @description Source: https://www2.census.gov/econ/bps/Documentation/
#' @export
#'
bps_doc = function(geography = "place") {
  if (geography == "state") {
    system('xdg-open "./inst/documents/stateasc.pdf"')
  } else if (geography == "county") {
    system('xdg-open "./inst/documents/cntyasc.pdf"')
  } else if (geography == "msa") {
    system('xdg-open "./inst/documents/msaasc.pdf"')
  } else if (geography == "place") {
    system('xdg-open "./inst/documents/placeasc.pdf"')
  }
}
