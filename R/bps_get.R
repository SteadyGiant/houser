#' bps_get
#'
#' Download Buildings Permits Survey data
#'
#' @name bps_get
#' @param path Character, length 1. Path to directory where data will download.
#' @param geography Character, length 1. Geography (unit of observation) of data
#'     to download. One of "state", "county", "msa", or "place".
#' @param formats Character. Format of data to download. Can be any combo or all
#'     of "a", "c", "y", or "r". Use \code{\link{bps_doc}} to get more info from
#'     the BPS documentation.
#' @param years Numeric. One or more years for which to download data.
#' @param months Character. DO NOT USE right now. One or more months for which
#'     to download data. If NULL, all available months will be downloaded.
#' @param regions Character. Can be any combo or all of "mw", "ne", "so", and
#'     "we", or "all".
#' @param return_log_df Logical, length 1.
#' @return Downloads data in the background. By default, returns nothing. If
#'     `return_log_df = TRUE`, returns a data frame containing metadata for all
#'     files downloaded.
#' @export
#' @seealso \code{\link{bps_read}}
#'
bps_get = function(path, geography = "state", formats = "a", years = NULL,
                   months = NULL, regions = NULL, return_log_df = F) {

  if (is.null(path)
      | !is.character(path)
      | length(path) > 1) {
    stop("`path` must be character vector of length 1.")
  }

  if (is.null(geography)
      | !is.character(geography)) {
    stop("`geographies` must be character vector of length 1.")
  }

  if (is.null(formats)
      | !formats %in% c("c", "y", "r", "a")) {
    stop("`formats` must equal \"c\", \"y\", \"r\", or \"a\". See ?get_bps.")
  }

  if (!is.null(years)
      & !is.numeric(years)) {
    stop("`years` must be numeric or character vector.")
  }

  if (!is.null(months)
      & !is.numeric(months)) {
    stop("`months` must be numeric or character vector.")
  }

  if (geography == "place") {
    if (!is.character(regions)
        | is.null(regions)){
      stop("If `geography` arg is 'place', `region` arg must equal \"mw\", ",
           "\"ne\", \"so\", \"we\", or \"all\".")
    }
  }

  if (geography != "place") {
    if (!is.null(regions)) {
      warning("If `geography` arg is not 'place', then `regions` arg is ",
              "ignored.")
    }
  }

  if (geography != "place") {

    to_dwnld = geography %>%
      rep(times = length(years)) %>%
      rep(times = length(formats)) %>%
      as.data.frame(stringsAsFactors = F) %>%
      `names<-`("geography")

    to_dwnld$year = rep(years, times = length(formats))

    to_dwnld = to_dwnld[order(to_dwnld$year),]

    to_dwnld$format = rep(formats, times = length(years))

    base_url = "https://www2.census.gov/econ/bps/"

    to_dwnld$parent_url = switch(geography,
                                 "state" = paste0(base_url, "State/"),
                                 "county" = paste0(base_url, "County/"),
                                 "msa" = paste0(base_url, "Metro/"))

    filename_geog = switch(geography,
                           "state" = "st",
                           "county" = "co",
                           "msa" = "ma")

    to_dwnld$filename = paste0(filename_geog, to_dwnld$year,
                               to_dwnld$format,
                               ".txt")

    to_dwnld$url = paste0(to_dwnld$parent_url, to_dwnld$filename)

  }

  if (geography == "place") {

    to_dwnld =
      c("mw", "ne", "so", "we") %>%
      rep(times = length(years)) %>%
      rep(times = length(formats)) %>%
      as.data.frame(stringsAsFactors = F) %>%
      `names<-`("region")

    to_dwnld$year = NA
    to_dwnld$format = NA

    if (!all(regions == "all")) {
      to_dwnld = to_dwnld[to_dwnld$region %in% regions,]
    }

    to_dwnld = to_dwnld[order(to_dwnld$region),]

    to_dwnld$year = rep(years, times = length(regions))

    to_dwnld = to_dwnld[order(to_dwnld$region, to_dwnld$year),]

    to_dwnld$format = rep(formats, times = length(regions))

    to_dwnld$parent_url = factor(to_dwnld$region)

    levels(to_dwnld$parent_url) =
      list("https://www2.census.gov/econ/bps/Place/Midwest%20Region/"   = "mw",
           "https://www2.census.gov/econ/bps/Place/Northeast%20Region/" = "ne",
           "https://www2.census.gov/econ/bps/Place/South%20Region/"     = "so",
           "https://www2.census.gov/econ/bps/Place/West%20Region/"      = "we")

    to_dwnld$parent_url = as.character(to_dwnld$parent_url)

    to_dwnld$filename = paste0(to_dwnld$region, to_dwnld$year, to_dwnld$format,
                               ".txt")

    to_dwnld$url = paste0(to_dwnld$parent_url, to_dwnld$filename)

  }

  for (i in 1:nrow(to_dwnld)) {
    download.file(url = to_dwnld$url[[i]],
                  destfile = paste0(path, to_dwnld$filename[[i]]))

    Sys.sleep(1)
  }

  if (return_log_df == T) {
    to_dwnld
  }

}
