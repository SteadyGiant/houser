#' bps_get
#'
#' Download Buildings Permits Survey data
#'
#' @name bps_get
#' @param path Character, length 1. Path to directory where data will download.
#' @param formats Character. Format of data to download. Can be any combo or all
#'     of "a", "c", "y", or "r". Use \code{\link{bps_doc}} to get more info from
#'     the BPS documentation.
#' @param regions Character. Can be any combo or all of "mw", "ne", "so", and
#'     "we" (Midwest, Northeast, South, West), or "all".
#' @param years Numeric. One or more years for which to download data.
#' @param months Character. One or more months for which to download data.
#'     If NULL, all available months will be downloaded.
#' @param return_log_df Logical, length 1.
#' @return Downloads data in the background. By default, returns nothing. If
#'     `return_log_df = TRUE`, returns a data frame containing metadata for all
#'     files downloaded.
#' @export
#' @seealso \code{\link{bps_read}}
#'
bps_get = function(path, formats = "a", regions = "all", years = NULL,
                   months = NULL, return_log_df = F) {

  if (is.null(path)
      | !is.character(path)
      | length(path) > 1) {
    stop("`path` must be character class of length 1.")
  }

  if (is.null(formats)
      | !formats %in% c("c", "y", "r", "a")) {
    stop("`formats` must equal \"c\", \"y\", \"r\", or \"a\". See ?get_bps.")
  }

  if (is.null(regions)
      | !is.character(regions)) {
    stop("`region` must equal \"mw\", \"ne\", \"so\", \"we\", or \"all\".",
         " See ?get_bps.")
  }

  if (!is.null(years)
      & !is.numeric(years)) {
    stop("`years` must be numeric or character class.")
  }

  if (!is.null(months)
      & !is.numeric(months)) {
    stop("`months` must be numeric or character class.")
  }

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

  to_dwnld$filename = paste0(to_dwnld$region, to_dwnld$year, to_dwnld$format,
                             ".txt")

  to_dwnld$parent_url = factor(to_dwnld$region)

  levels(to_dwnld$parent_url) =
    list("https://www2.census.gov/econ/bps/Place/Midwest%20Region/"   = "mw",
         "https://www2.census.gov/econ/bps/Place/Northeast%20Region/" = "ne",
         "https://www2.census.gov/econ/bps/Place/South%20Region/"     = "so",
         "https://www2.census.gov/econ/bps/Place/West%20Region/"      = "we")

  to_dwnld$parent_url = as.character(to_dwnld$parent_url)

  to_dwnld$url = paste0(to_dwnld$parent_url, to_dwnld$filename)

  for (i in 1:nrow(to_dwnld)) {
    download.file(url = to_dwnld$url[[i]],
                  destfile = paste0(path, to_dwnld$filename[[i]]))

    Sys.sleep(1)
  }

  if (return_log_df == T) {
    to_dwnld
  }

}
