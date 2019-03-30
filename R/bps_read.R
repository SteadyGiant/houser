#' Read annual BPS data
#'
#' Load BPS data and clean messy column names
#'
#' @name bps_read
#' @param file Character, length 1. Path to BPS annual data file.
#' @param repair_names Logical, length 1. If true, applies a custom name repair
#'     function. Does NOT use the 'tibble' package's name repair method.
#' @return Data frame. At the moment, the 'tibble' package is still maturing.
#'     Its name repair method has changed quite a bit in the past month or so.
#'     Once it's stable, perhaps I'll use it.
#' @export
#' @importFrom readr read_csv
#'
bps_read = function(file, repair_names = F) {

  bps = read_csv(file = file,
                 col_names = F)

  bps_names = bps %>%
    `[`(1:2,) %>%
    t() %>%
    as.data.frame(row.names = F,
                  stringsAsFactors = F)

  bps %<>%
    `[`(-c(1:2),)

  bps_names = paste(bps_names[, 1], bps_names[, 2])

  bps_names %<>%
    gsub("\\s*NA\\s*", "", .)

  if (repair_names == T) {
    bps_names %<>%
      gsub(" ", "_", .) %>%
      gsub("(?<=\\d)\\-(?=\\d)", "_to_", ., perl = T) %>%
      gsub("\\+", "_plus", .) %>%
      gsub("\\-", "_", .) %>%
      make.names(unique = T) %>%
      gsub("\\.", "_", .)
  }

  bps %<>%
    `names<-`(bps_names)

  bps

}
