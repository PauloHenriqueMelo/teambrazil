#' pgssc_data: Download and preprocess DataSUS files
#'
#' Downloads microdata files (DBC format) from DataSUS and imports the files for use.
#'
#'
#' @docType package
#' @name pgsscdata
#' @aliases pgsscdata
#'
#' @importFrom utils globalVariables
#' @importFrom data.table := setDT fcase
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
utils::globalVariables( c('a') )

