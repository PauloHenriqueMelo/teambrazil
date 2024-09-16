#' pgssc_data: Download and preprocess DataSUS files
#'
#' Downloads microdata files (DBC format) from DataSUS and imports the files for use.
#'
#'
#' @docType package
#' @name teambrazil
#' @aliases teambrazil
#'
#' @importFrom utils globalVariables
#' @importFrom data.table := setDT fcase
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
utils::globalVariables(c(
  "icd_codes", "DIAG_PRINC", "Description", "proc_rea", "PROC_REA",
  "PROCEDURE", "SEXO", "MUNIC_MOV", "MUNIC_RES", "N_AIH",
  "DIAG_SECUN", "RACA_COR", "NASC", "DT_INTER", "DT_SAIDA",
  "DIAS_PERM", "US_TOT", "COBRANCA"
))

