#' Process SIH variables from DataSUS
#'
#' This function processes key SIH variables, converting them to appropriate formats and labels, if the variables exist in the data.
#'
#' @param data A \code{data.frame} containing SIH data.
#' @return A \code{data.frame} with the processed data.
#' @examples
#' # Example usage:
#' process_sih_rd(sih_rd_sample)
#' @import dplyr
#' @importFrom dplyr mutate
#' @export
#'
process_sih_rd <- function(data) {

  # Variables names
  variables_names <- names(data)

  # Use dtplyr
  data <- dtplyr::lazy_dt(data)

  # SEXO (now renamed to sex)
  if ("SEXO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(SEXO = as.character(.data$SEXO)) %>%
      dplyr::mutate(sex = dplyr::case_match(
        .data$SEXO,
        "1" ~ "Male",       # Masculino becomes Male
        "2" ~ "Female",     # Feminino becomes Female
        "3" ~ "Female",     # Treat code "3" as Female as per your logic
        "0" ~ NA_character_,  # Code "0" becomes NA
        "9" ~ NA_character_,  # Code "9" becomes NA
        .default = NA_character_  # Any other value becomes NA
      )) %>%
      dplyr::mutate(sex = as.factor(.data$sex)) %>%
      dplyr::select(-SEXO)  # Optionally drop the original SEXO column if no longer needed
  }


  # From data.table to tibble
  data <- tibble::as_tibble(data)

  # Purge levels
  data <- droplevels(data.table::as.data.table(data))

  # Unescape unicode characters
  data <- suppressWarnings(tibble::as_tibble(lapply(X = data, FUN = stringi::stri_unescape_unicode)))


  return(data)
}
