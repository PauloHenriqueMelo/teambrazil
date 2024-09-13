' Process SIH variables from DataSUS
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




process_sih_rd <- function(data) {
  # List of variables to keep if they exist in the dataset
  variables_to_keep <- c(
    "ANO_CMPT", "MES_CMPT", "ESPEC", "N_AIH", "IDENT",
    "MUNIC_RES", "NASC", "SEXO", "PROC_REA", "VAL_SH", "VAL_SP", "VAL_TOT",
    "VAL_UTI", "US_TOT", "DT_INTER", "DT_SAIDA", "DIAG_PRINC", "DIAG_SECUN",
    "COBRANCA", "NATUREZA", "MUNIC_MOV", "COD_IDADE", "IDADE", "DIAS_PERM",
    "MORTE", "CAR_INT", "CNES", "CID_ASSO", "CID_MORTE", "INFEHOSP",
    "COMPLEX", "FINANC", "FAEC_TP", "RACA_COR", "VAL_UCI", "MARCA_UCI"
  )

  # Keep only the variables that are present in the dataset
  available_vars <- intersect(variables_to_keep, names(data))
  data <- dplyr::select(data, all_of(available_vars))

  # Process SEXO variable
  if ("SEXO" %in% names(data)) {
    data$SEXO <- dplyr::case_when(
      data$SEXO == "1" ~ "Male",
      data$SEXO == "2" ~ "Female",
      data$SEXO == "3" ~ "Female",  # Treat code "3" as Female
      data$SEXO %in% c("0", "9") ~ NA_character_,
      TRUE ~ NA_character_
    )
    data$SEXO <- as.factor(data$SEXO)
  }

  # Process IDADE and COD_IDADE variables
  if ("IDADE" %in% names(data) && "COD_IDADE" %in% names(data)) {
    age <- as.numeric(data$IDADE)
    data$IDADE <- dplyr::case_when(
      data$COD_IDADE == "1" ~ age,                     # Age in years
      data$COD_IDADE == "2" ~ age / 12,                # Age in months
      data$COD_IDADE == "3" ~ age / 365,               # Age in days
      data$COD_IDADE == "4" ~ age / (365 * 24),        # Age in hours
      TRUE ~ NA_real_
    )
  }

  # Process RACA_COR variable
  if ("RACA_COR" %in% names(data)) {
    data$RACA_COR <- dplyr::case_when(
      data$RACA_COR == "1" ~ "White",
      data$RACA_COR == "2" ~ "Black",
      data$RACA_COR == "3" ~ "Brown",
      data$RACA_COR == "4" ~ "Yellow",
      data$RACA_COR == "5" ~ "Indigenous",
      data$RACA_COR %in% c("0", "9") ~ NA_character_,
      TRUE ~ NA_character_
    )
    data$RACA_COR <- as.factor(data$RACA_COR)
  }

  # Process NASC variable (birth date)
  if ("NASC" %in% names(data)) {
    data$NASC <- as.Date(data$NASC, format = "%Y%m%d")
  }

  # Process admission (DT_INTER) and discharge (DT_SAIDA) dates
  if ("DT_INTER" %in% names(data)) {
    data$DT_INTER <- as.Date(data$DT_INTER, format = "%Y%m%d")
  }

  if ("DT_SAIDA" %in% names(data)) {
    data$DT_SAIDA <- as.Date(data$DT_SAIDA, format = "%Y%m%d")
  }

  # Process monetary values: VAL_SH, VAL_SP, VAL_TOT, VAL_UTI, VAL_UCI
  monetary_vars <- c("VAL_SH", "VAL_SP", "VAL_TOT", "VAL_UTI", "VAL_UCI")
  for (var in monetary_vars) {
    if (var %in% names(data)) {
      data[[var]] <- as.numeric(data[[var]])
    }
  }

  # Process DIAS_PERM and US_TOT variables (numerical)
  numeric_vars <- c("DIAS_PERM", "US_TOT")
  for (var in numeric_vars) {
    if (var %in% names(data)) {
      data[[var]] <- as.numeric(data[[var]])
    }
  }

  # Unescape unicode characters for all character columns
  char_cols <- sapply(data, is.character)
  data[char_cols] <- lapply(data[char_cols], stringi::stri_unescape_unicode)

  # Remove unused factor levels
  data <- droplevels(data)

  return(data)
}

