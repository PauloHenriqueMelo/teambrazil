



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

  # Find the correct path to the HospitalCity.RData file
  data_path <- system.file("data", "HospitalCity.RData", package = "pgsscdata")

  # Check if the file exists before loading
  if (file.exists(data_path)) {
    load(data_path)  # This loads the HospitalCity dataset
  } else {
    stop("HospitalCity.RData not found in the package.")
  }
  # List of variables to keep if they exist in the dataset
  variables_to_keep <- c(
    "ANO_CMPT", "MES_CMPT", "ESPEC", "N_AIH", "IDENT",
    "MUNIC_RES", "NASC", "SEXO", "PROC_REA", "VAL_SH", "VAL_SP", "VAL_TOT",
    "VAL_UTI", "US_TOT", "DT_INTER", "DT_SAIDA", "DIAG_PRINC", "DIAG_SECUN",
    "COBRANCA", "NATUREZA", "MUNIC_MOV", "COD_IDADE", "IDADE", "DIAS_PERM",
    "MORTE", "CAR_INT", "CNES", "CID_ASSO", "CID_MORTE", "INFEHOSP",
    "COMPLEX", "FINANC", "RACA_COR", "VAL_UCI", "MARCA_UCI"
  )

  # Keep only the variables that are present in the dataset
  available_vars <- intersect(variables_to_keep, names(data))
  data <- dplyr::select(data, all_of(available_vars))
  variables_names <- names(data)
  # Process SEXO variable
  if ("SEXO" %in% names(data)) {
    data$SEXO <- dplyr::case_when(
      data$SEXO == "1" ~ "Male",
      data$SEXO == "2" ~ "Female",
      data$SEXO == "3" ~ "Female",  # Treat code "3" as Female
      data$SEXO %in% c("0", "9") ~ NA_character_,
      TRUE ~ NA_character_
    )
    data$sex <- as.factor(data$SEXO)
    data <- data %>% dplyr::select(-SEXO)
  }

  # Rename MUNIC_MOV to Hospital_CityCod in 'data'
  data <- data %>%
    dplyr::rename(Hospital_CityCod = MUNIC_MOV)

  # Ensure both columns are numeric
  data$Hospital_CityCod <- as.numeric(data$Hospital_CityCod)
  HospitalCity$Hospital_CityCod <- as.numeric(HospitalCity$Hospital_CityCod)

  # Perform the left join
  data <- data %>%
    dplyr::left_join(HospitalCity, by = "Hospital_CityCod")

  # Process RACA_COR variable
  if ("RACA_COR" %in% names(data)) {
    # Ensure RACA_COR is treated as character for consistency
    data$RACA_COR <- as.character(data$RACA_COR)

    data$RACA_COR <- dplyr::case_when(
      data$RACA_COR %in% c("1", "01") ~ "White",
      data$RACA_COR %in% c("2", "02") ~ "Black",
      data$RACA_COR %in% c("3", "03") ~ "Brown",
      data$RACA_COR %in% c("4", "04") ~ "Yellow",
      data$RACA_COR %in% c("5", "05") ~ "Indigenous",
      data$RACA_COR %in% c("0", "9", "00", "09") ~ NA_character_,
      TRUE ~ NA_character_
    )

    data$race <- as.factor(data$RACA_COR)
    data <- data %>% dplyr::select(-RACA_COR)
  }


  # Process NASC variable (birth date)
  if ("NASC" %in% names(data)) {
    data$DOB <- as.Date(data$NASC, format = "%Y%m%d")
    data <- data %>% dplyr::select(-NASC)
  }

  # Process admission (DT_INTER) and discharge (DT_SAIDA) dates
  if ("DT_INTER" %in% names(data)) {
    data$DT_HOSP <- as.Date(data$DT_INTER, format = "%Y%m%d")
    data <- data %>% dplyr::select(-DT_INTER) # Remove DT_SAIDA
  }

  if ("DT_SAIDA" %in% names(data)) {
    data$DT_DISCHARGE <- as.Date(data$DT_SAIDA, format = "%Y%m%d")
    data <- data %>% dplyr::select(-DT_SAIDA) # Remove DT_SAIDA
  }

  # Process monetary values: VAL_SH, VAL_SP, VAL_TOT, VAL_UTI, VAL_UCI
  monetary_vars <- c("VAL_SH", "VAL_SP", "VAL_TOT", "VAL_UTI", "VAL_UCI")
  for (var in monetary_vars) {
    if (var %in% names(data)) {
      data[[var]] <- as.numeric(data[[var]])
    }
  }

  # DIAS_PERM
  if("DIAS_PERM" %in% variables_names){
    data <- data %>%
      dplyr::mutate(LOS = as.numeric(.data$DIAS_PERM))
      data <- data %>% dplyr::select(-DIAS_PERM)
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

