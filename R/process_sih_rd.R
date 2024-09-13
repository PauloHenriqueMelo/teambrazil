process_sih_rd <- function(data) {
  # Initialize a data frame with the same number of rows as 'data'
  processed_data <- data.frame(row.names = seq_len(nrow(data)))

  # List of variables to process
  variables_to_keep <- c(
    "ANO_CMPT", "MES_CMPT", "ESPEC", "N_AIH", "IDENT",
    "MUNIC_RES", "NASC", "SEXO", "PROC_REA", "VAL_SH", "VAL_SP", "VAL_TOT",
    "VAL_UTI", "US_TOT", "DT_INTER", "DT_SAIDA", "DIAG_PRINC", "DIAG_SECUN",
    "COBRANCA", "NATUREZA", "MUNIC_MOV", "COD_IDADE", "IDADE", "DIAS_PERM",
    "MORTE", "CAR_INT", "CNES", "CID_ASSO", "CID_MORTE", "INFEHOSP",
    "COMPLEX", "FINANC", "FAEC_TP", "RACA_COR", "VAL_UCI", "MARCA_UCI"
  )

  # Process each variable individually
  for (var in variables_to_keep) {
    if (var %in% names(data)) {
      # Special processing for specific variables
      if (var == "SEXO") {
        processed_data$sex <- dplyr::case_when(
          data$SEXO == "1" ~ "Male",
          data$SEXO == "2" ~ "Female",
          data$SEXO == "3" ~ "Female",  # Treat code "3" as Female
          data$SEXO %in% c("0", "9") ~ NA_character_,
          TRUE ~ NA_character_
        )
        processed_data$sex <- as.factor(processed_data$sex)
      } else if (var == "IDADE" && "COD_IDADE" %in% names(data)) {
        # Process age based on COD_IDADE
        age <- as.numeric(data$IDADE)
        processed_data$age <- dplyr::case_when(
          data$COD_IDADE == "1" ~ age,                     # Age in years
          data$COD_IDADE == "2" ~ age / 12,                # Age in months
          data$COD_IDADE == "3" ~ age / 365,               # Age in days
          data$COD_IDADE == "4" ~ age / (365 * 24),        # Age in hours
          TRUE ~ NA_real_
        )
      } else if (var == "RACA_COR") {
        processed_data$race <- dplyr::case_when(
          data$RACA_COR == "1" ~ "White",
          data$RACA_COR == "2" ~ "Black",
          data$RACA_COR == "3" ~ "Brown",
          data$RACA_COR == "4" ~ "Yellow",
          data$RACA_COR == "5" ~ "Indigenous",
          data$RACA_COR %in% c("0", "9") ~ NA_character_,
          TRUE ~ NA_character_
        )
        processed_data$race <- as.factor(processed_data$race)
      } else if (var == "NASC") {
        # Process birth date
        processed_data$birth_date <- as.Date(data$NASC, format = "%Y%m%d")
      } else if (var %in% c("DT_INTER", "DT_SAIDA")) {
        # Process admission and discharge dates
        processed_data[[var]] <- as.Date(data[[var]], format = "%Y%m%d")
      } else if (var %in% c("VAL_SH", "VAL_SP", "VAL_TOT", "VAL_UTI", "VAL_UCI")) {
        # Process monetary values
        processed_data[[var]] <- as.numeric(data[[var]])
      } else if (var %in% c("DIAS_PERM", "US_TOT")) {
        # Process numerical variables
        processed_data[[var]] <- as.numeric(data[[var]])
      } else {
        # Default processing for other variables
        processed_data[[var]] <- data[[var]]
      }
    }
  }

  # Unescape unicode characters for all character columns
  char_cols <- sapply(processed_data, is.character)
  processed_data[char_cols] <- lapply(processed_data[char_cols], stringi::stri_unescape_unicode)

  # Remove unused factor levels
  processed_data <- droplevels(processed_data)

  # Convert to tibble for better display and compatibility
  processed_data <- tibble::as_tibble(processed_data)

  return(processed_data)
}

