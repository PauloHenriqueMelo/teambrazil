



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




process_rd <- function(data) {

  utils::data("icd_codes", package = "pgsscdata")
  utils::data("proc_rea", package = "pgsscdata")
  utils::data("ibge", package = "pgsscdata")
  # Load the dataset from the downloaded file
  #HospitalCity <- readr::read_csv("https://raw.githubusercontent.com/PauloHenriqueMelo/test/main/data/HospitalCity.csv")



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



  # Process DIAG_PRINC variable
  if ("DIAG_PRINC" %in% names(data)) {
    # Ensure DIAG_PRINC is character
    data$DIAG_PRINC <- as.character(data$DIAG_PRINC)

    # Left join with the icd_codes dataset to get the descriptions
    data <- data %>%
      dplyr::left_join(icd_codes, by = c("DIAG_PRINC" = "code"))

    data <- data %>%
      dplyr::rename(ICD_10_MD = DIAG_PRINC)
    # Optionally rename the Description column
    data <- data %>%
      dplyr::rename(Main_Diagnosis = Description)
  }

  # Process DIAG_PRINC variable
  if ("PROC_REA" %in% names(data)) {
    # Ensure DIAG_PRINC is character
    data$PROC_REA <- as.character(data$PROC_REA)

    # Left join with the proce_rea dataset to get the descriptions
    data <- data %>%
      dplyr::left_join(proc_rea, by = c("PROC_REA" = "code"))

    data <- data %>%
      dplyr::rename(Procedure_Code = PROC_REA)
    # Optionally rename the Description column
    data <- data %>%
      dplyr::rename(Main_Procedure = PROCEDURE)
  }






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




  if ("MUNIC_MOV" %in% names(data)) {
    # Rename MUNIC_MOV to Hospital_CityCod in 'data'
    data <- data %>%
      dplyr::rename(Hospital_CityCod = MUNIC_MOV)

    # Ensure both columns are numeric
    data$Hospital_CityCod <- as.numeric(data$Hospital_CityCod)
    ibge$Hospital_CityCod <- as.numeric(ibge$Hospital_CityCod)

    # Perform the left join
    data <- data %>%
      dplyr::left_join(ibge, by = "Hospital_CityCod")
  }


  if ("MUNIC_RES" %in% names(data)){
    # Rename MUNIC_RES to Patient_CityCod in 'data'
    data <- data %>%
      dplyr::rename(Patient_CityCod = MUNIC_RES)

    # Ensure both columns are numeric
    data$Patient_CityCod <- as.numeric(data$Patient_CityCod)

    # Rename the Hospital* variables to Patient* directly in HospitalCity
    ibge <- ibge %>%
      dplyr::rename_with(~ gsub("^Hospital", "Patient", .), starts_with("Hospital"))

    # Ensure both columns are numeric
    ibge$Patient_CityCod <- as.numeric(ibge$Patient_CityCod)

    # Perform the left join with the modified HospitalCity dataset
    data <- data %>%
      dplyr::left_join(ibge, by = "Patient_CityCod")
  }



  # Process RACA_COR variable
  if ("N_AIH" %in% names(data)) {
    # Ensure RACA_COR is treated as character for consistency
    data$ID <- as.character(data$N_AIH)
    data <- data %>% dplyr::select(-N_AIH)
  }
  # Process RACA_COR variable
  if ("DIAG_SECUN" %in% names(data)) {
    # Ensure RACA_COR is treated as character for consistency
    data$ICD_10_SD <- as.character(data$DIAG_SECUN)
    data <- data %>% dplyr::select(-DIAG_SECUN)
  }

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
  if("DIAS_PERM" %in% names(data)){
    data <- data %>%
      dplyr::mutate(LOS = as.numeric(.data$DIAS_PERM))
    data <- data %>% dplyr::select(-DIAS_PERM)
  }

  if("US_TOT" %in% names(data)){
    data <- data %>%
      dplyr::mutate(Total_Cost_USD = as.numeric(.data$US_TOT))
    data <- data %>% dplyr::select(-US_TOT)
  }

  # COBRANCA (reason for discharge/stay, SAS ordinance 719)
  if("COBRANCA" %in% names(data)){
    data <- data %>%
      dplyr::mutate(COBRANCA = as.character(.data$COBRANCA)) %>%
      dplyr::mutate(outcome = dplyr::case_when(
        .data$COBRANCA == "11" ~ "Discharge cured",
        .data$COBRANCA == "12" ~ "Discharge improved",
        .data$COBRANCA == "14" ~ "Discharge at request",
        .data$COBRANCA == "15" ~ "Discharge with return for patient follow-up",
        .data$COBRANCA == "16" ~ "Discharge due to evasion",
        .data$COBRANCA == "18" ~ "Discharge for other reasons",
        .data$COBRANCA == "19" ~ "Discharge of acute patient in psychiatry",
        .data$COBRANCA == "21" ~ "Stay due to disease-specific characteristics",
        .data$COBRANCA == "22" ~ "Stay due to complications",
        .data$COBRANCA == "23" ~ "Stay due to socio-familial impossibility",
        .data$COBRANCA == "24" ~ "Stay for organ, tissue, cell donation - living donor",
        .data$COBRANCA == "25" ~ "Stay for organ, tissue, cell donation - deceased donor",
        .data$COBRANCA == "26" ~ "Stay due to procedure change",
        .data$COBRANCA == "27" ~ "Stay due to reoperation",
        .data$COBRANCA == "28" ~ "Stay for other reasons",
        .data$COBRANCA == "29" ~ "Transfer to home hospitalization",
        .data$COBRANCA == "32" ~ "Transfer to home hospitalization",
        .data$COBRANCA == "31" ~ "Transfer to another facility",
        .data$COBRANCA == "41" ~ "Death with Death Certificate issued by attending physician",
        .data$COBRANCA == "42" ~ "Death with Death Certificate issued by the Medical Examiner's Office",
        .data$COBRANCA == "43" ~ "Death with Death Certificate issued by the SVO",
        .data$COBRANCA == "51" ~ "Administrative closure",
        .data$COBRANCA == "61" ~ "Discharge of mother/puerpera and newborn",
        .data$COBRANCA == "17" ~ "Discharge of mother/puerpera and newborn",
        .data$COBRANCA == "62" ~ "Discharge of mother/puerpera and stay of newborn",
        .data$COBRANCA == "13" ~ "Discharge of mother/puerpera and stay of newborn",
        .data$COBRANCA == "63" ~ "Discharge of mother/puerpera and death of newborn",
        .data$COBRANCA == "64" ~ "Discharge of mother/puerpera with fetal death",
        .data$COBRANCA == "65" ~ "Death of pregnant woman and conceptus",
        .data$COBRANCA == "66" ~ "Death of mother/puerpera and discharge of newborn",
        .data$COBRANCA == "67" ~ "Death of mother/puerpera and stay of newborn",
        TRUE ~ .data$COBRANCA  # Default case to handle unknown values
      )) %>%
      dplyr::mutate(outcome = as.factor(.data$outcome)) %>%
      dplyr::select(-COBRANCA)  # Remove COBRANCA column
  }




  # Unescape unicode characters for all character columns
  char_cols <- sapply(data, is.character)
  data[char_cols] <- lapply(data[char_cols], stringi::stri_unescape_unicode)

  # Remove unused factor levels
  data <- droplevels(data)

  return(data)
}

