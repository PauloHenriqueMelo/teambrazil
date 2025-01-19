# -------------------- HELPER FUNCTIONS --------------------

# 1) Convert numeric columns: old_col -> new_col, then remove old_col
convert_numeric <- function(data, old_col, new_col) {
  if (old_col %in% names(data)) {
    data <- data %>%
      dplyr::mutate(
        !!new_col := as.numeric(.data[[old_col]])
      ) %>%
      dplyr::select(-dplyr::all_of(old_col))
  }
  data
}

# 2) Label FINANC -> Healthcare_Financing
label_financ <- function(data, col_name = "FINANC") {
  if (!(col_name %in% names(data))) return(data)
  data <- data %>%
    dplyr::mutate(Healthcare_Financing = as.character(.data[[col_name]])) %>%
    dplyr::mutate(Healthcare_Financing = dplyr::case_when(
      Healthcare_Financing %in% c("1", "01") ~ "Basic Care (PAB)",
      Healthcare_Financing %in% c("2", "02") ~ "Pharmaceutical Assistance",
      Healthcare_Financing %in% c("4", "04") ~ "Strategic Actions and Compensations Fund (FAEC)",
      Healthcare_Financing %in% c("5", "05") ~ "MAC Incentive",
      Healthcare_Financing %in% c("6", "06") ~ "Medium and High Complexity (MAC)",
      Healthcare_Financing %in% c("7", "07") ~ "Health Surveillance",
      Healthcare_Financing %in% c("0", "00", "99") ~ NA_character_,
      TRUE ~ ""  # If no match
    )) %>%
    dplyr::mutate(Healthcare_Financing = as.factor(.data$Healthcare_Financing)) %>%
    dplyr::select(-dplyr::all_of(col_name))
  data
}

# 3) Label COMPLEX -> Healthcare_Complexity
label_complex <- function(data, col_name = "COMPLEX") {
  if (!(col_name %in% names(data))) return(data)
  data <- data %>%
    dplyr::mutate(Healthcare_Complexity = as.character(.data[[col_name]])) %>%
    dplyr::mutate(Healthcare_Complexity = dplyr::case_when(
      Healthcare_Complexity %in% c("1", "01") ~ "Basic Care",
      Healthcare_Complexity %in% c("2", "02") ~ "Medium Complexity",
      Healthcare_Complexity %in% c("3", "03") ~ "High Complexity",
      Healthcare_Complexity %in% c("0", "00", "99") ~ NA_character_,
      TRUE ~ Healthcare_Complexity
    )) %>%
    dplyr::mutate(Healthcare_Complexity = as.factor(.data$Healthcare_Complexity)) %>%
    dplyr::select(-dplyr::all_of(col_name))
  data
}

# 4) Label CAR_INT -> Hospitalization_Type
label_car_int <- function(data, col_name = "CAR_INT") {
  if (!(col_name %in% names(data))) return(data)
  data <- data %>%
    dplyr::mutate(Hospitalization_Type = as.character(.data[[col_name]])) %>%
    dplyr::mutate(Hospitalization_Type = dplyr::case_when(
      Hospitalization_Type %in% c("1", "01") ~ "Elective",
      Hospitalization_Type %in% c("2", "02") ~ "Emergency",
      Hospitalization_Type %in% c("3", "03") ~ "Workplace accident or in company service",
      Hospitalization_Type %in% c("4", "04") ~ "Accident on commute to work",
      Hospitalization_Type %in% c("5", "05") ~ "Other types of traffic accident",
      Hospitalization_Type %in% c("6", "06") ~ "Other types of injury/poisoning",
      TRUE ~ ""
    )) %>%
    dplyr::mutate(Hospitalization_Type = as.factor(.data$Hospitalization_Type)) %>%
    dplyr::select(-dplyr::all_of(col_name))
  data
}

# 5) Label MARCA_UTI -> ICU_Usage_Mark
label_marca_uti <- function(data, col_name = "MARCA_UTI") {
  if (!(col_name %in% names(data))) return(data)
  data <- data %>%
    dplyr::mutate(ICU_Usage_Mark = as.character(.data[[col_name]])) %>%
    dplyr::mutate(ICU_Usage_Mark = dplyr::case_when(
      ICU_Usage_Mark %in% c("0", "00")  ~ "Did not use ICU",
      ICU_Usage_Mark == "51" ~ "Adult ICU - Type II COVID-19",
      ICU_Usage_Mark == "52" ~ "Pediatric ICU - Type II COVID-19",
      ICU_Usage_Mark == "74" ~ "Adult ICU - Type I",
      ICU_Usage_Mark == "75" ~ "Adult ICU - Type II",
      ICU_Usage_Mark == "76" ~ "Adult ICU - Type III",
      ICU_Usage_Mark == "77" ~ "Infant ICU - Type I",
      ICU_Usage_Mark == "78" ~ "Infant ICU - Type II",
      ICU_Usage_Mark == "79" ~ "Infant ICU - Type III",
      ICU_Usage_Mark == "80" ~ "Neonatal ICU - Type I",
      ICU_Usage_Mark == "81" ~ "Neonatal ICU - Type II",
      ICU_Usage_Mark == "82" ~ "Neonatal ICU - Type III",
      ICU_Usage_Mark == "83" ~ "Burn ICU",
      ICU_Usage_Mark == "85" ~ "Coronary ICU - UCO Type II",
      ICU_Usage_Mark == "86" ~ "Coronary ICU - UCO Type III",
      ICU_Usage_Mark == "99" ~ "ICU Donor",
      ICU_Usage_Mark %in% c("1", "01")  ~ "Used more than one type of ICU",
      TRUE ~ ""
    )) %>%
    dplyr::mutate(ICU_Usage_Mark = as.factor(.data$ICU_Usage_Mark)) %>%
    dplyr::select(-dplyr::all_of(col_name))
  data
}

# 6) Label RACA_COR -> race
label_raca_cor <- function(data, col_name = "RACA_COR") {
  if (!(col_name %in% names(data))) return(data)
  data <- data %>%
    dplyr::mutate(RACA_COR = as.character(.data[[col_name]])) %>%
    dplyr::mutate(RACA_COR = dplyr::case_when(
      RACA_COR %in% c("1", "01") ~ "White",
      RACA_COR %in% c("2", "02") ~ "Black",
      RACA_COR %in% c("3", "03") ~ "Brown",
      RACA_COR %in% c("4", "04") ~ "Yellow",
      RACA_COR %in% c("5", "05") ~ "Indigenous",
      RACA_COR %in% c("0", "9", "00", "09") ~ NA_character_,
      TRUE ~ NA_character_
    )) %>%
    dplyr::mutate(race = as.factor(.data$RACA_COR)) %>%
    dplyr::select(-dplyr::all_of(col_name))
  data
}

# 7) Label Morte -> Death
label_morte <- function(data, col_name = "MORTE") {
  if (!(col_name %in% names(data))) return(data)
  data <- data %>%
    dplyr::mutate(tmp_death = as.character(.data[[col_name]])) %>%
    dplyr::mutate(tmp_death = dplyr::case_when(
      tmp_death == "0" ~ "No",
      tmp_death == "1" ~ "Yes",
      TRUE ~ NA_character_
    )) %>%
    dplyr::mutate(Death = as.factor(.data$tmp_death)) %>%
    dplyr::select(-tmp_death, -dplyr::all_of(col_name))
  data
}

# 8) Label IDENT
label_ident <- function(data, col_name = "IDENT") {
  if (!(col_name %in% names(data))) return(data)
  data <- data %>%
    dplyr::mutate(IDENT = as.character(.data[[col_name]])) %>%
    dplyr::mutate(IDENT = dplyr::case_when(
      IDENT %in% c("1", "01") ~ "Primary",
      IDENT %in% c("3", "03") ~ "Continuation",
      IDENT %in% c("5", "05") ~ "Long stay",
      TRUE ~ IDENT
    )) %>%
    dplyr::mutate(IDENT = as.factor(.data$IDENT))
  data
}

# 9) Label COBRANCA -> outcome
label_cobranca <- function(data, col_name = "COBRANCA") {
  if (!(col_name %in% names(data))) return(data)
  data <- data %>%
    dplyr::mutate(tmp_cobranca = as.character(.data[[col_name]])) %>%
    dplyr::mutate(outcome = dplyr::case_when(
      tmp_cobranca == "11" ~ "Discharged: Cured",
      tmp_cobranca == "12" ~ "Discharged: Improved",
      tmp_cobranca == "14" ~ "Discharged: At request",
      tmp_cobranca == "15" ~ "Discharged: Return for follow-up",
      tmp_cobranca == "16" ~ "Discharged: Due to evasion",
      tmp_cobranca == "18" ~ "Discharged: Other reasons",
      tmp_cobranca == "19" ~ "Discharged: Acute patient in psychiatry",
      tmp_cobranca == "21" ~ "Extended stay: Disease-specific characteristics",
      tmp_cobranca == "22" ~ "Extended stay: Complications",
      tmp_cobranca == "23" ~ "Extended stay: Socio-familial reasons",
      tmp_cobranca == "24" ~ "Extended stay: Living donor",
      tmp_cobranca == "25" ~ "Extended stay: Deceased donor",
      tmp_cobranca == "26" ~ "Extended stay: Procedure change",
      tmp_cobranca == "27" ~ "Extended stay: Reoperation",
      tmp_cobranca == "28" ~ "Extended stay: Other reasons",
      tmp_cobranca == "29" ~ "Transferred: Home hospitalization",
      tmp_cobranca == "31" ~ "Transferred: Another facility",
      tmp_cobranca == "32" ~ "Transferred: Home hospitalization",
      tmp_cobranca == "41" ~ "Death: Certificate by attending physician",
      tmp_cobranca == "42" ~ "Death: Certificate by Medical Examiner",
      tmp_cobranca == "43" ~ "Death: Certificate by SVO",
      tmp_cobranca == "51" ~ "Administrative closure",
      tmp_cobranca == "61" ~ "Discharged: Mother/puerpera & newborn",
      tmp_cobranca == "17" ~ "Discharged: Mother/puerpera & newborn",
      tmp_cobranca == "62" ~ "Discharged: Mother/puerpera, newborn stays",
      tmp_cobranca == "13" ~ "Discharged: Mother/puerpera, newborn stays",
      tmp_cobranca == "63" ~ "Discharged: Mother/puerpera, newborn death",
      tmp_cobranca == "64" ~ "Discharged: Mother/puerpera w/ fetal death",
      tmp_cobranca == "65" ~ "Death: Pregnant woman and conceptus",
      tmp_cobranca == "66" ~ "Death: Mother/puerpera, newborn discharged",
      tmp_cobranca == "67" ~ "Death: Mother/puerpera, newborn stays",
      TRUE ~ tmp_cobranca
    )) %>%
    dplyr::mutate(outcome = as.factor(.data$outcome)) %>%
    dplyr::select(-tmp_cobranca, -dplyr::all_of(col_name))
  data
}

# 10) Process age columns: COD_IDADE + IDADE => Age_Code, AGE_UNSPECIFIED, AGE_YEARS
process_age <- function(data) {
  if (!("COD_IDADE" %in% names(data))) return(data)

  data <- data %>%
    dplyr::mutate(Age_Code = as.character(.data$COD_IDADE)) %>%
    dplyr::mutate(Age_Code = dplyr::case_when(
      Age_Code == "0" ~ NA_character_,
      Age_Code %in% c("2", "02") ~ "Days",
      Age_Code %in% c("3", "03") ~ "Months",
      Age_Code %in% c("4", "04") ~ "Years",
      Age_Code %in% c("5", "05") ~ "Centenarian (100+)",
      TRUE ~ ""
    )) %>%
    dplyr::mutate(Age_Code = as.factor(.data$Age_Code)) %>%
    dplyr::mutate(IDADE = as.numeric(.data$IDADE)) %>%
    dplyr::mutate(AGE_YEARS = dplyr::case_when(
      Age_Code == "Days"                     ~ 0,
      Age_Code == "Months"                   ~ 0,
      Age_Code == "Years"                    ~ .data$IDADE,
      Age_Code == "Centenarian (100+)"       ~ .data$IDADE + 100,
      TRUE                                   ~ NA_real_
    )) %>%
    dplyr::rename(AGE_UNSPECIFIED = IDADE) %>%
    dplyr::select(-COD_IDADE)
  data
}

# -------------------- MAIN FUNCTION --------------------

#' This function processes key SIH variables, converting them to appropriate formats and labels,
#' if the variables exist in the data.
#' @param data A data.frame containing SIH data.
#' @return A data.frame with the processed data.
#' @examples
#' # Example usage:
#' process_rd(sih_rd_sample)
#' @import dplyr
#' @export
process_rd <- function(data) {
  # -- Load your helper datasets (icd_codes, proc_rea, ibge) from the package
  utils::data("icd_codes", package = "teambrazil")   # has columns: code, Description
  utils::data("proc_rea",  package = "teambrazil")   # has columns: code, PROCEDURE
  utils::data("ibge",      package = "teambrazil")   # has columns: Hospital_CityCod, etc.

  # -- List of variables to keep if they exist in the dataset
  variables_to_keep <- c(
    "ANO_CMPT", "MES_CMPT", "ESPEC", "N_AIH", "IDENT",
    "MUNIC_RES", "NASC", "SEXO", "PROC_REA", "VAL_SH", "VAL_SP", "VAL_TOT",
    "VAL_UTI", "US_TOT", "DT_INTER", "DT_SAIDA", "DIAG_PRINC", "DIAG_SECUN",
    "DIAGSEC1", "COBRANCA", "NATUREZA", "MUNIC_MOV", "COD_IDADE", "IDADE",
    "DIAS_PERM", "MORTE", "CAR_INT", "CNES", "CID_MORTE", "ETNIA",
    "COMPLEX", "FINANC", "RACA_COR"
  )

  # -- Keep only the variables present
  keep_now <- intersect(variables_to_keep, names(data))
  data <- dplyr::select(data, dplyr::all_of(keep_now))

  # ------------------------ Basic Numeric Conversions ------------------------
  data <- convert_numeric(data, "VAL_UTI", "ICU_Service_Value")
  data <- convert_numeric(data, "VAL_SH",  "Hospital_Service_Value")
  data <- convert_numeric(data, "VAL_SP",  "Professional_Service_Value")
  data <- convert_numeric(data, "VAL_TOT", "Total_Cost_BRL")
  data <- convert_numeric(data, "US_TOT",  "Total_Cost_USD")

  # ------------------------ Labels for factor-like columns -------------------
  data <- label_financ(data, "FINANC")
  data <- label_complex(data, "COMPLEX")
  data <- label_car_int(data, "CAR_INT")
  data <- label_marca_uti(data, "MARCA_UTI")
  data <- label_raca_cor(data, "RACA_COR")
  data <- label_morte(data, "MORTE")
  data <- label_ident(data, "IDENT")
  data <- label_cobranca(data, "COBRANCA")

  # ------------------------ Year_Competency, Month_Competency ---------------
  if ("ANO_CMPT" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(Year_Competency = as.numeric(.data$ANO_CMPT)) %>%
      dplyr::select(-ANO_CMPT)
  }
  if ("MES_CMPT" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(Month_Competency = as.numeric(.data$MES_CMPT)) %>%
      dplyr::select(-MES_CMPT)
  }

  # ------------------------ ICD_MORTE -> ICD_Death --------------------------
  if ("CID_MORTE" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(ICD_Death = as.character(.data$CID_MORTE)) %>%
      dplyr::mutate(ICD_Death = as.factor(.data$ICD_Death)) %>%
      dplyr::select(-CID_MORTE)
  }

  # ------------------------ N_AIH -> ID -------------------------------------
  if ("N_AIH" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(ID = as.character(.data$N_AIH)) %>%
      dplyr::select(-N_AIH)
  }

  # ------------------------ Age fields --------------------------------------
  data <- process_age(data)

  # ------------------------ Date fields -------------------------------------
  if ("NASC" %in% names(data)) {
    data$Birthday <- as.Date(data$NASC, format = "%Y%m%d")
    data <- dplyr::select(data, -NASC)
  }
  if ("DT_INTER" %in% names(data)) {
    data$DT_HOSP <- as.Date(data$DT_INTER, format = "%Y%m%d")
    data <- dplyr::select(data, -DT_INTER)
  }
  if ("DT_SAIDA" %in% names(data)) {
    data$DT_DISCHARGE <- as.Date(data$DT_SAIDA, format = "%Y%m%d")
    data <- dplyr::select(data, -DT_SAIDA)
  }

  # ------------------------ Length of stay ----------------------------------
  if ("DIAS_PERM" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(`LOS(days)` = as.numeric(.data$DIAS_PERM)) %>%
      dplyr::select(-DIAS_PERM)
  }

  # ------------------------ DIAG_PRINC -> link with icd_codes ---------------
  if ("DIAG_PRINC" %in% names(data)) {
    data$DIAG_PRINC <- as.character(data$DIAG_PRINC)
    data <- data %>%
      dplyr::left_join(icd_codes, by = c("DIAG_PRINC" = "code")) %>%
      dplyr::rename(
        ICD_10_MD = DIAG_PRINC,
        Main_Diagnosis = Description
      )
  }

  # ------------------------ PROC_REA -> link with proc_rea ------------------
  if ("PROC_REA" %in% names(data)) {
    data$PROC_REA <- as.character(data$PROC_REA)
    data <- data %>%
      dplyr::left_join(proc_rea, by = c("PROC_REA" = "code")) %>%
      dplyr::rename(
        Procedure_Code = PROC_REA,
        Main_Procedure = PROCEDURE
      )
  }

  # ------------------------ SEXO -> sex -------------------------------------
  if ("SEXO" %in% names(data)) {
    data$SEXO <- dplyr::case_when(
      data$SEXO == "1" ~ "Male",
      data$SEXO %in% c("2", "3") ~ "Female",
      data$SEXO %in% c("0", "9") ~ NA_character_,
      TRUE ~ NA_character_
    )
    data$sex <- as.factor(data$SEXO)
    data <- dplyr::select(data, -SEXO)
  }

  # ------------------------ Handling secondary diagnosis ---------------------
  if ("Year_Competency" %in% names(data)) {
    data$Year_Competency <- suppressWarnings(as.numeric(data$Year_Competency))
    data$SecondaryDiagnosis <- NA_character_  # default
    has_diag_secun <- "DIAG_SECUN" %in% names(data)
    has_diagsec1   <- "DIAGSEC1"  %in% names(data)

    if (has_diag_secun && has_diagsec1) {
      data$SecondaryDiagnosis <- ifelse(
        data$Year_Competency <= 2014,
        as.character(data$DIAG_SECUN),
        as.character(data$DIAGSEC1)
      )
      data <- dplyr::select(data, -DIAG_SECUN, -DIAGSEC1)
    } else if (has_diag_secun) {
      data$SecondaryDiagnosis <- ifelse(
        data$Year_Competency <= 2014,
        as.character(data$DIAG_SECUN),
        NA_character_
      )
      data <- dplyr::select(data, -DIAG_SECUN)
    } else if (has_diagsec1) {
      data$SecondaryDiagnosis <- ifelse(
        data$Year_Competency > 2014,
        as.character(data$DIAGSEC1),
        NA_character_
      )
      data <- dplyr::select(data, -DIAGSEC1)
    }
  }

  # Remove leftover DIAG_SECUN or DIAGSEC1 if any remain
  if ("DIAG_SECUN" %in% names(data))  data <- dplyr::select(data, -DIAG_SECUN)
  if ("DIAGSEC1"  %in% names(data))   data <- dplyr::select(data, -DIAGSEC1)

  # ------------------------ NATUREZA -> Institution_Type ---------------------
  if ("NATUREZA" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(Institution_Type = as.character(.data$NATUREZA)) %>%
      dplyr::mutate(Institution_Type = dplyr::case_when(
        Institution_Type %in% c("0", "99") ~ NA_character_,
        Institution_Type == "10" ~ "Own",
        Institution_Type == "20" ~ "Contracted",
        Institution_Type == "22" ~ "Contracted opting for SIMPLES",
        Institution_Type == "30" ~ "Federal",
        Institution_Type == "31" ~ "Federal with Own Funds",
        Institution_Type == "40" ~ "State",
        Institution_Type == "41" ~ "State with Own Funds",
        Institution_Type == "50" ~ "Municipal",
        Institution_Type == "60" ~ "Philanthropic",
        Institution_Type == "61" ~ "Philanthropic exempt",
        Institution_Type == "63" ~ "Philanthropic exempt (income tax)",
        Institution_Type == "70" ~ "University for Teaching",
        Institution_Type == "80" ~ "Union",
        Institution_Type == "90" ~ "University for Research",
        Institution_Type == "91" ~ "Research exempt from taxes/contributions",
        Institution_Type == "93" ~ "Research exempt from income tax",
        Institution_Type %in% c("92", "94") ~ "Private university for teaching/research",
        TRUE ~ Institution_Type
      )) %>%
      dplyr::mutate(Institution_Type = as.factor(.data$Institution_Type)) %>%
      dplyr::select(-NATUREZA)
  }

  # ------------------------ Municipalities merges ----------------------------
  #  (Hospital_CityCod)
  if ("MUNIC_MOV" %in% names(data)) {
    data <- data %>% dplyr::rename(Hospital_CityCod = MUNIC_MOV)
    data$Hospital_CityCod <- as.numeric(data$Hospital_CityCod)
    ibge$Hospital_CityCod <- as.numeric(ibge$Hospital_CityCod)

    # If code starts with '53', replace with 530010
    data <- data %>%
      dplyr::mutate(Hospital_CityCod = dplyr::if_else(
        substr(.data$Hospital_CityCod, 1, 2) == "53",
        530010,
        .data$Hospital_CityCod
      ))

    data <- data %>% dplyr::left_join(ibge, by = "Hospital_CityCod")
  }
  # ------------------------ SURGICAL MANAGEMENT ------------------------
  # (Place this section after the code block where Procedure_Code is created)
  if ("Procedure_Code" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(Surgical_Management = NA_character_) %>%
      dplyr::mutate(Surgical_Management = dplyr::case_when(
        stringr::str_starts(.data$Procedure_Code, "04")   ~ "Yes",
        stringr::str_starts(.data$Procedure_Code, "0505") ~ "Yes",
        TRUE                                             ~ "No"
      ))
  }

  #  (Patient_CityCod)
  if ("MUNIC_RES" %in% names(data)) {
    data <- data %>% dplyr::rename(Patient_CityCod = MUNIC_RES)
    data$Patient_CityCod <- as.numeric(data$Patient_CityCod)

    # rename ibge columns from Hospital* to Patient*
    ibge2 <- ibge %>%
      dplyr::rename_with(~ gsub("^Hospital", "Patient", .), dplyr::starts_with("Hospital"))

    ibge2$Patient_CityCod <- as.numeric(ibge2$Patient_CityCod)

    # If code starts with '53', replace with 530010
    data <- data %>%
      dplyr::mutate(Patient_CityCod = dplyr::if_else(
        substr(.data$Patient_CityCod, 1, 2) == "53",
        530010,
        .data$Patient_CityCod
      ))

    data <- data %>% dplyr::left_join(ibge2, by = "Patient_CityCod")
  }

  # ------------------------ Final Column Order -------------------------------
  col_order <- c(
    "Year_Competency",     # 1
    "Month_Competency",    # 2
    "ID",                  # 3
    "DT_HOSP",             # 4
    "DT_DISCHARGE",        # 5
    "LOS(days)",           # 6
    "Main_Procedure",
    "Procedure_Code",
    "Surgical_Management",
    "Main_Diagnosis",      # 8
    "ICD_10_MD",           # 9
    "SecondaryDiagnosis",  # 10
    "sex",                 # 11
    "AGE_YEARS",           # 12
    "AGE_UNSPECIFIED",     # 13
    "Age_Code",            # 14
    "race",                # 15
    "Total_Cost_USD",      # 16
    "Death",               # 17
    "ICD_Death",           # 18
    "Birthday",            # 19
    "outcome"              # 20
  )
  # Select them in the desired order, then everything else
  col_order <- intersect(col_order, names(data))
  data <- dplyr::select(data, dplyr::all_of(col_order), dplyr::everything())

  # -- Done
  return(droplevels(data))
}
