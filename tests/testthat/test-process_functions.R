test_that("cnes-st works at 2016, 6-7", {
  cnes_st <- process_cnes(data = cnes_st_sample, information_system = "CNES-ST", nomes = TRUE, municipality_data = TRUE)
  expect_true("tbl_df" %in% class(cnes_st))
})

test_that("cnes-sf works at 2016, 6-7", {
  cnes_pf <- process_cnes(data = cnes_pf_sample, information_system = "CNES-PF", nomes = TRUE, municipality_data = TRUE)
  expect_true("tbl_df" %in% class(cnes_pf))
})

test_that("sia works at 2016, 6-7", {
  sia <- process_sia(data = sia_pa_sample, nome_proced = FALSE, nome_ocupacao = TRUE, nome_equipe = TRUE, information_system = "SIA-PA", municipality_data = TRUE)
  expect_true("tbl_df" %in% class(sia))
})

test_that("sih rd works at 2016, 6-7", {
  sih <- process_sih(data = sih_rd_sample, information_system = "SIH-RD", municipality_data = TRUE)
  expect_true("tbl_df" %in% class(sih))
})


test_that("sih_works at 2016, 6-7", {
  sih <- process_sih_rd(data = sih_rd_sample)
  print(class(sih))
  expect_true("data.frame" %in% class(sih))
})


test_that("sih contains Hospital_City at 2016, 6-7", {
  # Process the SIH data
  sih <- process_sih_rd(data = sih_rd_sample)

  # Print the class of the returned object
  print(class(sih))

  # Ensure that the result is a data frame
  expect_true("data.frame" %in% class(sih))

  # Check if the 'Hospital_City' variable exists in the returned data frame
  expect_true("Hospital_City" %in% colnames(sih), info = "'Hospital_City' variable is missing from the processed SIH data")
})



test_that("sih contains Hospital_City at 2016, 6-7", {
  # Process the SIH data
  sih <- process_sih_rd(data = sih_rd_sample)

  # Print the class of the returned object
  print(class(sih))

  # Ensure that the result is a data frame
  expect_true("data.frame" %in% class(sih))

  # Check if the 'Hospital_City' variable exists in the returned data frame
  expect_true("outcome" %in% colnames(sih), info = "'outcome' variable is missing from the processed SIH data")
})












test_that("sim do works", {
  sim <- process_sim(data = sim_do_sample, municipality_data = TRUE)
  expect_true("tbl_df" %in% class(sim))
})

test_that("sinasc works", {
  sinasc <- process_sinasc(data = sinasc_sample, municipality_data = TRUE)
  expect_true("tbl_df" %in% class(sinasc))
})

test_that("sinan-chikungunya at 2022", {
  sinan_chikungunya <- process_sinan_chikungunya(data = sinan_chikungunya_sample, municipality_data = TRUE)
  expect_true("tbl_df" %in% class(sinan_chikungunya))
})

test_that("sinan-dengue at 2016", {
  sinan_dengue <- process_sinan_dengue(data = sinan_dengue_sample, municipality_data = TRUE)
  expect_true("tbl_df" %in% class(sinan_dengue))
})

test_that("sinan-malaria at 2016", {
  sinan_malaria <- process_sinan_malaria(data = sinan_malaria_sample, municipality_data = TRUE)
  expect_true("tbl_df" %in% class(sinan_malaria))
})

test_that("sinan-zika at 2016", {
  sinan_zika <- process_sinan_zika(data = sinan_zika_sample, municipality_data = TRUE)
  expect_true("tbl_df" %in% class(sinan_zika))
})



test_that("process_rd returns a data frame", {
  processed_data <- process_rd(sih_rd_sample)
  expect_s3_class(processed_data, "data.frame")
})

test_that("DIAG_PRINC is translated correctly", {
  processed_data <- process_rd(sih_rd_sample)
  expect_true("Main_Diagnosis" %in% names(processed_data))
  expect_false("DIAG_PRINC" %in% names(processed_data))
  # Check that Main_Diagnosis is not missing
  expect_false(any(is.na(processed_data$Main_Diagnosis)))
})

test_that("SEXO variable is processed correctly", {
  processed_data <- process_rd(sih_rd_sample)
  expect_true("sex" %in% names(processed_data))
  expect_false("SEXO" %in% names(processed_data))
  expect_true(all(levels(processed_data$sex) %in% c("Male", "Female")))
})

test_that("RACA_COR variable is processed correctly", {
  processed_data <- process_rd(sih_rd_sample)
  expect_true("race" %in% names(processed_data))
  expect_false("RACA_COR" %in% names(processed_data))
  expect_true(all(levels(processed_data$race) %in% c("White", "Black", "Brown", "Yellow", "Indigenous")))
})

test_that("Date variables are processed correctly", {
  processed_data <- process_rd(sih_rd_sample)
  expect_true("DOB" %in% names(processed_data))
  expect_true("DT_HOSP" %in% names(processed_data))
  expect_true("DT_DISCHARGE" %in% names(processed_data))
  expect_type(processed_data$DOB, "double")  # Dates are stored as numeric internally
  expect_type(processed_data$DT_HOSP, "double")
  expect_type(processed_data$DT_DISCHARGE, "double")
})

test_that("Monetary variables are numeric", {
  processed_data <- process_rd(sih_rd_sample)
  monetary_vars <- c("VAL_SH", "VAL_SP", "VAL_TOT", "VAL_UTI", "VAL_UCI")
  existing_vars <- intersect(monetary_vars, names(processed_data))
  for (var in existing_vars) {
    expect_type(processed_data[[var]], "double")
  }
})

test_that("Outcome variable is processed correctly", {
  processed_data <- process_rd(sih_rd_sample)
  expect_true("outcome" %in% names(processed_data))
  expect_false("COBRANCA" %in% names(processed_data))
  expect_s3_class(processed_data$outcome, "factor")
})

test_that("City codes are joined correctly", {
  processed_data <- process_rd(sih_rd_sample)
  # Check if Hospital city variables are present
  expect_true("Hospital_CityCod" %in% names(processed_data))
  expect_true("Hospital_City" %in% names(processed_data))
  # Check if Patient city variables are present
  expect_true("Patient_CityCod" %in% names(processed_data))
  expect_true("Patient_City" %in% names(processed_data))
})

test_that("LOS and Total_Cost_USD are numeric", {
  processed_data <- process_rd(sih_rd_sample)
  expect_true("LOS" %in% names(processed_data))
  expect_true("Total_Cost_USD" %in% names(processed_data))
  expect_type(processed_data$LOS, "double")
  expect_type(processed_data$Total_Cost_USD, "double")
})


