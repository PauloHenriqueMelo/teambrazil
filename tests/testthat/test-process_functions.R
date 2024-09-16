

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


