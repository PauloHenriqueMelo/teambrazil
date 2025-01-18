# teambrazil

**teambrazil** is an R package designed to download and preprocess microdata from Brazil’s Hospital Information System (SIH), specifically the RD (internment) files from [DataSUS](http://www.datasus.gov.br). It is similar in purpose to [microdatasus](https://github.com/rfsaldanha/microdatasus) but is **entirely in English**, offers a simplified workflow for specifying monthly data pulls, and provides slightly different variable labeling conventions.

With **teambrazil**, you can:
- Download raw `.dbc` files from DataSUS’s FTP server for one or more Brazilian states and a specific **year-month**.
- Seamlessly merge data from multiple states into a single `data.frame`.
- Convert or label key SIH variables (e.g., diagnoses, procedures, race, sex) into human-readable categories.
- Manage data in a more “tidyverse”-friendly manner (utilizing `dplyr`).

---

## Table of Contents

1. [Installation](#installation)  
2. [Differences from `microdatasus`](#differences-from-microdatasus)  
3. [Usage Overview](#usage-overview)  
   1. [Fetching Data with `fetch_rd()`](#fetching-data-with-fetch_rd)  
      - [Function Arguments](#function-arguments)  
      - [Download Examples](#download-examples)  
   2. [Processing Data with `process_rd()`](#processing-data-with-process_rd)  
      - [Processing Examples](#processing-examples)  
4. [List of Brazilian State Codes](#list-of-brazilian-state-codes)  
5. [Troubleshooting](#troubleshooting)  
6. [Acknowledgments](#acknowledgments)  

---

## Installation

### 1. Install `devtools` (if necessary)

```r
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
```

### 2. Install **teambrazil** from GitHub

```r
devtools::install_github("PauloHenriqueMelo/teambrazil")
```

### 3. Load the Package

```r
library(teambrazil)
```

### 4. Check the Installed Version (Optional)

```r
packageVersion("teambrazil")
```

---

## Differences from `microdatasus`

If you’ve used [microdatasus](https://github.com/rfsaldanha/microdatasus) before, here are the main differences:

1. **Specifying Date Ranges**  
   - **microdatasus**: You provide a starting year/month and an ending year/month (e.g., `year_start = 2013, year_end = 2014`) and the function loops across each intermediate time period.  
   - **teambrazil**: You specify **a single year and month** at a time in `fetch_rd()`. If you need multiple months, call `fetch_rd()` in a loop or for each relevant month.  

2. **Package Language**  
   - **microdatasus** is in Portuguese and uses function names like `process_sih()`.  
   - **teambrazil** is written and documented in English. For example, `process_rd()` produces output with English column labels.

3. **Preprocessing**  
   - Both packages label and transform raw SIH data. **teambrazil** uses English-friendly schemes, such as renaming `RACA_COR` to `race`, `SEXO` to `sex`, etc.  
   - In **microdatasus**, functions like `process_sih()` handle preprocessing.  
   - In **teambrazil**, use `process_rd()` for the RD dataset specifically.

4. **Focus on RD Files**  
   - **microdatasus** supports multiple information systems (SIM, SINASC, SIH, CNES, etc.).  
   - **teambrazil** is specialized for **SIH-RD** data.

---

## Usage Overview

This package provides **two main functions**:

1. **`fetch_rd()`**: Downloads and merges `.dbc` files from DataSUS for a given year, month, and set of states.  
2. **`process_rd()`**: Converts raw data into a tidy format with English-labeled columns, numeric conversions, factor labeling, etc.

### Workflow Diagram

```
fetch_rd() ---> returns raw data frame ---> process_rd() ---> returns processed data frame
```

---

### Fetching Data with `fetch_rd()`

```r
fetch_rd(
  year,
  month,
  uf = "all",
  timeout = 500,
  stop_on_error = FALSE,
  track_source = FALSE
)
```

#### Function Arguments

- **year**: Numeric value (e.g., `2020`). Must be >= 1996.
- **month**: Numeric value from `1` to `12` (January to December).
- **uf**: Federative unit(s) for data:
  - `"all"` (default): Fetches data for all Brazilian states.
  - A character vector of state codes, e.g., `c("SP", "RJ")`.
- **timeout**: Maximum time (seconds) to wait for downloads. Default: `500`.
- **stop_on_error**: If `TRUE`, stops on the first error. Default: `FALSE`.
- **track_source**: If `TRUE`, adds a `source` column with file names.

#### Download Examples

**Example 1: All States**

```r
my_data_raw <- fetch_rd(
  year  = 2020,
  month = 1,
  uf    = "all"
)
```

**Example 2: A Single State**

```r
my_data_raw <- fetch_rd(
  year  = 2019,
  month = 3,
  uf    = "RJ"
)
```

**Example 3: Multiple States**

```r
my_data_raw <- fetch_rd(
  year  = 2021,
  month = 7,
  uf    = c("SP", "MG")
)
```

---

### Processing Data with `process_rd()`

After downloading raw data with `fetch_rd()`, use `process_rd()` to clean and label it:

```r
my_data_processed <- process_rd(my_data_raw)
```

#### Key Features of `process_rd()`

- Converts numeric columns (e.g., `VAL_SH`) to proper formats.
- Renames columns like `RACA_COR` to `race` and `SEXO` to `sex`.
- Labels factors in English.
- Parses dates into `Date` objects (e.g., `DT_INTER` -> `DT_HOSP`).
- Joins reference data for ICD/procedure codes.

#### Processing Examples

```r
my_data_processed <- process_rd(my_data_raw)

names(my_data_processed)
# [1] "Year_Competency" "Month_Competency" "ID"
# [4] "DT_HOSP" "DT_DISCHARGE" "LOS(days)"
# [7] "Main_Procedure" "Main_Diagnosis" "race"
```

---

## List of Brazilian State Codes

The **27** state codes for Brazil:

```
AC, AL, AP, AM, BA, CE, DF, ES, GO, MA,
MT, MS, MG, PA, PB, PR, PE, PI, RJ, RN,
RS, RO, RR, SC, SP, SE, TO
```

---

## Troubleshooting

1. **Download Failures**: Ensure your internet connection is stable and DataSUS’s FTP server is up.
2. **Timeout Issues**: Increase the `timeout` value if needed.
3. **Encoding Errors**: Verify your R session’s locale settings.

---

## Acknowledgments

1. [DataSUS](http://www.datasus.gov.br): Source of the raw SIH data.
2. [microdatasus](https://github.com/rfsaldanha/microdatasus): Inspired many features.
3. [read.dbc](https://cran.r-project.org/package=read.dbc): Essential for parsing `.dbc` files.

---

*Last updated: [Date]*

