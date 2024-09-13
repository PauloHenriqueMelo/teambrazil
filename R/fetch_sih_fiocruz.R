#' Fetch SIH Data from DataSUS by State (Fiocruz)
#'
#' This function fetches and merges SIH (Hospital Information System) data from DataSUS for a specified list of states or for all states by default.
#' It downloads `.dbc` files from DataSUS's FTP server, processes them one by one, and returns a combined dataset.
#'
#' @param year Numeric value indicating the year. Must be 1996 or later.
#' @param month Numeric value indicating the month. Must be between 1 and 12.
#' @param uf Character vector of state codes representing the federative units. Default is `"all"` (fetches data for all states).
#' @param timeout Numeric value indicating the maximum number of seconds to wait for each download. Default is 240 seconds.
#' @param stop_on_error Logical value. If `TRUE`, stops the function if any download fails. Default is `FALSE`.
#' @param track_source Logical value. If `TRUE`, adds a `source` column to the data, tracking the original file for each row. Default is `FALSE`.
#'
#' @return A data frame with the downloaded and merged data from DataSUS, or `NULL` if the FTP server is down or there is no internet connection.
#' @export
#'
#' @examples
#' \dontrun{
#' # Fetch SIH data for January 2021 for all states
#' data <- fetch_sih_fiocruz(year = 2021, month = 1)
#'
#' # Fetch SIH data for January 2021 for a specific list of states
#' data <- fetch_sih_fiocruz(year = 2021, month = 1, uf = c("SP", "MG", "RJ"))
#' }

fetch_sih_fiocruz <- function(year, month, uf = "all", timeout = 240, stop_on_error = FALSE, track_source = FALSE) {

  # Resets original timeout option on function exit
  original_time_option <- getOption("timeout")
  on.exit(options(timeout = original_time_option))

  # Set new timeout
  options(timeout = timeout)

  # Assert arguments
  checkmate::assert_numeric(x = year, lower = 1996, null.ok = FALSE)
  checkmate::assert_numeric(x = month, lower = 1, upper = 12, null.ok = FALSE)

  # Create the date string
  date <- paste0(substr(year, 3, 4), formatC(month, width = 2, format = "d", flag = "0"))

  # State codes for all Brazilian states
  all_state_codes <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
                       "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
                       "RS", "RO", "RR", "SC", "SP", "SE", "TO")

  # Handle the `uf` argument: if "all", select all state codes
  if (identical(uf, "all")) {
    selected_states <- all_state_codes
  } else {
    # Validate and check if the states provided are valid
    checkmate::assert_subset(uf, all_state_codes, empty.ok = FALSE, add = NULL)
    selected_states <- uf
  }

  # Check local Internet connection
  if (!curl::has_internet()) {
    cli::cli_alert_warning("It appears that your local Internet connection is not working. Can you check?")
    return(NULL)
  }

  # Check DataSUS FTP server
  if (!RCurl::url.exists("ftp.datasus.gov.br", .opts = list(timeout = timeout))) {
    cli::cli_alert_warning("It appears that DataSUS FTP is down or not reachable.")
    return(NULL)
  }

  cli::cli_alert_info("Your local Internet connection seems to be ok.")
  cli::cli_alert_info("DataSUS FTP server seems to be up and reachable.")
  cli::cli_alert_info("Starting download...")

  # Prepare the file URLs for each selected state
  base_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/"
  files_list <- paste0(base_url, "RD", selected_states, date, ".dbc")

  # Download and merge files
  data <- NULL
  for (file in files_list) {
    temp <- tempfile()
    partial <- data.frame()

    tryCatch({
      utils::download.file(file, temp, mode = "wb", method = "libcurl")
      partial <- read.dbc::read.dbc(temp, as.is = TRUE)
      file.remove(temp)

      if (nrow(partial) > 0) {
        # Add a source column if track_source is TRUE
        if (track_source == TRUE) {
          partial$source <- basename(file)
        }
        # Merge the data
        data <- dplyr::bind_rows(data, partial)
      }
    }, error = function(cond) {
      cli::cli_alert_info(paste("Something went wrong with this URL:", file))
      cli::cli_alert("This can be a problem with the Internet or the file does not exist yet.")
      cli::cli_alert("If the file is too big, try increasing the timeout argument value.")

      if (stop_on_error == TRUE) {
        cli::cli_abort("Stopping download.")
      }
    })
  }

  # Return the combined dataset
  return(data)
}


