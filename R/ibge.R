#' ibge: Dataset of Hospitals by City
#'
#' This dataset contains information about hospitals and their associated cities, including geographic coordinates and other attributes.
#' It can be used for various geographical and healthcare-related analyses.
#'
#' @format A data frame with 5659 observations of 7 variables:
#' \describe{
#'   \item{Hospital_CityCod}{Integer. The unique identifier for each hospital city.}
#'   \item{Hospital_City}{Character. Name of the city where the hospital is located.}
#'   \item{Hospital_State}{Character. Name of the state where the hospital is located.}
#'   \item{Hospital_CityLat}{Numeric. Latitude of the hospital city.}
#'   \item{Hospital_CityLon}{Numeric. Longitude of the hospital city.}
#'   \item{Hospital_CityAlt}{Integer. Altitude of the hospital city.}
#'   \item{Hospital_CityArea}{Numeric. Area of the hospital city in square kilometers.}
#' }
#'
#' @source \url{https://example-source-url.com}  # Replace with the real source if available
#' @examples
#' data(hospitalcity)
#' summary(hospitalcity)
#'
"ibge"
