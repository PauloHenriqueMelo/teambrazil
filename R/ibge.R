#' IBGE Dataset: City information for hospitals in Brazil
#'
#' This dataset contains city-level information for hospitals in Brazil,
#' including city codes, names, geographic data, and now includes a `Hospital_Region`
#' variable that assigns each hospital to one of the five geographic regions in Brazil.
#'
#' @format A data frame with 5631 observations on the following 8 variables:
#' \describe{
#'   \item{Hospital_CityCod}{City code}
#'   \item{Hospital_City}{City name}
#'   \item{Hospital_State}{State name}
#'   \item{Hospital_CityLat}{Latitude}
#'   \item{Hospital_CityLon}{Longitude}
#'   \item{Hospital_CityAlt}{Altitude}
#'   \item{Hospital_CityArea}{City area in km²}
#'   \item{Hospital_Region}{Geographic region (North, Northeast, Central-West, Southeast, South)}
#' }
#' @source Brazilian Institute of Geography and Statistics (IBGE)

"ibge"
