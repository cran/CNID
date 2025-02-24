#' @title Calculate whether a given year is a leap year
#'
#' @description
#' Calculate whether a given year is a leap year.
#'
#' @param year A given year, vectorization parameter passing is supported.
#'
#' @return TRUE or FALSE. Return TRUE if it is a leap year, FALSE otherwise.
#'
#' @examples
#' leap_year(2024)
#'
#' @export

#------------------------------------------------------------------------------#

leap_year = function(year) {
  (year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0)
}

#------------------------------------------------------------------------------#

