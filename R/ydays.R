#' @title Calculate the number of days in a given year
#'
#' @description
#' Calculate the number of days in a given year.
#'
#' @param year A given year, vectorization parameter passing is supported.
#'
#' @return Days in a given year.
#'
#' @examples
#' ydays(2025)
#'
#' @export

#------------------------------------------------------------------------------#

ydays = function(year) {
  365 + leap_year(year)
}

#------------------------------------------------------------------------------#

