#' @title Calculate the number of days in a given year and month
#'
#' @description
#' Calculate the number of days in a given year and month.
#'
#' @param year A given year.
#' 
#' @param month A given month, vectorization parameter passing is supported.
#'
#' @return Days in a given year and month.
#'
#' @examples
#' mdays(2025, 1:12)
#'
#' @export

#------------------------------------------------------------------------------#

mdays = function(year, month) {
  days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[month]
  days[month == 2 & leap_year(year)] = 29  # February in leap years has 29 days
  days
}

#------------------------------------------------------------------------------#

