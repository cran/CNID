#' @title Calculate the number of days in a specified year and month
#'
#' @description
#' Calculate the number of days in a specified year and month.
#'
#' @param month A given month, such as 2.
#' 
#' @param year A given year, the default is current year.
#'
#' @return Days in a specified year and month.
#'
#' @examples
#' mdays(2, 2022)
#'
#' @export

#------------------------------------------------------------------------------#

mdays = function(
    month, year = as.integer(format(Sys.Date(), "%Y"))
) {
  
  if (is.na(as.numeric(month))) {
    stop("month must be numeric value")
  }
  
  if (is.na(as.numeric(year))) {
    stop("year must be numeric value")
  }
  
  month = as.integer(month)
  year = as.integer(year)
  
  if (month < 1 || month > 12) {
    stop("The month must be between 1 and 12")
  }
  
  days_in_month = integer(12)
  days_in_month[c(1, 3, 5, 7, 8, 10, 12)] = 31
  days_in_month[c(4, 6, 9, 11)] = 30
  
  if (
    (year %% 4 == 0 & year %% 100 != 0) |
    (year %% 400 == 0)
  ) {
    days_in_month[2] = 29
  } else {
    days_in_month[2] = 28
  }
  
  return(days_in_month[month])
  
}

#------------------------------------------------------------------------------#

